{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The agent loop.
--
-- This is the heart of Haskode: a loop that alternates between
-- getting user input, sending it to the LLM, processing tool calls,
-- and displaying results.
--
-- The loop looks like this:
--
-- @
--   user message
--     -> build context (system prompt + conversation)
--     -> call provider
--     -> if tool calls:
--         -> check policy for each tool call
--         -> execute allowed tools
--         -> append tool results to conversation
--         -> call provider again (with tool results)
--       else:
--         -> display assistant reply
--         -> wait for next user message
-- @

module Haskode.Agent
  ( -- * Agent loop
    AgentState (..)
  , runAgent
  , initState
  , initStateWithDisplay
    -- * Single-turn processing
  , processTurn
    -- * System prompt
  , buildSystemPrompt
  , loadSystemMd
  , loadAgentsMd
    -- * Context estimation
  , estimateContextChars
    -- * Context statistics
  , ContextStats (..)
  , contextStats
    -- * Session lifecycle helpers
  , recordSessionStart
  , recordSessionStartWithResume
  , recordSessionEnd
  , recordConversationReset
    -- * Approval
  , ApprovalFunc
  , autoApprove
  , autoReject
  , terminalApproval
    -- * Display
  , DisplayFunc
  , terminalDisplay
  , AgentDisplay (..)
  , terminalAgentDisplay
  ) where

import Control.Exception  (IOException, try, SomeException, throwIO)
import Control.Monad      (when, unless)
import Data.Aeson         (Value, encode)
import qualified Data.ByteString.Lazy as LBS
import Data.IORef
import Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO       as TIO
import System.Directory   (doesFileExist, getFileSize)
import System.IO          (hFlush, stdout)

import Haskode.Config     (Config (..), ProviderConfig (..))
import Haskode.Patch      (colorizeUnifiedDiff)
import Haskode.Display    (DisplayEvent (..), renderDisplayEvent,
                           formatConfirmTool, formatConfirmArgs,
                           formatConfirmReason, formatConfirmPrompt,
                           formatConfirmFile, formatConfirmDiffHeader,
                           formatConfirmPreviewHeader, formatContextLimitRefusal,
                           indentBlock,
                           streamBegin, streamChunk, streamEnd)
import Haskode.Core       (Conversation, Message (..), Role (..), ToolCall (..),
                           ToolResult (..), emptyConversation,
                           mkUserMessage, mkSystemMessage, appendMessage)
import Haskode.Policy     (Policy, Decision (..), checkPolicy)
import Haskode.Provider   (Provider (..), CompletionRequest (..),
                           CompletionResponse (..), StreamHandler (..))
import Haskode.Session    (Event (..), EventType (..), SessionLog,
                           emptyLog, logEvent)
import Haskode.Tools      (Tool (..), ToolRegistry, toolNames, lookupTool,
                           safeCanonicalize, isUnderRoot,
                           extractTextField, computePatchPreview,
                           computeWriteFilePreview,
                           computeBatchApplyPreview)
import Data.Time.Clock    (getCurrentTime)

-- ---------------------------------------------------------------------------
-- Approval
-- ---------------------------------------------------------------------------

-- | A function that decides whether to execute a tool call that the
--   policy flagged with 'AskUser'.
--
--   Parameters:
--
--   * The 'ToolCall' under consideration (name, args, id)
--   * The reason text from the 'AskUser' decision
--
--   Returns 'True' to approve execution, 'False' to reject.
--
--   Inject a pure function here for deterministic tests
--   (e.g. 'autoApprove' or 'autoReject').  The CLI uses
--   'terminalApproval', which reads from stdin.
type ApprovalFunc = ToolCall -> Text -> IO Bool

-- | Always approve.  Useful in tests that need tool calls to go
--   through without touching stdin.
autoApprove :: ApprovalFunc
autoApprove _tc _reason = pure True

-- | Always reject.  Useful in tests that verify rejection behaviour.
autoReject :: ApprovalFunc
autoReject _tc _reason = pure False

-- | Print the tool name, arguments, and reason, then prompt the
--   user with @y\/N@ on the terminal.  Empty input or any input
--   other than @y@ or @Y@ is treated as rejection.
--
--   This is the default approval function used by the CLI.
terminalApproval :: ApprovalFunc
terminalApproval tc reason = do
  TIO.putStrLn $ formatConfirmTool (tcName tc)
  TIO.putStrLn $ formatConfirmArgs (encodeText (tcArgs tc))
  TIO.putStrLn $ formatConfirmReason reason
  putStr   (T.unpack formatConfirmPrompt)
  hFlush stdout
  answer <- TIO.getLine
  pure (T.strip answer `elem` ["y", "Y"])

-- ---------------------------------------------------------------------------
-- Display
-- ---------------------------------------------------------------------------

-- | A function that consumes structured display events.
--
-- The CLI uses 'terminalDisplay', which renders each event as plain
-- terminal text. A future TUI can inject its own sink and update widgets
-- from the same structured events without parsing terminal strings.
type DisplayFunc = DisplayEvent -> IO ()

-- | Render display events to the current CLI terminal.
terminalDisplay :: DisplayFunc
terminalDisplay = TIO.putStrLn . renderDisplayEvent

-- | Display callbacks used by the agent loop.
--
-- Plain display events and streaming assistant chunks are separate because
-- streaming deliberately does not allocate a transcript event per token.
-- Front ends can still consume both paths without parsing terminal text.
data AgentDisplay = AgentDisplay
  { agentDisplayEvent       :: !DisplayFunc
  , agentDisplayStreamBegin :: !(IO ())
  , agentDisplayStreamChunk :: !(Text -> IO ())
  , agentDisplayStreamEnd   :: !(IO ())
  , agentDisplayPreview     :: !(ToolCall -> IO ())
  }

-- | Default display callbacks for the CLI terminal.
terminalAgentDisplay :: AgentDisplay
terminalAgentDisplay = AgentDisplay
  { agentDisplayEvent       = terminalDisplay
  , agentDisplayStreamBegin = streamBegin
  , agentDisplayStreamChunk = streamChunk
  , agentDisplayStreamEnd   = streamEnd
  , agentDisplayPreview     = showConfirmationPreview
  }

-- ---------------------------------------------------------------------------
-- Agent state
-- ---------------------------------------------------------------------------

-- | Everything the agent needs to carry between turns.
data AgentState = AgentState
  { asConversation :: !Conversation
  , asSession      :: !SessionLog
  , asConfig       :: !Config
  , asProvider     :: !Provider
  , asPolicy       :: !Policy
  , asRegistry     :: !ToolRegistry
  , asApproval     :: !ApprovalFunc
  , asDisplay      :: !AgentDisplay
  , asResumed      :: !Bool             -- ^ True when conversation was loaded from a prior session
  }

initState :: Config -> Provider -> Policy -> ToolRegistry -> ApprovalFunc -> Bool -> AgentState
initState cfg prov pol reg approve resumed =
  initStateWithDisplay cfg prov pol reg approve terminalAgentDisplay resumed

-- | Initialize agent state with explicit display callbacks.
--
-- This keeps the default CLI path small while giving other front ends
-- a seam for consuming 'DisplayEvent' values and stream chunks directly.
initStateWithDisplay :: Config -> Provider -> Policy -> ToolRegistry -> ApprovalFunc -> AgentDisplay -> Bool -> AgentState
initStateWithDisplay cfg prov pol reg approve display resumed = AgentState
  { asConversation = emptyConversation
  , asSession      = emptyLog
  , asConfig       = cfg
  , asProvider     = prov
  , asPolicy       = pol
  , asRegistry     = reg
  , asApproval     = approve
  , asDisplay      = display
  , asResumed      = resumed
  }

-- ---------------------------------------------------------------------------
-- Session lifecycle helpers
-- ---------------------------------------------------------------------------

-- | Record a @session_start@ event in the agent state's session log.
--   Call once when an interactive or single-shot run begins.
recordSessionStart :: AgentState -> IO AgentState
recordSessionStart st = do
  now <- getCurrentTime
  pure st { asSession = logEvent (Event now ESessionStart "session started") (asSession st) }

-- | Record a @session_start@ event with resume information.
--   Used when @\-\-resume@ loaded conversation history from a prior session.
recordSessionStartWithResume :: AgentState -> Int -> IO AgentState
recordSessionStartWithResume st msgCount = do
  now <- getCurrentTime
  let info = "session started (resumed: " <> T.pack (show msgCount) <> " messages)"
  pure st { asSession = logEvent (Event now ESessionStart info) (asSession st) }

-- | Record a @session_end@ event in the agent state's session log.
--   Call once when the run exits normally, before flushing.
recordSessionEnd :: AgentState -> IO AgentState
recordSessionEnd st = do
  now <- getCurrentTime
  pure st { asSession = logEvent (Event now ESessionEnd "session ended") (asSession st) }

-- | Record a @conversation_reset@ event in the agent state's session log.
--   Call when @/new@ resets the in-memory conversation.
recordConversationReset :: AgentState -> IO AgentState
recordConversationReset st = do
  now <- getCurrentTime
  pure st { asSession = logEvent (Event now EConversationReset "conversation reset by /new") (asSession st) }

-- ---------------------------------------------------------------------------
-- System prompt
-- ---------------------------------------------------------------------------

-- | Maximum size of AGENTS.md or SYSTEM.md (in bytes) that will be
--   included in the system prompt.  Files larger than this are silently
--   skipped to avoid blowing up the context window.
agentsMdMaxSize :: Integer
agentsMdMaxSize = 32768  -- 32 KB

-- | Load the contents of a project-local markdown file (e.g.
--   @SYSTEM.md@ or @AGENTS.md@) from the working directory.
--
--   Uses 'safeCanonicalize' and 'isUnderRoot' to guard against symlink
--   escape and path-traversal tricks.  Returns 'Nothing' if the file is
--   missing, unreadable, resolves outside the working directory, or
--   exceeds 'agentsMdMaxSize'.
loadProjectMd :: FilePath -> IO (Maybe Text)
loadProjectMd filename = do
  rootResult <- safeCanonicalize "."
  case rootResult of
    Nothing -> pure Nothing
    Just rootCanon -> do
      exists <- doesFileExist filename
      if not exists
        then pure Nothing
        else do
          pathResult <- safeCanonicalize filename
          case pathResult of
            Nothing -> pure Nothing
            Just canon
              | not (isUnderRoot rootCanon canon) -> pure Nothing
              | otherwise -> do
                  sizeResult <- try (getFileSize filename) :: IO (Either IOException Integer)
                  case sizeResult of
                    Left _  -> pure Nothing
                    Right size
                      | size > agentsMdMaxSize -> pure Nothing
                      | otherwise -> do
                          readResult <- try (TIO.readFile filename) :: IO (Either IOException Text)
                          case readResult of
                            Left _  -> pure Nothing
                            Right txt
                              | T.null txt -> pure Nothing
                              | otherwise  -> pure (Just txt)

-- | Load the contents of @SYSTEM.md@ from the working directory, if
--   present and readable.
loadSystemMd :: IO (Maybe Text)
loadSystemMd = loadProjectMd "SYSTEM.md"

-- | Load the contents of @AGENTS.md@ from the working directory, if
--   present and readable.
loadAgentsMd :: IO (Maybe Text)
loadAgentsMd = loadProjectMd "AGENTS.md"

-- | Build a system message that describes all available tools.
--
--   The prompt is plain text (not JSON) so that any LLM provider can
--   understand it.  Each tool is listed with its name, description,
--   and JSON schema.
--
--   When the provider supports native tool calling (e.g. OpenAI's
--   @tools@ array), the system prompt tells the model to use the
--   provided tools — it must NOT print JSON tool-call objects as
--   assistant text.
--
--   If a @SYSTEM.md@ file was loaded, its contents are included as
--   project-local system instructions after the built-in prompt.
--   If an @AGENTS.md@ file was loaded, its contents are appended as
--   repository-specific instructions for the agent.
--
--   Section order:
--
--     1. Built-in Haskode system prompt
--     2. Project @SYSTEM.md@ (if present)
--     3. Project @AGENTS.md@ (if present)
buildSystemPrompt :: ToolRegistry -> Maybe Text -> Maybe Text -> Text
buildSystemPrompt reg systemMd agentsMd =
  "You are a helpful coding assistant. You have access to tools that\n\
  \are provided to you by the system. Use them when needed to answer\n\
  \the user's questions or perform tasks. Do NOT print tool calls as\n\
  \JSON in your reply text -- use the tool-calling mechanism provided\n\
  \by the API.\n\
  \\n\
  \Available tools:\n\n"
  <> T.concat (map describeTool (toolNames reg))
  <> systemMdSection
  <> agentsMdSection
  where
    describeTool name = case lookupTool name reg of
      Nothing -> ""
      Just t  -> "- **" <> toolName t <> "**: " <> toolDescription t
              <> "\n  Schema: " <> T.pack (show (toolSchema t)) <> "\n\n"
    systemMdSection = case systemMd of
      Nothing -> ""
      Just md -> "\n# Project instructions (SYSTEM.md)\n\n" <> md <> "\n"
    agentsMdSection = case agentsMd of
      Nothing -> ""
      Just md -> "\n# Repository instructions (AGENTS.md)\n\n" <> md <> "\n"

-- ---------------------------------------------------------------------------
-- Context estimation
-- ---------------------------------------------------------------------------

-- | Estimate the total character count of a conversation.
--
--   This is a cheap, dependency-free proxy for token count.  It sums:
--
--     * The content of every message ('msgContent')
--     * The JSON-encoded arguments of every 'ToolCall' in assistant messages
--     * A small per-message overhead for role labels and framing
--
--   The estimate is deliberately conservative (slightly over-counting)
--   so that the guard in 'processTurn' fires before the provider
--   receives an oversized request.
estimateContextChars :: Conversation -> Int
estimateContextChars = sum . map messageChars
  where
    -- Per-message overhead: role label, JSON keys, framing.
    -- Kept small but non-zero so the estimate grows with message count.
    messageOverhead :: Int
    messageOverhead = 20

    messageChars :: Message -> Int
    messageChars m = messageOverhead
      + T.length (msgContent m)
      + maybe 0 (sum . map toolCallChars) (msgToolCalls m)

    toolCallChars :: ToolCall -> Int
    toolCallChars tc = T.length (tcId tc) + T.length (tcName tc)
                     + T.length (encodeText (tcArgs tc))

-- ---------------------------------------------------------------------------
-- Context statistics
-- ---------------------------------------------------------------------------

-- | Snapshot of character-based context usage.
--
--   This is an estimate based on character count, not real token
--   counting.  Use 'contextStats' to compute from a conversation.
data ContextStats = ContextStats
  { csCurrent   :: !Int  -- ^ Estimated current conversation characters
  , csMax       :: !Int  -- ^ Configured maximum context characters
  , csRemaining :: !Int  -- ^ Remaining characters (negative when over budget)
  , csPercent   :: !Int  -- ^ Percentage of max used (may exceed 100)
  } deriving (Show, Eq)

-- | Compute context usage statistics from a conversation and limit.
--
--   Uses the same character-count estimate as 'estimateContextChars'.
--   This is an estimate, not a real token count.
--
--   >>> contextStats [mkUserMessage "hello"] 120000
--   ContextStats {csCurrent = 25, csMax = 120000, csRemaining = 119975, csPercent = 0}
contextStats :: Conversation -> Int -> ContextStats
contextStats conv maxChars =
  let est    = estimateContextChars conv
      remain = maxChars - est
      pct    = if maxChars > 0 then est * 100 `div` maxChars else 0
  in ContextStats
       { csCurrent   = est
       , csMax       = maxChars
       , csRemaining = remain
       , csPercent   = pct
       }

-- ---------------------------------------------------------------------------
-- Agent loop
-- ---------------------------------------------------------------------------

-- | Run the agent.  Records the user message, then processes turns
--   until the provider returns a final text reply (no tool calls).
runAgent :: AgentState -> Text -> IO AgentState
runAgent state input = do
  -- Record the user message
  now <- getCurrentTime
  let userMsg = mkUserMessage input
      state1  = state
        { asConversation = appendMessage userMsg (asConversation state)
        , asSession = logEvent (Event now EUserMessage input) (asSession state)
        }

  -- Process turns until we get a final text reply
  state2 <- processTurn state1
  pure state2

-- ---------------------------------------------------------------------------
-- Single-turn processing
-- ---------------------------------------------------------------------------

-- | Process conversation turns until the provider returns a final
--   text reply (no tool calls).  Each iteration:
--
--   1. Builds a context from system prompt + conversation
--   2. Calls the provider
--   3. If the response contains tool calls, checks policy and executes them
--   4. Loops back with updated conversation
processTurn :: AgentState -> IO AgentState
processTurn state = do
  let cfg  = asConfig state
      prov = asProvider state
      reg  = asRegistry state

  -- Build context: system prompt + conversation
  systemMd <- loadSystemMd
  agentsMd <- loadAgentsMd
  let sysMsg    = mkSystemMessage (buildSystemPrompt reg systemMd agentsMd)
      context   = sysMsg : asConversation state

  -- Context-window guard: estimate total size before calling provider.
  -- This is a cheap character-count check that prevents oversized
  -- requests from reaching the provider.  No truncation or summarization
  -- is performed — the conversation must be shortened manually.
  let stats = contextStats context (cfgMaxContextChars cfg)
  when (csCurrent stats > csMax stats) $ do
    let msg = formatContextLimitRefusal (csCurrent stats) (csMax stats)
    emitDisplayEvent state $ DisplayContextLimitRefusal (csCurrent stats) (csMax stats)
    fail (T.unpack msg)

  -- Call the provider (prefer streaming when available)
  let req = CompletionRequest
        { crMessages  = context
        , crModel     = T.pack (pcModel (cfgProvider cfg))
        , crMaxTokens = cfgMaxTokens cfg
        }
  let mStream = providerStream prov
  resp <- case mStream of
    Just stream -> do
      displayStarted <- newIORef False
      let display = asDisplay state
      let onTokenLazy chunk
            | T.null chunk = pure ()
            | otherwise = do
                started <- readIORef displayStarted
                unless started $ do
                  writeIORef displayStarted True
                  agentDisplayStreamBegin display
                agentDisplayStreamChunk display chunk
      r <- do
        result <- try $ stream req StreamHandler { onToken = onTokenLazy }
        case result of
          Left (e :: SomeException) -> do
            started <- readIORef displayStarted
            when started (agentDisplayStreamEnd display)
            throwIO e
          Right val -> pure val
      started <- readIORef displayStarted
      when started (agentDisplayStreamEnd display)
      pure r
    Nothing ->
      providerComplete prov req

  -- Record the assistant reply (identical for both paths)
  now <- getCurrentTime
  let reply = crReply resp
      -- If the response includes tool calls, attach them to the
      -- assistant message so the OpenAI wire format includes the
      -- required "tool_calls" array on the preceding assistant
      -- message.  Without this, subsequent "tool" role messages
      -- would fail validation.
      replyWithCalls = case crToolCalls resp of
        Nothing  -> reply
        Just tcs -> reply { msgToolCalls = Just tcs }
      -- Build a descriptive event payload.  When tool calls are
      -- present we include a summary so the session log shows
      -- what the assistant asked to do, not just the text reply.
      replyEventData = case crToolCalls resp of
        Nothing  -> msgContent reply
        Just tcs -> msgContent reply
                 <> " | tool_calls: "
                 <> T.intercalate ", "
                      [ tcId t <> "=" <> tcName t | t <- tcs ]
      state1 = state
        { asConversation = appendMessage replyWithCalls (asConversation state)
        , asSession = logEvent
            (Event now EAssistantReply replyEventData)
            (asSession state)
        }

  -- Check for tool calls
  case crToolCalls resp of
    Nothing -> do
      -- Final text reply.  Streaming already displayed it via
      -- streamBegin/streamChunk/streamEnd; non-streaming displays it now.
      case mStream of
        Just _  -> pure ()
        Nothing -> emitDisplayEvent state1 $ DisplayAssistant (msgContent reply)
      pure state1
    Just calls -> do
      -- Process each tool call and loop
      state2 <- processToolCalls state1 calls
      processTurn state2

-- ---------------------------------------------------------------------------
-- Tool-call processing
-- ---------------------------------------------------------------------------

-- | Process a list of tool calls: check policy, execute allowed ones,
--   deny blocked ones, and append results to the conversation.
processToolCalls :: AgentState -> [ToolCall] -> IO AgentState
processToolCalls state [] = pure state
processToolCalls state (tc:tcs) = do
  state1 <- processSingleToolCall state tc
  processToolCalls state1 tcs

-- | Write-like tools get a short action label in confirmation
--   metadata.  These labels are user-facing, so keep them stable.
writeLikeToolLabel :: ToolCall -> Maybe Text
writeLikeToolLabel tc =
  case tcName tc of
    "apply_patch"       -> Just "patch"
    "write_file"        -> Just "write"
    "apply_patch_batch" -> Just "batch apply"
    _                   -> Nothing

-- | Extract the single target path used in confirmation metadata.
--   Batch applies have per-operation paths, so they intentionally do
--   not return a single display path here.
writeLikeToolPath :: ToolCall -> Maybe Text
writeLikeToolPath tc =
  case tcName tc of
    "apply_patch" -> extractTextField "path" (tcArgs tc)
    "write_file"  -> extractTextField "path" (tcArgs tc)
    _             -> Nothing

-- | Reason text passed to approval functions and recorded for user
--   rejection.  Preserves the existing wording for write-like tools.
writeLikeDecisionReason :: ToolCall -> Text -> Text
writeLikeDecisionReason tc reason =
  case (writeLikeToolLabel tc, writeLikeToolPath tc) of
    (Just label, Just path) -> label <> " " <> path <> " -- " <> reason
    (Just "batch apply", _) -> "batch apply -- " <> reason
    _                      -> reason

-- | Audit text for an approved confirmation.  Single-file write-like
--   tools include their target path when one was provided.
approvalAuditText :: ToolCall -> Text
approvalAuditText tc =
  case writeLikeToolPath tc of
    Just path -> tcName tc <> ": approved -- " <> path
    Nothing   -> tcName tc <> ": approved by user"

-- | Audit text for a rejected confirmation.
denialAuditText :: ToolCall -> Text -> Text
denialAuditText = writeLikeDecisionReason

-- | Show any write-like confirmation preview for the tool call.
showConfirmationPreview :: ToolCall -> IO ()
showConfirmationPreview tc =
  case tcName tc of
    "apply_patch"       -> showPatchPreview tc
    "write_file"        -> showWriteFilePreview tc
    "apply_patch_batch" -> showBatchApplyPreview tc
    _                   -> pure ()

-- | Write-like tool results can contain large diffs, so their session
--   event output is bounded.
isWriteLikeTool :: ToolCall -> Bool
isWriteLikeTool tc =
  case writeLikeToolLabel tc of
    Just _  -> True
    Nothing -> False

-- | Process a single tool call.
processSingleToolCall :: AgentState -> ToolCall -> IO AgentState
processSingleToolCall state tc = do
  let pol = asPolicy state

  -- Check policy
  let decision = checkPolicy pol tc
  now <- getCurrentTime
  let state1 = state
        { asSession = logEvent
            (Event now EPolicyDecision
              (tcName tc <> ": " <> T.pack (show decision)))
            (asSession state)
        }

  case decision of
    Deny reason -> do
      -- Append a denial result
      now2 <- getCurrentTime
      let resultMsg = Message
            { msgRole      = User
            , msgContent   = "Tool '" <> tcName tc <> "' denied: " <> reason
            , msgCallId    = Just (tcId tc)
            , msgToolCalls = Nothing
            }
          state2 = state1
            { asConversation = appendMessage resultMsg (asConversation state1)
            , asSession = logEvent
                (Event now2 EToolResult
                  (tcId tc <> " denied: " <> reason))
                (asSession state1)
            }
      emitDisplayEvent state2 $ DisplayPolicyDenied (tcName tc) reason
      pure state2

    AskUser reason -> do
      -- Ask the user for confirmation via the injected approval function.
      -- In the CLI this prints tool info and reads y/N from stdin.
      -- In tests, autoApprove or autoReject is injected instead.
      let approve  = asApproval state
          enrichedReason = writeLikeDecisionReason tc reason
      emitDisplayEvent state1 $ DisplayPolicyConfirmationNeeded (tcName tc)
      agentDisplayPreview (asDisplay state1) tc
      approved <- approve tc enrichedReason
      if approved
        then do
          -- User approved — execute the tool (same path as Allow)
          emitDisplayEvent state1 DisplayPolicyApproved
          now2 <- getCurrentTime
          let approvalData = approvalAuditText tc
          let state2 = state1
                { asSession = logEvent
                    (Event now2 EPolicyDecision approvalData)
                    (asSession state1)
                }
          executeTool state2 tc
        else do
          -- User rejected — log and return a denial result
          emitDisplayEvent state1 DisplayPolicyRejected
          now2 <- getCurrentTime
          let denialReason = denialAuditText tc reason
          let resultMsg = Message
                { msgRole      = User
                , msgContent   = "Tool '" <> tcName tc <> "' denied by user: " <> denialReason
                , msgCallId    = Just (tcId tc)
                , msgToolCalls = Nothing
                }
              state2 = state1
                { asConversation = appendMessage resultMsg (asConversation state1)
                , asSession = logEvent
                    (Event now2 EToolResult
                      (tcId tc <> " denied by user: " <> denialReason))
                    (asSession state1)
                }
          pure state2

    Allow -> executeTool state1 tc

-- ---------------------------------------------------------------------------
-- Tool execution
-- ---------------------------------------------------------------------------

-- | Execute a tool call: look it up in the registry, run it, and
--   append the result to the conversation.  Shared by the @Allow@
--   path and the approved-@AskUser@ path.
executeTool :: AgentState -> ToolCall -> IO AgentState
executeTool state tc = do
  let reg = asRegistry state
  case lookupTool (tcName tc) reg of
    Nothing -> do
      -- Unknown or disabled tool.
      now <- getCurrentTime
      let resultMsg = Message
            { msgRole      = User
            , msgContent   = "Error: unknown or disabled tool '" <> tcName tc <> "'"
            , msgCallId    = Just (tcId tc)
            , msgToolCalls = Nothing
            }
          state' = state
            { asConversation = appendMessage resultMsg (asConversation state)
            , asSession = logEvent
                (Event now EToolResult
                  (tcId tc <> " error: unknown or disabled tool " <> tcName tc))
                (asSession state)
            }
      emitDisplayEvent state' $ DisplayToolUnknown (tcName tc)
      pure state'

    Just tool -> do
      -- Execute
      emitDisplayEvent state $ DisplayToolExecuting (tcName tc)
      now <- getCurrentTime
      let state' = state
            { asSession = logEvent
                (Event now EToolCall
                  (tcId tc <> " " <> tcName tc <> " " <> encodeText (tcArgs tc)))
                (asSession state)
            }

      result <- toolExecute tool (tcArgs tc)

      -- Append tool result to conversation.
      -- The conversation keeps the full output (for the model), but
      -- the session event truncates large diffs from apply_patch to
      -- keep the JSONL audit trail bounded.
      now2 <- getCurrentTime
      let fullOutput   = trOutput result
          sessionOutput = if isWriteLikeTool tc
                           then truncatePatchLog fullOutput
                           else fullOutput
          resultMsg = Message
            { msgRole      = User
            , msgContent   = fullOutput
            , msgCallId    = Just (tcId tc)
            , msgToolCalls = Nothing
            }
          state'' = state'
            { asConversation = appendMessage resultMsg (asConversation state')
            , asSession = logEvent
                (Event now2 EToolResult
                  (tcId tc <> " " <> sessionOutput))
                (asSession state')
            }
      emitDisplayEvent state'' $ DisplayToolResult (truncateDisplay fullOutput)
      pure state''

-- | Emit a structured display event through the state's display sink.
emitDisplayEvent :: AgentState -> DisplayEvent -> IO ()
emitDisplayEvent state = agentDisplayEvent (asDisplay state)

-- | Show a diff preview for an @apply_patch@ tool call.
--   Prints the target path and a concise unified diff to the terminal
--   so the user can see exactly what will change before approving.
--   Failures are silently ignored (the approval prompt still works;
--   the diff just won't be shown).
showPatchPreview :: ToolCall -> IO ()
showPatchPreview tc = do
  result <- computePatchPreview (tcArgs tc)
  case result of
    Left _err ->
      -- Preview failed; the approval prompt still works without it.
      pure ()
    Right (path, diff) -> do
      TIO.putStrLn $ formatConfirmFile (T.pack path)
      TIO.putStrLn   formatConfirmDiffHeader
      TIO.putStrLn $ indentBlock 4 (colorizeUnifiedDiff diff)

-- | Show a preview for a @write_file@ tool call.
--   Prints the target path and a concise diff-like preview to the
--   terminal so the user can see what will be created before approving.
--   Failures are silently ignored (the approval prompt still works;
--   the preview just won't be shown).
showWriteFilePreview :: ToolCall -> IO ()
showWriteFilePreview tc = do
  result <- computeWriteFilePreview (tcArgs tc)
  case result of
    Left _err ->
      -- Preview failed; the approval prompt still works without it.
      pure ()
    Right (path, preview) -> do
      TIO.putStrLn $ formatConfirmFile (T.pack path)
      TIO.putStrLn   formatConfirmPreviewHeader
      TIO.putStrLn $ indentBlock 4 (colorizeUnifiedDiff preview)

-- | Show a preview for an @apply_patch_batch@ tool call.
--   Prints a concise per-file summary and bounded diff previews
--   to the terminal so the user can see all changes before approving.
--   Failures are silently ignored (the approval prompt still works;
--   the preview just won't be shown).
showBatchApplyPreview :: ToolCall -> IO ()
showBatchApplyPreview tc = do
  result <- computeBatchApplyPreview (tcArgs tc)
  case result of
    Left _err ->
      -- Preview failed; the approval prompt still works without it.
      pure ()
    Right (summary, parts) -> do
      TIO.putStrLn $ indentBlock 2 summary
      mapM_ (\p -> TIO.putStrLn $ indentBlock 4 (colorizeUnifiedDiff p)) parts

-- | Encode a JSON Value to Text (for logging).
--   Produces clean JSON without Haskell string escaping.
encodeText :: Value -> Text
encodeText = TE.decodeUtf8 . LBS.toStrict . encode

-- | Truncate text for terminal display with a clear marker.
truncateDisplay :: Text -> Text
truncateDisplay t
  | T.length t <= 200 = t
  | otherwise = T.take 200 t <> "... [" <> T.pack (show (T.length t)) <> " chars total]"

-- | Maximum characters kept from an apply_patch result in the session
--   event log.  The full diff is kept in the conversation (for the
--   model) and printed to the terminal, but the session event is
--   bounded to avoid bloating the JSONL audit trail.
patchResultLogLimit :: Int
patchResultLogLimit = 1024

-- | Truncate the diff portion of an apply_patch result for the
--   session event log.  Keeps the first @patchResultLogLimit@
--   characters and appends a marker when truncation occurs.
--   Non-patch results pass through unchanged.
truncatePatchLog :: Text -> Text
truncatePatchLog t
  | T.length t <= patchResultLogLimit = t
  | otherwise = T.take patchResultLogLimit t
              <> "\n[truncated: " <> T.pack (show (T.length t)) <> " chars total]"
