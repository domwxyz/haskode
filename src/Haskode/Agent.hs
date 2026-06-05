{-# LANGUAGE OverloadedStrings #-}

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
    -- * Single-turn processing
  , processTurn
    -- * System prompt
  , buildSystemPrompt
    -- * Approval
  , ApprovalFunc
  , autoApprove
  , autoReject
  , terminalApproval
  ) where

import Data.Aeson       (Value, encode)
import Data.Text        (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import System.IO        (hFlush, stdout)

import Haskode.Config     (Config (..), ProviderConfig (..))
import Haskode.Core       (Conversation, Message (..), Role (..), ToolCall (..),
                           ToolResult (..), emptyConversation,
                           mkUserMessage, mkSystemMessage, appendMessage)
import Haskode.Policy     (Policy, Decision (..), checkPolicy)
import Haskode.Provider   (Provider (..), CompletionRequest (..),
                           CompletionResponse (..))
import Haskode.Session    (Event (..), EventType (..), SessionLog,
                           emptyLog, logEvent)
import Haskode.Tools      (Tool (..), ToolRegistry, toolNames, lookupTool)
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
  TIO.putStrLn $ "  [confirm] Tool:     " <> tcName tc
  TIO.putStrLn $ "  [confirm] Args:     " <> encodeText (tcArgs tc)
  TIO.putStrLn $ "  [confirm] Reason:   " <> reason
  putStr   "  [confirm] Approve? (y/N) "
  hFlush stdout
  answer <- TIO.getLine
  pure (T.strip answer `elem` ["y", "Y"])

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
  }

initState :: Config -> Provider -> Policy -> ToolRegistry -> ApprovalFunc -> AgentState
initState cfg prov pol reg approve = AgentState
  { asConversation = emptyConversation
  , asSession      = emptyLog
  , asConfig       = cfg
  , asProvider     = prov
  , asPolicy       = pol
  , asRegistry     = reg
  , asApproval     = approve
  }

-- ---------------------------------------------------------------------------
-- System prompt
-- ---------------------------------------------------------------------------

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
buildSystemPrompt :: ToolRegistry -> Text
buildSystemPrompt reg =
  "You are a helpful coding assistant. You have access to tools that\n\
  \are provided to you by the system. Use them when needed to answer\n\
  \the user's questions or perform tasks. Do NOT print tool calls as\n\
  \JSON in your reply text — use the tool-calling mechanism provided\n\
  \by the API.\n\
  \\n\
  \Available tools:\n\n"
  <> T.concat (map describeTool (toolNames reg))
  where
    describeTool name = case lookupTool name reg of
      Nothing -> ""
      Just t  -> "- **" <> toolName t <> "**: " <> toolDescription t
              <> "\n  Schema: " <> T.pack (show (toolSchema t)) <> "\n\n"

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
  let sysMsg    = mkSystemMessage (buildSystemPrompt reg)
      context   = sysMsg : asConversation state

  -- Call the provider
  let req = CompletionRequest
        { crMessages  = context
        , crModel     = T.pack (pcModel (cfgProvider cfg))
        , crMaxTokens = cfgMaxTokens cfg
        }
  resp <- providerComplete prov req

  -- Record the assistant reply
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
      state1 = state
        { asConversation = appendMessage replyWithCalls (asConversation state)
        , asSession = logEvent
            (Event now EAssistantReply (msgContent reply))
            (asSession state)
        }

  -- Check for tool calls
  case crToolCalls resp of
    Nothing -> do
      -- Final text reply — display and return
      TIO.putStrLn $ "\nAssistant: " <> msgContent reply <> "\n"
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
      TIO.putStrLn $ "  [policy] Denied: " <> tcName tc <> " — " <> reason
      pure state2

    AskUser reason -> do
      -- Ask the user for confirmation via the injected approval function.
      -- In the CLI this prints tool info and reads y/N from stdin.
      -- In tests, autoApprove or autoReject is injected instead.
      let approve = asApproval state
      TIO.putStrLn $ "  [policy] Confirmation needed: " <> tcName tc
      approved <- approve tc reason
      if approved
        then do
          -- User approved — execute the tool (same path as Allow)
          TIO.putStrLn "  [policy] Approved by user."
          now2 <- getCurrentTime
          let state2 = state1
                { asSession = logEvent
                    (Event now2 EPolicyDecision
                      (tcName tc <> ": approved by user"))
                    (asSession state1)
                }
          executeTool state2 tc
        else do
          -- User rejected — log and return a denial result
          TIO.putStrLn "  [policy] Rejected by user."
          now2 <- getCurrentTime
          let resultMsg = Message
                { msgRole      = User
                , msgContent   = "Tool '" <> tcName tc <> "' denied by user: " <> reason
                , msgCallId    = Just (tcId tc)
                , msgToolCalls = Nothing
                }
              state2 = state1
                { asConversation = appendMessage resultMsg (asConversation state1)
                , asSession = logEvent
                    (Event now2 EToolResult
                      (tcId tc <> " denied by user: " <> reason))
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
      -- Unknown tool
      now <- getCurrentTime
      let resultMsg = Message
            { msgRole      = User
            , msgContent   = "Error: unknown tool '" <> tcName tc <> "'"
            , msgCallId    = Just (tcId tc)
            , msgToolCalls = Nothing
            }
          state' = state
            { asConversation = appendMessage resultMsg (asConversation state)
            , asSession = logEvent
                (Event now EToolResult
                  (tcId tc <> " error: unknown tool " <> tcName tc))
                (asSession state)
            }
      TIO.putStrLn $ "  [error] Unknown tool: " <> tcName tc
      pure state'

    Just tool -> do
      -- Execute
      TIO.putStrLn $ "  [tool] Executing: " <> tcName tc
      now <- getCurrentTime
      let state' = state
            { asSession = logEvent
                (Event now EToolCall
                  (tcId tc <> " " <> tcName tc <> " " <> encodeText (tcArgs tc)))
                (asSession state)
            }

      result <- toolExecute tool (tcArgs tc)

      -- Append tool result to conversation
      now2 <- getCurrentTime
      let resultMsg = Message
            { msgRole      = User
            , msgContent   = trOutput result
            , msgCallId    = Just (tcId tc)
            , msgToolCalls = Nothing
            }
          state'' = state'
            { asConversation = appendMessage resultMsg (asConversation state')
            , asSession = logEvent
                (Event now2 EToolResult
                  (tcId tc <> " " <> trOutput result))
                (asSession state')
            }
      TIO.putStrLn $ "  [tool] Result: " <> T.take 200 (trOutput result)
      pure state''

-- | Encode a JSON Value to Text (for logging).
encodeText :: Value -> Text
encodeText = T.pack . show . encode
