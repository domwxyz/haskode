{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

-- | Core data, provider stub, tool, and agent-loop tests.
module Haskode.Test.Core (tests) where

import Data.Aeson ( object, encode, decode, KeyValue((.=)) )
import Data.Time.Clock ( getCurrentTime )
import Haskode.Agent
    ( AgentState(asSession, asConversation),
      AgentDisplay(..),
      ContextStats (..),
      autoApprove,
      buildSystemPrompt,
      contextStats,
      estimateContextChars,
      initState,
      initStateWithDisplay,
      runAgent )
import Haskode.Config
    ( defaultConfig,
      defaultMaxContextChars,
      Config(cfgMaxContextChars, cfgMaxTokens) )
import Haskode.Core
    ( ToolResult(trOutput),
      mkAssistantMessage,
      mkUserMessage,
      Message(Message, msgRole, msgToolCalls, msgCallId, msgContent),
      Role(User, Assistant),
      ToolCall(ToolCall, tcId, tcName) )
import Haskode.Display ( DisplayEvent(..) )
import Haskode.Patch ( makePatch, showDiff )
import Haskode.Policy ( checkPolicy, defaultPolicy, Decision(..) )
import Haskode.Provider
    ( scriptedProvider,
      stubProvider,
      CompletionRequest(crMaxTokens, CompletionRequest, crMessages,
                        crModel),
      CompletionResponse(crToolCalls, CompletionResponse, crReply),
      Provider(providerComplete) )
import Haskode.Session
    ( emptyLog,
      events,
      logEvent,
      Event(evType, Event, evData),
      EventType(EToolResult, EPolicyDecision, EUserMessage,
                EAssistantReply, EToolCall) )
import Haskode.Test.Util ( createTestTree, Test )
import Haskode.Tools
    ( defaultRegistry,
      disableTools,
      extractTextField,
      listFilesTool,
      readFileTool,
      shellTool,
      toolNames,
      Tool(toolExecute) )
import System.Directory
    ( getCurrentDirectory, getTemporaryDirectory, setCurrentDirectory )
import System.IO ( hClose, openTempFile )
import System.Info ( os )
import qualified Data.Text as T ( Text, isInfixOf, pack, unpack )
import qualified Data.Text.IO as TIO ( hPutStrLn )
import qualified Data.IORef as IORef

-- ---------------------------------------------------------------------------
-- Basic data and registry tests
-- ---------------------------------------------------------------------------

testMessageRoundtrip :: Test
testMessageRoundtrip = do
  let msg = mkUserMessage "hello"
  case decode (encode msg) of
    Nothing -> pure $ Left "Message JSON roundtrip failed"
    Just msg'
      | msgContent msg' == "hello" && msgRole msg' == User -> pure $ Right ()
      | otherwise -> pure $ Left "Message roundtrip: content mismatch"

testRoleJSON :: Test
testRoleJSON = do
  let roles = [minBound .. maxBound :: Role]
  let roundtrip r = decode (encode r) == Just r
  if all roundtrip roles
    then pure $ Right ()
    else pure $ Left "Role JSON roundtrip failed"

testStubProvider :: Test
testStubProvider = do
  let req = CompletionRequest
        { crMessages  = [mkUserMessage "test input"]
        , crModel     = "test"
        , crMaxTokens = 100
        }
  resp <- providerComplete stubProvider req
  if T.isInfixOf "test input" (msgContent (crReply resp))
    then pure $ Right ()
    else pure $ Left "StubProvider did not echo user input"

testDefaultConfig :: Test
testDefaultConfig = do
  let cfg = defaultConfig
  if cfgMaxTokens cfg > 0
    then pure $ Right ()
    else pure $ Left "DefaultConfig has unexpected values"

testPolicyAllow :: Test
testPolicyAllow =
  case checkPolicy defaultPolicy (ToolCall "1" "read_file" "null") of
    Allow -> pure $ Right ()
    other -> pure $ Left $ "Expected Allow for read_file, got: " ++ show other

testPolicyDeny :: Test
testPolicyDeny =
  case checkPolicy defaultPolicy (ToolCall "1" "shell" "\"rm -rf /\"") of
    Deny _ -> pure $ Right ()
    other  -> pure $ Left $ "Expected Deny for rm -rf /, got: " ++ show other

testPolicyAsk :: Test
testPolicyAsk =
  case checkPolicy defaultPolicy (ToolCall "1" "write_file" "{}") of
    AskUser _ -> pure $ Right ()
    other     -> pure $ Left $ "Expected AskUser for write_file, got: " ++ show other

testToolRegistry :: Test
testToolRegistry = do
  let names = toolNames defaultRegistry
  if "read_file" `elem` names && "shell" `elem` names
    then pure $ Right ()
    else pure $ Left $ "Default registry missing tools: " ++ show names

-- | A tool removed from the registry cannot execute even if a provider
--   calls it anyway.
testDisabledToolCallUsesUnknownPath :: Test
testDisabledToolCallUsesUnknownPath = do
  case disableTools ["shell"] defaultRegistry of
    Left err -> pure $ Left $ "disableTools failed: " ++ T.unpack err
    Right reg -> do
      prov <- scriptedProvider
        [ CompletionResponse
            { crReply     = mkAssistantMessage "Let me run that."
            , crToolCalls = Just
                [ ToolCall "tc-disabled-shell" "shell"
                    (object ["command" .= ("echo should-not-run" :: T.Text)])
                ]
            }
        , CompletionResponse
            { crReply     = mkAssistantMessage "I could not run it."
            , crToolCalls = Nothing
            }
        ]
      let state = initState defaultConfig prov defaultPolicy reg autoApprove False
      state' <- runAgent state "try a shell command"
      let evts = events (asSession state')
          toolResults = filter (\e -> evType e == EToolResult) evts
          toolCalls = filter (\e -> evType e == EToolCall) evts
          disabledResult =
            any (T.isInfixOf "unknown or disabled tool shell" . evData) toolResults
      if disabledResult && null toolCalls
        then pure $ Right ()
        else pure $ Left $
          "disabled tool should not execute; toolResults="
          ++ show toolResults
          ++ " toolCalls="
          ++ show toolCalls

testSessionLog :: Test
testSessionLog = do
  now <- getCurrentTime
  let ev   = Event now EUserMessage "hello"
      log' = logEvent ev emptyLog
  case events log' of
    [ev'] | evType ev' == EUserMessage -> pure $ Right ()
    _ -> pure $ Left "Session log event mismatch"

testPatchDiff :: Test
testPatchDiff = do
  let p    = makePatch "test.txt" "old\n" "new\n"
      diff = showDiff p
  if T.isInfixOf "--- test.txt" diff && T.isInfixOf "+new" diff
    then pure $ Right ()
    else pure $ Left $ "Patch diff unexpected: " ++ T.unpack diff

-- ---------------------------------------------------------------------------
-- Basic tool and agent loop tests
-- ---------------------------------------------------------------------------

-- | Valid tool-call JSON roundtrips correctly through ToolCall.
testToolCallJSONParse :: Test
testToolCallJSONParse = do
  let tc = ToolCall "call-1" "read_file" (object ["path" .= ("foo.hs" :: T.Text)])
  case decode (encode tc) of
    Nothing  -> pure $ Left "ToolCall JSON roundtrip failed"
    Just tc'
      | tcId tc' == "call-1" && tcName tc' == "read_file" -> pure $ Right ()
      | otherwise -> pure $ Left $ "ToolCall roundtrip mismatch: " ++ show tc'

-- | Unknown tools are reported cleanly when looked up.
testUnknownToolDenied :: Test
testUnknownToolDenied = do
  let tc = ToolCall "call-2" "nonexistent_tool" (object [])
  case checkPolicy defaultPolicy tc of
    AskUser _ -> pure $ Right ()
    other     -> pure $ Left $ "Expected AskUser for unknown tool, got: " ++ show other

-- | read_file tool actually reads a file within the working directory.
testReadFileExecutes :: Test
testReadFileExecutes = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object ["path" .= ("Main.hs" :: T.Text)]
  result <- toolExecute readFileTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "module Main" out
    then pure $ Right ()
    else pure $ Left $ "read_file output unexpected: " ++ T.unpack out

-- | list_files tool lists directory contents within the working directory.
testListFilesExecutes :: Test
testListFilesExecutes = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object ["dir" .= ("." :: T.Text)]
  result <- toolExecute listFilesTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "Main.hs" out && T.isInfixOf "README.md" out
    then pure $ Right ()
    else pure $ Left $ "list_files output unexpected: " ++ T.unpack out

-- | shell tool executes a command with structured output.
testShellExecutes :: Test
testShellExecutes = do
  let args = object ["command" .= ("echo haskode-shell-test" :: T.Text)]
  result <- toolExecute shellTool args
  let out = trOutput result
  if T.isInfixOf "haskode-shell-test" out
     && T.isInfixOf "[exit]" out
     && T.isInfixOf "[stdout]" out
     && T.isInfixOf "[stderr]" out
    then pure $ Right ()
    else pure $ Left $ "shell output unexpected: " ++ T.unpack out

-- | Shell with dangerous command is denied by policy.
testShellDangerousDenied :: Test
testShellDangerousDenied =
  let args = object ["command" .= ("rm -rf /" :: T.Text)]
      tc   = ToolCall "call-3" "shell" args
  in case checkPolicy defaultPolicy tc of
       Deny _ -> pure $ Right ()
       other  -> pure $ Left $ "Expected Deny for dangerous shell, got: " ++ show other

-- | Shell with safe command falls through to AskUser.
testShellSafeAskUser :: Test
testShellSafeAskUser =
  let args = object ["command" .= ("ls -la" :: T.Text)]
      tc   = ToolCall "call-4" "shell" args
  in case checkPolicy defaultPolicy tc of
       AskUser _ -> pure $ Right ()
       other     -> pure $ Left $ "Expected AskUser for safe shell, got: " ++ show other

-- | Helper: generate a command that produces long output (cross-platform).
longOutputCmd :: String
longOutputCmd
  | os == "mingw32" = "powershell -c \"1..5000 | % { 'x' }\""
  | otherwise       = "seq 1 1000 | while read i; do printf 'x%.0s' $(seq 1 6); done"

-- | Shell output includes section markers and truncation metadata.
testShellOutputFormatting :: Test
testShellOutputFormatting = do
  -- Generate a command whose output exceeds 4096 chars to trigger truncation.
  let args = object ["command" .= (T.pack longOutputCmd :: T.Text)]
  result <- toolExecute shellTool args
  let out = trOutput result
  -- Verify section markers are present
  let hasExit   = T.isInfixOf "[exit]" out
      hasStdout = T.isInfixOf "[stdout]" out
      hasStderr = T.isInfixOf "[stderr]" out
  -- If output is long enough, we should see truncation metadata
  let hasTruncMeta = T.isInfixOf "[truncated" out
  if hasExit && hasStdout && hasStderr && hasTruncMeta
    then pure $ Right ()
    else pure $ Left $ "exit=" ++ show hasExit ++ " stdout=" ++ show hasStdout
                     ++ " stderr=" ++ show hasStderr ++ " trunc=" ++ show hasTruncMeta

-- | Multi-tool conversation: assistant calls two tools, gets two
--   results, then gives a final reply.  Verifies the full agent loop
--   produces the expected session events and the conversation history
--   preserves tool_calls on assistant messages.
testMultiToolConversationHistory :: Test
testMultiToolConversationHistory = do
  prov <- scriptedProvider
    [ -- First response: request two tool calls
      CompletionResponse
        { crReply     = mkAssistantMessage "Let me check both."
        , crToolCalls = Just
            [ ToolCall "tc-m1" "read_file" (object ["path" .= ("foo.hs" :: T.Text)])
            , ToolCall "tc-m2" "list_files" (object ["dir" .= ("." :: T.Text)])
            ]
        }
    , -- Second response: final text reply
      CompletionResponse
        { crReply     = mkAssistantMessage "Here is what I found."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove False
  state' <- runAgent state "check files"
  let conv = asConversation state'
      evts = events (asSession state')
      types = map evType evts
  -- Verify session events: UserMessage, AssistantReply, ToolCall, ToolResult x2, AssistantReply
  let hasUser    = EUserMessage `elem` types
      hasAsst    = EAssistantReply `elem` types
      hasTCall   = EToolCall `elem` types
      hasTResult = EToolResult `elem` types
  -- Verify conversation structure: user, assistant-with-calls, tool-result x2, assistant
  let hasAssistantWithCalls = any
        (\m -> msgRole m == Assistant && msgToolCalls m /= Nothing) conv
      toolResults = filter (\m -> msgCallId m /= Nothing) conv
      toolResultCount = length toolResults
  if hasUser && hasAsst && hasTCall && hasTResult
     && hasAssistantWithCalls && toolResultCount == 2
    then pure $ Right ()
    else pure $ Left $ "events=" ++ show types
                     ++ " assistantWithCalls=" ++ show hasAssistantWithCalls
                     ++ " toolResults=" ++ show toolResultCount

-- | Shell approval flow: a safe shell command is flagged AskUser by
--   policy, autoApprove lets it through, and the agent loop completes
--   with the shell output in the conversation.
testShellApprovalFlow :: Test
testShellApprovalFlow = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Let me run that."
        , crToolCalls = Just [ToolCall "tc-sh1" "shell"
                               (object ["command" .= ("echo approved-shell-test" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "The command ran successfully."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove False
  state' <- runAgent state "run echo"
  let evts = events (asSession state')
      types = map evType evts
      conv  = asConversation state'
  -- Shell is AskUser; autoApprove lets it through to execution.
  let hasPolicyDecision = EPolicyDecision `elem` types
      hasToolResult     = EToolResult `elem` types
      hasApproved       = any (T.isInfixOf "approved by user" . evData) evts
      -- The tool result should contain shell output with [exit] marker
      toolResults = filter (\m -> msgCallId m /= Nothing) conv
      hasExitMarker = any (T.isInfixOf "[exit]" . msgContent) toolResults
  if hasPolicyDecision && hasToolResult && hasApproved && hasExitMarker
    then pure $ Right ()
    else pure $ Left $ "policy=" ++ show hasPolicyDecision
                     ++ " result=" ++ show hasToolResult
                     ++ " approved=" ++ show hasApproved
                     ++ " exitMarker=" ++ show hasExitMarker
testPolicyStructuredArgs :: Test
testPolicyStructuredArgs =
  let args = object ["command" .= ("mkfs.ext4 /dev/sda" :: T.Text)]
      tc   = ToolCall "call-5" "shell" args
  in case checkPolicy defaultPolicy tc of
       Deny _ -> pure $ Right ()
       other  -> pure $ Left $ "Expected Deny for mkfs, got: " ++ show other

-- | extractTextField works on JSON objects.
testExtractTextField :: Test
testExtractTextField = do
  let val = object ["name" .= ("haskode" :: T.Text), "count" .= (42 :: Int)]
  case extractTextField "name" val of
    Just "haskode" -> pure $ Right ()
    other          -> pure $ Left $ "extractTextField failed: " ++ show other

-- | Agent loop records session events for a simple text exchange.
testAgentLoopEvents :: Test
testAgentLoopEvents = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "echo reply"
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove False
  state' <- runAgent state "hello agent"
  let evts = events (asSession state')
      types = map evType evts
  if EUserMessage `elem` types && EAssistantReply `elem` types
    then pure $ Right ()
    else pure $ Left $ "Agent session events missing, got: " ++ show types

-- | Agent display events can be consumed without rendering terminal text.
testAgentCustomDisplaySink :: Test
testAgentCustomDisplaySink = do
  displayRef <- IORef.newIORef []
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "display reply"
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      capture event = IORef.modifyIORef displayRef (<> [event])
      display = AgentDisplay
        { agentDisplayEvent = capture
        , agentDisplayStreamBegin = pure ()
        , agentDisplayStreamChunk = \_chunk -> pure ()
        , agentDisplayStreamEnd = pure ()
        , agentDisplayPreview = \_toolCall -> pure ()
        }
      state = initStateWithDisplay cfg prov defaultPolicy defaultRegistry autoApprove display False
  _ <- runAgent state "hello agent"
  displayEvents <- IORef.readIORef displayRef
  if DisplayAssistant "display reply" `elem` displayEvents
    then pure $ Right ()
    else pure $ Left $ "Custom display sink missed assistant event: " ++ show displayEvents

-- | Agent loop executes a tool call and continues to final reply.
testAgentLoopToolExecution :: Test
testAgentLoopToolExecution = do
  tmpDir <- getTemporaryDirectory
  (path, h) <- openTempFile tmpDir "haskode-agent-test.txt"
  TIO.hPutStrLn h "agent loop test content"
  hClose h
  prov <- scriptedProvider
    [ -- First response: request a tool call
      CompletionResponse
        { crReply     = mkAssistantMessage "Let me read that file."
        , crToolCalls = Just [ToolCall "tc-1" "read_file" (object ["path" .= T.pack path])]
        }
    , -- Second response: final text reply
      CompletionResponse
        { crReply     = mkAssistantMessage "I read the file successfully."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove False
  state' <- runAgent state "read the test file"
  let evts = events (asSession state')
      types = map evType evts
  -- Should have: UserMessage, AssistantReply, ToolCall, ToolResult, AssistantReply
  if EUserMessage `elem` types
     && EAssistantReply `elem` types
     && EToolCall `elem` types
     && EToolResult `elem` types
    then pure $ Right ()
    else pure $ Left $ "Agent tool execution events: " ++ show types

-- | buildSystemPrompt includes tool names.
testBuildSystemPrompt :: Test
testBuildSystemPrompt = do
  let prompt = buildSystemPrompt defaultRegistry Nothing Nothing
  if T.isInfixOf "read_file" prompt
     && T.isInfixOf "list_files" prompt
     && T.isInfixOf "shell" prompt
    then pure $ Right ()
    else pure $ Left "System prompt missing expected tool names"

-- ---------------------------------------------------------------------------
-- Context estimation & guard tests
-- ---------------------------------------------------------------------------

-- | estimateContextChars counts user message content.
testEstimateContextCharsUserMsg :: Test
testEstimateContextCharsUserMsg = do
  let conv = [mkUserMessage "hello world"]  -- 11 chars
      est  = estimateContextChars conv
  -- Should be >= content length (plus per-message overhead of 20)
  if est >= 11
    then pure $ Right ()
    else pure $ Left $ "estimateContextChars user msg: " ++ show est

-- | estimateContextChars counts assistant message content.
testEstimateContextCharsAssistantMsg :: Test
testEstimateContextCharsAssistantMsg = do
  let conv = [mkAssistantMessage "a reply"]  -- 7 chars
      est  = estimateContextChars conv
  if est >= 7
    then pure $ Right ()
    else pure $ Left $ "estimateContextChars assistant msg: " ++ show est

-- | estimateContextChars counts tool-call argument JSON.
testEstimateContextCharsToolCalls :: Test
testEstimateContextCharsToolCalls = do
  let tc   = ToolCall "call-1" "read_file" (object ["path" .= ("very/long/path/to/file.hs" :: T.Text)])
      msg  = Message Assistant "checking" Nothing (Just [tc])
      conv = [msg]
      est  = estimateContextChars conv
  -- Must be strictly greater than just the content "checking" (8 chars)
  -- because tool-call id, name, and args JSON are counted.
  if est > 8 + 20  -- content + overhead
    then pure $ Right ()
    else pure $ Left $ "estimateContextChars tool calls: " ++ show est

-- | estimateContextChars returns 0 for empty conversation.
testEstimateContextCharsEmpty :: Test
testEstimateContextCharsEmpty = do
  let est = estimateContextChars []
  if est == 0
    then pure $ Right ()
    else pure $ Left $ "estimateContextChars empty: " ++ show est

-- | Default config has a positive cfgMaxContextChars.
testDefaultConfigMaxContextChars :: Test
testDefaultConfigMaxContextChars =
  if cfgMaxContextChars defaultConfig > 0
     && cfgMaxContextChars defaultConfig == defaultMaxContextChars
    then pure $ Right ()
    else pure $ Left $ "defaultConfig cfgMaxContextChars: "
                     ++ show (cfgMaxContextChars defaultConfig)

-- ---------------------------------------------------------------------------
-- ContextStats tests
-- ---------------------------------------------------------------------------

-- | contextStats returns correct values for a normal (well under limit) case.
testContextStatsNormal :: Test
testContextStatsNormal = do
  let conv   = [mkUserMessage "hello"]  -- 5 + 20 = 25 chars
      maxC   = 120000
      stats  = contextStats conv maxC
      expect = ContextStats
        { csCurrent   = estimateContextChars conv
        , csMax       = maxC
        , csRemaining = maxC - estimateContextChars conv
        , csPercent   = 0
        }
  if stats == expect
    then pure $ Right ()
    else pure $ Left $ "contextStats normal: " ++ show stats

-- | contextStats shows correct percentage near the limit.
testContextStatsNearLimit :: Test
testContextStatsNearLimit = do
  let conv  = [mkUserMessage "hello"]  -- 25 chars
      maxC  = 100
      stats = contextStats conv maxC
  if csCurrent stats == 25
     && csMax stats == 100
     && csRemaining stats == 75
     && csPercent stats == 25
    then pure $ Right ()
    else pure $ Left $ "contextStats near-limit: " ++ show stats

-- | contextStats at exactly the limit shows 100% and 0 remaining.
testContextStatsExactLimit :: Test
testContextStatsExactLimit = do
  let conv  = [mkUserMessage "hello"]  -- 25 chars
      maxC  = 25
      stats = contextStats conv maxC
  if csCurrent stats == 25
     && csMax stats == 25
     && csRemaining stats == 0
     && csPercent stats == 100
    then pure $ Right ()
    else pure $ Left $ "contextStats exact-limit: " ++ show stats

-- | contextStats over limit shows negative remaining and >100%.
testContextStatsOverLimit :: Test
testContextStatsOverLimit = do
  let conv  = [mkUserMessage "this is a longer message for testing"]
      maxC  = 10
      stats = contextStats conv maxC
  if csCurrent stats > maxC
     && csMax stats == 10
     && csRemaining stats < 0
     && csPercent stats > 100
    then pure $ Right ()
    else pure $ Left $ "contextStats over-limit: " ++ show stats

-- | contextStats on empty conversation returns zero current and 0%.
testContextStatsEmpty :: Test
testContextStatsEmpty = do
  let stats = contextStats [] 120000
  if csCurrent stats == 0
     && csRemaining stats == 120000
     && csPercent stats == 0
    then pure $ Right ()
    else pure $ Left $ "contextStats empty: " ++ show stats

-- | contextStats with zero max avoids division by zero.
testContextStatsZeroMax :: Test
testContextStatsZeroMax = do
  let conv  = [mkUserMessage "hello"]
      stats = contextStats conv 0
  if csCurrent stats > 0
     && csMax stats == 0
     && csRemaining stats < 0
     && csPercent stats == 0  -- no division by zero
    then pure $ Right ()
    else pure $ Left $ "contextStats zero-max: " ++ show stats

tests :: [Test]
tests =
  [ testMessageRoundtrip
  , testRoleJSON
  , testStubProvider
  , testDefaultConfig
  , testPolicyAllow
  , testPolicyDeny
  , testPolicyAsk
  , testToolRegistry
  , testDisabledToolCallUsesUnknownPath
  , testSessionLog
  , testPatchDiff
  , testToolCallJSONParse
  , testUnknownToolDenied
  , testReadFileExecutes
  , testListFilesExecutes
  , testShellExecutes
  , testShellDangerousDenied
  , testShellSafeAskUser
  , testShellOutputFormatting
  , testPolicyStructuredArgs
  , testExtractTextField
  , testAgentLoopEvents
  , testAgentCustomDisplaySink
  , testAgentLoopToolExecution
  , testMultiToolConversationHistory
  , testBuildSystemPrompt
  , testEstimateContextCharsUserMsg
  , testEstimateContextCharsAssistantMsg
  , testEstimateContextCharsToolCalls
  , testEstimateContextCharsEmpty
  , testDefaultConfigMaxContextChars
  , testShellApprovalFlow
  , testContextStatsNormal
  , testContextStatsNearLimit
  , testContextStatsExactLimit
  , testContextStatsOverLimit
  , testContextStatsEmpty
  , testContextStatsZeroMax
  ]
