{-# LANGUAGE OverloadedStrings #-}

-- | Test suite for Haskode.
--
-- We use a simple main-based test runner (no external test framework)
-- to keep dependencies minimal.  Tests are plain functions that return
-- Either String () — Right for pass, Left for failure.

module Main (main) where

import Data.Aeson       (Value (..), encode, decode, object, (.=))
import qualified Data.Aeson.Key    as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Vector        as V
import System.Directory  (getTemporaryDirectory, doesFileExist)
import System.Exit       (exitFailure, exitSuccess)
import System.IO         (hClose, openTempFile)

import Haskode.Core
import Haskode.Config    (defaultConfig, Config (..), ProviderConfig (..),
                          tokenLimitFieldName)
import Haskode.Provider  (Provider (..), CompletionRequest (..),
                          CompletionResponse (..), stubProvider, scriptedProvider)
import Haskode.Provider.OpenAI
                          (buildRequestBody, messagesToJSON, messageToJSON, toolsToJSON,
                           parseResponseBody, parseToolCall, OpenAIError (..))
import Haskode.Policy    (checkPolicy, defaultPolicy, Decision (..))
import Haskode.Tools     (defaultRegistry, toolNames, readFileTool, listFilesTool,
                          shellTool, extractTextField, Tool (..))
import Haskode.Session   (emptyLog, logEvent, events, Event (..), EventType (..))
import Haskode.Patch     (makePatch, showDiff)
import Haskode.Agent     (AgentState (..), initState, runAgent, buildSystemPrompt,
                          autoApprove, autoReject)
import Data.Time.Clock   (getCurrentTime)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO

-- ---------------------------------------------------------------------------
-- Test runner
-- ---------------------------------------------------------------------------

type Test = IO (Either String ())

runTests :: [Test] -> IO ()
runTests tests = do
  results <- sequence tests
  let failures = [ e | Left e <- results ]
  if null failures
    then putStrLn ("All " ++ show (length results) ++ " tests passed.")
         >> exitSuccess
    else do
      mapM_ (\e -> putStrLn $ "FAIL: " ++ e) failures
      exitFailure

-- ---------------------------------------------------------------------------
-- Original tests
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
-- Phase 1 tests
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

-- | read_file tool actually reads a file.
testReadFileExecutes :: Test
testReadFileExecutes = do
  tmpDir <- getTemporaryDirectory
  (path, h) <- openTempFile tmpDir "haskode-test-read.txt"
  TIO.hPutStrLn h "hello from test"
  hClose h
  let args = object ["path" .= T.pack path]
  result <- toolExecute readFileTool args
  if T.isInfixOf "hello from test" (trOutput result)
    then do
      -- cleanup
      _ <- doesFileExist path  -- force read to avoid unused warning
      pure $ Right ()
    else pure $ Left $ "read_file output unexpected: " ++ T.unpack (trOutput result)

-- | list_files tool lists directory contents.
testListFilesExecutes :: Test
testListFilesExecutes = do
  let args = object ["dir" .= ("." :: T.Text)]
  result <- toolExecute listFilesTool args
  -- The current directory should have at least "src" and "app"
  if T.isInfixOf "src" (trOutput result) || T.isInfixOf "app" (trOutput result)
    then pure $ Right ()
    else pure $ Left $ "list_files output unexpected: " ++ T.unpack (trOutput result)

-- | shell tool executes a command.
testShellExecutes :: Test
testShellExecutes = do
  let args = object ["command" .= ("echo haskode-shell-test" :: T.Text)]
  result <- toolExecute shellTool args
  if T.isInfixOf "haskode-shell-test" (trOutput result)
    then pure $ Right ()
    else pure $ Left $ "shell output unexpected: " ++ T.unpack (trOutput result)

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

-- | Policy with structured args (object) works the same as bare strings.
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
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "hello agent"
  let evts = events (asSession state')
      types = map evType evts
  if EUserMessage `elem` types && EAssistantReply `elem` types
    then pure $ Right ()
    else pure $ Left $ "Agent session events missing, got: " ++ show types

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
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
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
  let prompt = buildSystemPrompt defaultRegistry
  if T.isInfixOf "read_file" prompt
     && T.isInfixOf "list_files" prompt
     && T.isInfixOf "shell" prompt
    then pure $ Right ()
    else pure $ Left "System prompt missing expected tool names"

-- ---------------------------------------------------------------------------
-- Approval tests
-- ---------------------------------------------------------------------------

-- | An AskUser tool call executes when the approval function approves.
--   We use 'write_file' (which falls through to AskUser in defaultPolicy)
--   and inject autoApprove.  Since write_file is not in the registry,
--   the tool execution path returns "unknown tool" — but the important
--   thing is that it reaches the execution path (not denial).
testApprovalApproved :: Test
testApprovalApproved = do
  tmpDir <- getTemporaryDirectory
  (path, h) <- openTempFile tmpDir "haskode-approval-test.txt"
  TIO.hPutStrLn h "approval test content"
  hClose h
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Let me write that file."
        , crToolCalls = Just [ToolCall "tc-w1" "write_file"
                               (object ["path" .= T.pack path, "content" .= ("new" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "write the file"
  let evts = events (asSession state')
      types = map evType evts
  -- write_file is not in the registry, so we get EToolResult with "unknown tool"
  -- but the key thing: it was NOT denied — it reached the execution path.
  if EPolicyDecision `elem` types && EToolResult `elem` types
    then
      -- Check that no "denied" event appears
      let deniedEvts = filter (\e -> evType e == EToolResult && T.isInfixOf "denied" (evData e)) evts
      in if null deniedEvts
           then pure $ Right ()
           else pure $ Left $ "Expected no denial events, got: " ++ show deniedEvts
    else pure $ Left $ "Expected EPolicyDecision and EToolResult, got: " ++ show types

-- | An AskUser tool call is rejected when the approval function rejects.
testApprovalRejected :: Test
testApprovalRejected = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Let me write that file."
        , crToolCalls = Just [ToolCall "tc-w2" "write_file"
                               (object ["path" .= ("test.txt" :: T.Text), "content" .= ("new" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "OK, I won't."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoReject
  state' <- runAgent state "write the file"
  let evts = events (asSession state')
      types = map evType evts
  -- Should have EPolicyDecision and EToolResult with "denied by user"
  let toolResults = filter (\e -> evType e == EToolResult) evts
      deniedResults = filter (T.isInfixOf "denied by user" . evData) toolResults
  if EPolicyDecision `elem` types && not (null deniedResults)
    then pure $ Right ()
    else pure $ Left $ "Expected denial by user, got events: " ++ show types

-- | Session events include approval info for approved calls.
testApprovalSessionEvents :: Test
testApprovalSessionEvents = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Checking."
        , crToolCalls = Just [ToolCall "tc-w3" "write_file"
                               (object ["path" .= ("x.txt" :: T.Text), "content" .= ("y" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "check"
  let evts = events (asSession state')
      policyEvts = filter (\e -> evType e == EPolicyDecision) evts
      approvedEvts = filter (T.isInfixOf "approved by user" . evData) policyEvts
  if not (null approvedEvts)
    then pure $ Right ()
    else pure $ Left $ "Expected 'approved by user' in policy events, got: " ++ show policyEvts

-- | Session events include rejection info for rejected calls.
testRejectionSessionEvents :: Test
testRejectionSessionEvents = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Checking."
        , crToolCalls = Just [ToolCall "tc-w4" "write_file"
                               (object ["path" .= ("x.txt" :: T.Text), "content" .= ("y" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "OK."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoReject
  state' <- runAgent state "check"
  let evts = events (asSession state')
      toolResultEvts = filter (\e -> evType e == EToolResult) evts
      deniedByUser = filter (T.isInfixOf "denied by user" . evData) toolResultEvts
  if not (null deniedByUser)
    then pure $ Right ()
    else pure $ Left $ "Expected 'denied by user' in tool result events, got: " ++ show toolResultEvts

-- | Dangerous commands are still hard-denied by policy without prompting.
--   Even with autoApprove, a dangerous shell command should be blocked
--   by the Deny rule — the approval function is never consulted.
testDangerousDeniedWithoutPrompting :: Test
testDangerousDeniedWithoutPrompting = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Let me run that."
        , crToolCalls = Just [ToolCall "tc-d1" "shell"
                               (object ["command" .= ("rm -rf /" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "OK."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "delete everything"
  let evts = events (asSession state')
      policyEvts = filter (\e -> evType e == EPolicyDecision) evts
      -- The Deny rule fires, so the policy decision text contains "Deny"
      deniedByPolicy = filter (T.isInfixOf "Deny" . evData) policyEvts
  if not (null deniedByPolicy)
    then pure $ Right ()
    else pure $ Left $ "Expected policy denial for dangerous command, got: " ++ show policyEvts

-- ---------------------------------------------------------------------------
-- OpenAI provider tests (deterministic, no network)
-- ---------------------------------------------------------------------------

-- | Helper: wrap a message JSON into a full OpenAI API response.
mkApiResponse :: Value -> LBS.ByteString
mkApiResponse msgJson = encode $ object
  [ "id"      .= ("chatcmpl-test" :: T.Text)
  , "object"  .= ("chat.completion" :: T.Text)
  , "choices" .= [object
      [ "index"         .= (0 :: Int)
      , "message"       .= msgJson
      , "finish_reason" .= ("stop" :: T.Text)
      ]]
  ]

-- | Sample: plain text reply.
sampleTextResponse :: LBS.ByteString
sampleTextResponse = mkApiResponse $ object
  [ "role"    .= ("assistant" :: T.Text)
  , "content" .= ("Hello! How can I help?" :: T.Text)
  ]

-- | Sample: single tool call.
sampleToolCallResponse :: LBS.ByteString
sampleToolCallResponse = encode $ object
  [ "id"      .= ("chatcmpl-def456" :: T.Text)
  , "object"  .= ("chat.completion" :: T.Text)
  , "choices" .= [object
      [ "index" .= (0 :: Int)
      , "message" .= object
          [ "role"    .= ("assistant" :: T.Text)
          , "content" .= Null
          , "tool_calls" .= [object
              [ "id"   .= ("call_001" :: T.Text)
              , "type" .= ("function" :: T.Text)
              , "function" .= object
                  [ "name"      .= ("read_file" :: T.Text)
                  , "arguments" .= ("{\"path\":\"src/Main.hs\"}" :: T.Text)
                  ]
              ]]
          ]
      , "finish_reason" .= ("tool_calls" :: T.Text)
      ]]
  ]

-- | Sample: multiple tool calls.
sampleMultiToolCallResponse :: LBS.ByteString
sampleMultiToolCallResponse = encode $ object
  [ "id"      .= ("chatcmpl-ghi789" :: T.Text)
  , "object"  .= ("chat.completion" :: T.Text)
  , "choices" .= [object
      [ "index" .= (0 :: Int)
      , "message" .= object
          [ "role"    .= ("assistant" :: T.Text)
          , "content" .= ("Let me check." :: T.Text)
          , "tool_calls" .=
              [ object
                  [ "id"   .= ("call_002" :: T.Text)
                  , "type" .= ("function" :: T.Text)
                  , "function" .= object
                      [ "name"      .= ("read_file" :: T.Text)
                      , "arguments" .= ("{\"path\":\"foo.hs\"}" :: T.Text)
                      ]
                  ]
              , object
                  [ "id"   .= ("call_003" :: T.Text)
                  , "type" .= ("function" :: T.Text)
                  , "function" .= object
                      [ "name"      .= ("list_files" :: T.Text)
                      , "arguments" .= ("{\"dir\":\".\"}" :: T.Text)
                      ]
                  ]
              ]
          ]
      , "finish_reason" .= ("tool_calls" :: T.Text)
      ]]
  ]

-- | buildRequestBody produces valid JSON with model, messages, max_tokens.
testOpenAIRequestShape :: Test
testOpenAIRequestShape = do
  let msgs = [mkUserMessage "hello"]
      body = buildRequestBody "max_tokens" msgs "gpt-4o" 1024 mempty
  case decode body of
    Nothing -> pure $ Left "buildRequestBody produced invalid JSON"
    Just (Object obj) -> do
      let hasModel    = KM.lookup (Key.fromText "model") obj == Just (String "gpt-4o")
          hasMaxTok   = KM.lookup (Key.fromText "max_tokens") obj == Just (Number 1024)
          hasMessages = case KM.lookup (Key.fromText "messages") obj of
                          Just (Array _) -> True
                          _              -> False
      if hasModel && hasMaxTok && hasMessages
        then pure $ Right ()
        else pure $ Left $ "Request JSON missing expected fields: " ++ show obj
    Just other -> pure $ Left $ "Request JSON is not an object: " ++ show other

-- | buildRequestBody includes tools and tool_choice when registry is non-empty.
testOpenAIRequestTools :: Test
testOpenAIRequestTools = do
  let msgs = [mkUserMessage "hello"]
      body = buildRequestBody "max_tokens" msgs "gpt-4o" 1024 defaultRegistry
  case decode body of
    Nothing -> pure $ Left "buildRequestBody with tools produced invalid JSON"
    Just (Object obj) -> do
      let hasTools = case KM.lookup (Key.fromText "tools") obj of
                       Just (Array arr) -> length arr == length (toolNames defaultRegistry)
                       _                -> False
          hasToolChoice = KM.lookup (Key.fromText "tool_choice") obj == Just (String "auto")
      if hasTools && hasToolChoice
        then pure $ Right ()
        else pure $ Left $ "tools=" ++ show hasTools ++ " tool_choice=" ++ show hasToolChoice
    Just other -> pure $ Left $ "Request JSON is not an object: " ++ show other

-- | parseResponseBody handles a plain text reply correctly.
testOpenAIResponseText :: Test
testOpenAIResponseText =
  case parseResponseBody sampleTextResponse of
    Left err -> pure $ Left $ "Failed to parse text response: " ++ show err
    Right resp
      | msgContent (crReply resp) == "Hello! How can I help?"
        && crToolCalls resp == Nothing -> pure $ Right ()
      | otherwise ->
          pure $ Left $ "Unexpected content: " ++ T.unpack (msgContent (crReply resp))

-- | parseResponseBody handles a tool-call response correctly.
testOpenAIResponseToolCall :: Test
testOpenAIResponseToolCall =
  case parseResponseBody sampleToolCallResponse of
    Left err -> pure $ Left $ "Failed to parse tool-call response: " ++ show err
    Right resp -> case crToolCalls resp of
      Nothing -> pure $ Left "Expected tool calls, got Nothing"
      Just [tc]
        | tcId tc == "call_001" && tcName tc == "read_file" -> pure $ Right ()
        | otherwise -> pure $ Left $ "Tool call mismatch: " ++ show tc
      Just tcs -> pure $ Left $ "Expected 1 tool call, got " ++ show (length tcs)

-- | parseResponseBody handles multiple tool calls.
testOpenAIResponseMultiToolCall :: Test
testOpenAIResponseMultiToolCall =
  case parseResponseBody sampleMultiToolCallResponse of
    Left err -> pure $ Left $ "Failed to parse multi-tool response: " ++ show err
    Right resp -> case crToolCalls resp of
      Just [tc1, tc2]
        | tcName tc1 == "read_file" && tcName tc2 == "list_files" -> pure $ Right ()
        | otherwise -> pure $ Left $ "Tool call names mismatch: " ++ show (tcName tc1, tcName tc2)
      Just tcs -> pure $ Left $ "Expected 2 tool calls, got " ++ show (length tcs)
      Nothing -> pure $ Left "Expected tool calls, got Nothing"

-- | parseResponseBody fails cleanly on malformed JSON.
testOpenAIMalformedResponse :: Test
testOpenAIMalformedResponse =
  case parseResponseBody "{bad json" of
    Left (ResponseParseError _) -> pure $ Right ()
    Left other -> pure $ Left $ "Expected ResponseParseError, got: " ++ show other
    Right _ -> pure $ Left "Expected error for malformed JSON, got success"

-- | parseResponseBody fails cleanly when choices array is missing.
testOpenAIMissingChoices :: Test
testOpenAIMissingChoices =
  case parseResponseBody "{\"id\":\"test\",\"object\":\"chat.completion\"}" of
    Left (ResponseParseError _) -> pure $ Right ()
    Left other -> pure $ Left $ "Expected ResponseParseError, got: " ++ show other
    Right _ -> pure $ Left "Expected error for missing choices, got success"

-- | parseToolCall handles a well-formed tool call JSON value.
testOpenAIParseToolCall :: Test
testOpenAIParseToolCall = do
  let tcJson = object
        [ "id"   .= ("call_test" :: T.Text)
        , "type" .= ("function" :: T.Text)
        , "function" .= object
            [ "name"      .= ("read_file" :: T.Text)
            , "arguments" .= ("{\"path\":\"foo.hs\"}" :: T.Text)
            ]
        ]
  case parseToolCall tcJson of
    Left err -> pure $ Left $ "parseToolCall failed: " ++ show err
    Right tc
      | tcId tc == "call_test" && tcName tc == "read_file" -> pure $ Right ()
      | otherwise -> pure $ Left $ "parseToolCall mismatch: " ++ show tc

-- | parseToolCall fails cleanly on missing function name.
testOpenAIParseToolCallMissing :: Test
testOpenAIParseToolCallMissing = do
  let tcJson = object
        [ "id"   .= ("call_bad" :: T.Text)
        , "type" .= ("function" :: T.Text)
        , "function" .= object
            [ "arguments" .= ("{}" :: T.Text)
            ]
        ]
  case parseToolCall tcJson of
    Left (ResponseParseError _) -> pure $ Right ()
    Left other -> pure $ Left $ "Expected ResponseParseError, got: " ++ show other
    Right _ -> pure $ Left "Expected error for missing function name, got success"

-- | messageToJSON produces correct wire format for user messages.
testOpenAIMessageToJSON :: Test
testOpenAIMessageToJSON = do
  let msg = mkUserMessage "hello"
      val = messageToJSON msg
  case val of
    Object obj
      | KM.lookup (Key.fromText "role") obj == Just (String "user")
        && KM.lookup (Key.fromText "content") obj == Just (String "hello") ->
          pure $ Right ()
      | otherwise -> pure $ Left $ "messageToJSON mismatch: " ++ show obj
    _ -> pure $ Left "messageToJSON did not produce an object"

-- | messageToJSON produces "tool" role for tool-result messages.
testOpenAIToolResultToJSON :: Test
testOpenAIToolResultToJSON = do
  let msg = Message User "file contents" (Just "call_123") Nothing
      val = messageToJSON msg
  case val of
    Object obj
      | KM.lookup (Key.fromText "role") obj == Just (String "tool")
        && KM.lookup (Key.fromText "tool_call_id") obj == Just (String "call_123") ->
          pure $ Right ()
      | otherwise -> pure $ Left $ "Tool result JSON mismatch: " ++ show obj
    _ -> pure $ Left "messageToJSON did not produce an object for tool result"

-- | buildRequestBody omits tools and tool_choice when registry is empty.
testOpenAIRequestNoTools :: Test
testOpenAIRequestNoTools = do
  let msgs = [mkUserMessage "hello"]
      body = buildRequestBody "max_tokens" msgs "gpt-4o" 1024 mempty
  case decode body of
    Nothing -> pure $ Left "buildRequestBody (empty reg) produced invalid JSON"
    Just (Object obj) -> do
      let noTools      = KM.lookup (Key.fromText "tools") obj == Nothing
          noToolChoice = KM.lookup (Key.fromText "tool_choice") obj == Nothing
      if noTools && noToolChoice
        then pure $ Right ()
        else pure $ Left $ "Expected no tools/tool_choice, got: " ++ show obj
    Just other -> pure $ Left $ "Request JSON is not an object: " ++ show other

-- | toolsToJSON produces correct OpenAI format for each tool.
testOpenAIToolsSchema :: Test
testOpenAIToolsSchema = do
  let toolList = toolsToJSON defaultRegistry
  -- Each tool must be {"type":"function","function":{"name":...,"description":...,"parameters":...}}
  let checkTool val = case val of
        Object obj -> do
          let hasType = KM.lookup (Key.fromText "type") obj == Just (String "function")
          case KM.lookup (Key.fromText "function") obj of
            Just (Object fn) -> do
              let hasName = case KM.lookup (Key.fromText "name") fn of
                              Just (String _) -> True
                              _               -> False
                  hasDesc = case KM.lookup (Key.fromText "description") fn of
                              Just (String _) -> True
                              _               -> False
                  hasParams = case KM.lookup (Key.fromText "parameters") fn of
                                Just (Object _) -> True
                                _               -> False
              if hasType && hasName && hasDesc && hasParams
                then Right ()
                else Left $ "Tool schema fields missing: " ++ show val
            _ -> Left $ "Tool missing 'function' object: " ++ show val
        _ -> Left $ "Tool is not an object: " ++ show val
  case mapM_ checkTool toolList of
    Right () -> pure $ Right ()
    Left err -> pure $ Left err

-- | parseResponseBody handles content:null with tool_calls (explicit).
testOpenAIResponseNullContentToolCall :: Test
testOpenAIResponseNullContentToolCall = do
  let resp = encode $ object
        [ "id"      .= ("chatcmpl-null" :: T.Text)
        , "object"  .= ("chat.completion" :: T.Text)
        , "choices" .= [object
            [ "index" .= (0 :: Int)
            , "message" .= object
                [ "role"    .= ("assistant" :: T.Text)
                , "content" .= Null
                , "tool_calls" .= [object
                    [ "id"   .= ("call_null" :: T.Text)
                    , "type" .= ("function" :: T.Text)
                    , "function" .= object
                        [ "name"      .= ("list_files" :: T.Text)
                        , "arguments" .= ("{\"dir\":\".\"}" :: T.Text)
                        ]
                    ]]
                ]
            , "finish_reason" .= ("tool_calls" :: T.Text)
            ]]
        ]
  case parseResponseBody resp of
    Left err -> pure $ Left $ "Failed to parse null-content tool response: " ++ show err
    Right cr -> do
      let contentOk  = msgContent (crReply cr) == ""
          callIdOk   = case crToolCalls cr of
                         Just [tc] -> tcId tc == "call_null"
                         _         -> False
          nameOk     = case crToolCalls cr of
                         Just [tc] -> tcName tc == "list_files"
                         _         -> False
          argsOk     = case crToolCalls cr of
                         Just [tc] -> tcArgs tc == object ["dir" .= ("." :: T.Text)]
                         _         -> False
      if contentOk && callIdOk && nameOk && argsOk
        then pure $ Right ()
        else pure $ Left $ "Null-content parse mismatch: content="
                ++ show contentOk ++ " callId=" ++ show callIdOk
                ++ " name=" ++ show nameOk ++ " args=" ++ show argsOk

-- | buildSystemPrompt does NOT tell the model to print JSON tool calls.
testBuildSystemPromptNoJsonInstruction :: Test
testBuildSystemPromptNoJsonInstruction = do
  let prompt = buildSystemPrompt defaultRegistry
  -- The old prompt contained these strings; the new one must not.
  if T.isInfixOf "{\"tool_call\"" prompt
    then pure $ Left "System prompt still contains JSON tool_call instruction"
    else pure $ Right ()

-- | buildSystemPrompt tells the model to use the native tool mechanism.
testBuildSystemPromptNativeTools :: Test
testBuildSystemPromptNativeTools = do
  let prompt = buildSystemPrompt defaultRegistry
  if T.isInfixOf "tool-calling mechanism" prompt
     && T.isInfixOf "Available tools" prompt
    then pure $ Right ()
    else pure $ Left "System prompt missing native tool-calling guidance"

-- | tokenLimitFieldName returns "max_completion_tokens" for openai.
testTokenLimitFieldOpenAI :: Test
testTokenLimitFieldOpenAI =
  let pc = ProviderConfig "openai" "gpt-4o" "https://api.openai.com" ""
  in if tokenLimitFieldName pc == "max_completion_tokens"
       then pure $ Right ()
       else pure $ Left $ "Expected max_completion_tokens, got: "
                         ++ T.unpack (tokenLimitFieldName pc)

-- | tokenLimitFieldName returns "max_tokens" for ollama.
testTokenLimitFieldOllama :: Test
testTokenLimitFieldOllama =
  let pc = ProviderConfig "ollama" "llama3.1" "http://localhost:11434" ""
  in if tokenLimitFieldName pc == "max_tokens"
       then pure $ Right ()
       else pure $ Left $ "Expected max_tokens, got: "
                         ++ T.unpack (tokenLimitFieldName pc)

-- | tokenLimitFieldName returns "max_tokens" for vllm, litellm, openrouter.
testTokenLimitFieldOtherProviders :: Test
testTokenLimitFieldOtherProviders =
  let check name = tokenLimitFieldName (ProviderConfig name "m" "http://x" "") == "max_tokens"
      names = ["vllm", "litellm", "openrouter"]
  in if all check names
       then pure $ Right ()
       else pure $ Left $ "Some providers did not return max_tokens"

-- | buildRequestBody with "max_completion_tokens" uses that field name.
testOpenAIRequestMaxCompletionTokens :: Test
testOpenAIRequestMaxCompletionTokens = do
  let msgs = [mkUserMessage "hello"]
      body = buildRequestBody "max_completion_tokens" msgs "gpt-4o-mini" 2048 mempty
  case decode body of
    Nothing -> pure $ Left "buildRequestBody (max_completion_tokens) invalid JSON"
    Just (Object obj) -> do
      let hasField  = KM.lookup (Key.fromText "max_completion_tokens") obj == Just (Number 2048)
          noOldField = KM.lookup (Key.fromText "max_tokens") obj == Nothing
      if hasField && noOldField
        then pure $ Right ()
        else pure $ Left $ "max_completion_tokens=" ++ show hasField
                         ++ " no max_tokens=" ++ show noOldField
    Just other -> pure $ Left $ "Request JSON is not an object: " ++ show other

-- | buildRequestBody with "max_tokens" uses that field name (ollama/compatible).
testOpenAIRequestMaxTokensOllama :: Test
testOpenAIRequestMaxTokensOllama = do
  let msgs = [mkUserMessage "hello"]
      body = buildRequestBody "max_tokens" msgs "llama3.1" 4096 mempty
  case decode body of
    Nothing -> pure $ Left "buildRequestBody (max_tokens ollama) invalid JSON"
    Just (Object obj) -> do
      let hasField   = KM.lookup (Key.fromText "max_tokens") obj == Just (Number 4096)
          noNewField = KM.lookup (Key.fromText "max_completion_tokens") obj == Nothing
      if hasField && noNewField
        then pure $ Right ()
        else pure $ Left $ "max_tokens=" ++ show hasField
                         ++ " no max_completion_tokens=" ++ show noNewField
    Just other -> pure $ Left $ "Request JSON is not an object: " ++ show other

-- | An assistant message with tool_calls serializes with the tool_calls
--   array in OpenAI wire format (id, type=function, function.name,
--   function.arguments as a JSON string).
testOpenAIAssistantToolCallsJSON :: Test
testOpenAIAssistantToolCallsJSON = do
  let tcs = [ ToolCall "call_1" "read_file" (object ["path" .= ("foo.hs" :: T.Text)])
            , ToolCall "call_2" "list_files" (object ["dir" .= ("." :: T.Text)])
            ]
      msg = Message Assistant "Let me check." Nothing (Just tcs)
      val = messageToJSON msg
  case val of
    Object obj -> do
      let roleOk = KM.lookup (Key.fromText "role") obj == Just (String "assistant")
          contentOk = KM.lookup (Key.fromText "content") obj == Just (String "Let me check.")
      case KM.lookup (Key.fromText "tool_calls") obj of
        Just (Array arr) | length arr == 2 -> do
          -- Check first tool call structure
          case arr V.! 0 of
            Object tc1 -> do
              let idOk   = KM.lookup (Key.fromText "id") tc1 == Just (String "call_1")
                  typeOk = KM.lookup (Key.fromText "type") tc1 == Just (String "function")
              case KM.lookup (Key.fromText "function") tc1 of
                Just (Object fn) -> do
                  let nameOk = KM.lookup (Key.fromText "name") fn == Just (String "read_file")
                      argsOk = case KM.lookup (Key.fromText "arguments") fn of
                                 Just (String a) -> a == "{\"path\":\"foo.hs\"}"
                                 _               -> False
                  if roleOk && contentOk && idOk && typeOk && nameOk && argsOk
                    then pure $ Right ()
                    else pure $ Left $ "Field mismatch: role=" ++ show roleOk
                             ++ " content=" ++ show contentOk ++ " id=" ++ show idOk
                             ++ " type=" ++ show typeOk ++ " name=" ++ show nameOk
                             ++ " args=" ++ show argsOk
                _ -> pure $ Left "tool_call missing function object"
            _ -> pure $ Left "tool_call[0] is not an object"
        _ -> pure $ Left $ "tool_calls missing or wrong length: " ++ show (KM.lookup (Key.fromText "tool_calls") obj)
    _ -> pure $ Left "messageToJSON did not produce an object"

-- | A full conversation with assistant tool_calls followed by tool
--   results serializes in correct OpenAI order with matching IDs.
testOpenAIConversationToolCallRoundTrip :: Test
testOpenAIConversationToolCallRoundTrip = do
  let tcs = [ToolCall "call_x1" "list_files" (object ["dir" .= ("." :: T.Text)])]
      msgs =
        [ mkUserMessage "List the files in this repo"
        , Message Assistant "Let me check." Nothing (Just tcs)
        , Message User "src\napp\ntest\n" (Just "call_x1") Nothing
        ]
      vals = messagesToJSON msgs
  case vals of
    [userVal, asstVal, toolVal] -> do
      -- User message
      let userRoleOk = case userVal of
            Object o -> KM.lookup (Key.fromText "role") o == Just (String "user")
            _        -> False
      -- Assistant message must have tool_calls
      let asstOk = case asstVal of
            Object o -> do
              let roleOk = KM.lookup (Key.fromText "role") o == Just (String "assistant")
                  hasTcs = case KM.lookup (Key.fromText "tool_calls") o of
                             Just (Array arr) -> length arr == 1
                             _                -> False
              roleOk && hasTcs
            _ -> False
      -- Tool result must have role=tool and matching tool_call_id
      let toolOk = case toolVal of
            Object o -> do
              let roleOk   = KM.lookup (Key.fromText "role") o == Just (String "tool")
                  callIdOk = KM.lookup (Key.fromText "tool_call_id") o == Just (String "call_x1")
                  contentOk = KM.lookup (Key.fromText "content") o == Just (String "src\napp\ntest\n")
              roleOk && callIdOk && contentOk
            _ -> False
      if userRoleOk && asstOk && toolOk
        then pure $ Right ()
        else pure $ Left $ "user=" ++ show userRoleOk
                         ++ " asst=" ++ show asstOk
                         ++ " tool=" ++ show toolOk
    _ -> pure $ Left $ "Expected 3 messages, got " ++ show (length vals)

-- | The tool_call ID in the assistant message matches the tool_call_id
--   in the subsequent tool result message.
testOpenAIToolCallIdMatch :: Test
testOpenAIToolCallIdMatch = do
  let tcs = [ToolCall "call_match_42" "read_file" (object ["path" .= ("x.hs" :: T.Text)])]
      asstMsg = Message Assistant "" Nothing (Just tcs)
      toolMsg = Message User "file contents" (Just "call_match_42") Nothing
      asstVal = messageToJSON asstMsg
      toolVal = messageToJSON toolMsg
  -- Extract the tool_call id from assistant message
  let asstCallId = case asstVal of
        Object o -> case KM.lookup (Key.fromText "tool_calls") o of
          Just (Array arr) -> case arr V.! 0 of
            Object tc -> case KM.lookup (Key.fromText "id") tc of
              Just (String i) -> Just i
              _               -> Nothing
            _ -> Nothing
          _ -> Nothing
        _ -> Nothing
  -- Extract the tool_call_id from tool message
  let toolCallId = case toolVal of
        Object o -> case KM.lookup (Key.fromText "tool_call_id") o of
          Just (String i) -> Just i
          _               -> Nothing
        _ -> Nothing
  if asstCallId == Just "call_match_42" && toolCallId == Just "call_match_42" && asstCallId == toolCallId
    then pure $ Right ()
    else pure $ Left $ "ID mismatch: asst=" ++ show asstCallId ++ " tool=" ++ show toolCallId

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = runTests
  [ -- Original tests
    testMessageRoundtrip
  , testRoleJSON
  , testStubProvider
  , testDefaultConfig
  , testPolicyAllow
  , testPolicyDeny
  , testPolicyAsk
  , testToolRegistry
  , testSessionLog
  , testPatchDiff
    -- Phase 1 tests
  , testToolCallJSONParse
  , testUnknownToolDenied
  , testReadFileExecutes
  , testListFilesExecutes
  , testShellExecutes
  , testShellDangerousDenied
  , testShellSafeAskUser
  , testPolicyStructuredArgs
  , testExtractTextField
  , testAgentLoopEvents
  , testAgentLoopToolExecution
  , testBuildSystemPrompt
    -- Approval tests
  , testApprovalApproved
  , testApprovalRejected
  , testApprovalSessionEvents
  , testRejectionSessionEvents
  , testDangerousDeniedWithoutPrompting
    -- OpenAI provider tests
  , testOpenAIRequestShape
  , testOpenAIRequestTools
  , testOpenAIResponseText
  , testOpenAIResponseToolCall
  , testOpenAIResponseMultiToolCall
  , testOpenAIMalformedResponse
  , testOpenAIMissingChoices
  , testOpenAIParseToolCall
  , testOpenAIParseToolCallMissing
  , testOpenAIMessageToJSON
  , testOpenAIToolResultToJSON
  , testOpenAIRequestNoTools
  , testOpenAIToolsSchema
  , testOpenAIResponseNullContentToolCall
  , testBuildSystemPromptNoJsonInstruction
  , testBuildSystemPromptNativeTools
  , testTokenLimitFieldOpenAI
  , testTokenLimitFieldOllama
  , testTokenLimitFieldOtherProviders
  , testOpenAIRequestMaxCompletionTokens
  , testOpenAIRequestMaxTokensOllama
  , testOpenAIAssistantToolCallsJSON
  , testOpenAIConversationToolCallRoundTrip
  , testOpenAIToolCallIdMatch
  ]
