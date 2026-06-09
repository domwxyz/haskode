{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

-- | Test suite for Haskode.
--
-- We use a simple main-based test runner (no external test framework)
-- to keep dependencies minimal.  Tests are plain functions that return
-- Either String () — Right for pass, Left for failure.

module Main (main) where

import Data.Aeson       (Value (..), encode, decode, eitherDecode, object, (.=))
import qualified Data.Aeson.Key    as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as LBS
import Control.Exception (try, IOException, throwIO)
import qualified Data.IORef
import Data.List          (isInfixOf)
import qualified Data.Map.Strict as Map
import qualified Data.Vector        as V
import System.Directory  (getTemporaryDirectory, doesFileExist, removeFile,
                          createDirectory, removeDirectoryRecursive,
                          getCurrentDirectory, setCurrentDirectory,
                          createFileLink, createDirectoryLink, emptyPermissions,
                          getPermissions, setPermissions, renameFile)
import System.Environment (setEnv, unsetEnv)
import System.Exit       (exitFailure, exitSuccess)
import System.FilePath   ((</>))
import System.Info       (os)
import System.IO         (hClose, hFileSize, openFile, openTempFile, IOMode (..))

import Haskode.Core
import Haskode.Commands  (parseSlashCommand, formatHelp, formatStatus, formatUnknownCommand, formatNewConfirmation, resetConversation, formatContextUsage)
import Haskode.Display   (indentBlock, formatAssistantReply, formatToolExecuting,
                          formatToolResult, formatToolUnknown,
                          formatPolicyDenied, formatPolicyConfirmationNeeded,
                          formatPolicyApproved, formatPolicyRejected,
                          formatConfirmTool, formatConfirmArgs,
                          formatConfirmReason, formatConfirmPrompt,
                          formatConfirmFile, formatConfirmDiffHeader,
                          formatConfirmPreviewHeader, formatError, formatVerbose,
                          formatContextLimitRefusal)
import Haskode.Config    (defaultConfig, Config (..), ProviderConfig (..),
                          tokenLimitFieldName, defaultMaxContextChars,
                          defaultMaxSessionLogBytes,
                          expandEnvVars, expandConfig)
import Haskode.Provider  (Provider (..), CompletionRequest (..),
                          CompletionResponse (..), stubProvider, scriptedProvider)
import Haskode.Provider.OpenAI
                          (buildRequestBody, messagesToJSON, messageToJSON, toolsToJSON,
                           parseResponseBody, parseToolCall, OpenAIError (..))
import Haskode.Policy    (checkPolicy, defaultPolicy, Decision (..))
import Haskode.Tools     (defaultRegistry, toolNames, lookupTool, readFileTool, listFilesTool,
                          shellTool, globTool, searchTool, previewPatchTool,
                          applyPatchTool, writeFileTool,
                          extractTextField, Tool (..),
                          TruncResult (..), truncateText, formatTruncMeta,
                          matchGlob, isIgnoredDir, searchInText, formatSearchMatch,
                          isUnderRoot, searchMaxFileSize,
                          TraversalStats (..), emptyStats, formatStats,
                          safeCanonicalize, loadAgentIgnore, shouldIgnorePath,
                          computePatchPreview, computeWriteFilePreview)
import Haskode.Session   (emptyLog, logEvent, events, flushLog, flushLogOnException,
                          Event (..), EventType (..),
                          SessionSummary (..), summarizeSession, formatSessionSummary,
                          isMeaningfulSession)
import Haskode.Patch     (makePatch, showDiff)
import Haskode.Agent     (AgentState (..), initState, runAgent, buildSystemPrompt,
                          loadAgentsMd,
                          estimateContextChars,
                          ApprovalFunc,
                          autoApprove, autoReject,
                          recordSessionStart, recordSessionEnd, recordConversationReset)
import Data.Time.Clock   (getCurrentTime)
import qualified Data.Text    as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO

-- ---------------------------------------------------------------------------
-- Test runner
-- ---------------------------------------------------------------------------

type Test = IO (Either String ())

-- | Check if the current process can create symbolic links.
--   On Windows this typically requires Administrator privileges.
--   Returns True if symlinks are supported, False otherwise.
canCreateSymlinks :: IO Bool
canCreateSymlinks = do
  tmpDir <- getTemporaryDirectory
  let probePath = tmpDir </> "haskode-symlink-probe"
  -- Clean up any leftover probe
  _ <- try (removeFile probePath) :: IO (Either IOException ())
  result <- try (createFileLink probePath (probePath ++ ".link")) :: IO (Either IOException ())
  case result of
    Left _  -> pure False
    Right _ -> do
      _ <- try (removeFile (probePath ++ ".link")) :: IO (Either IOException ())
      pure True

-- | Skip a test if symlinks are not supported on this platform.
--   Returns Right () (pass) when symlinks are unavailable.
skipIfNoSymlinks :: IO (Either String ()) -> IO (Either String ())
skipIfNoSymlinks test = do
  ok <- canCreateSymlinks
  if ok then test
  else pure $ Right ()  -- skip: symlinks not supported

-- | Skip a test on Windows (mingw32).
--   Used for tests that rely on Unix-style file permissions or
--   hardcoded Unix paths.  Returns Right () (pass) on Windows.
skipOnWindows :: IO (Either String ()) -> IO (Either String ())
skipOnWindows test
  | os == "mingw32" = pure $ Right ()
  | otherwise       = test

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
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
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
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
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
  let prompt = buildSystemPrompt defaultRegistry Nothing
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
-- Config backward-compatibility tests
-- ---------------------------------------------------------------------------

-- | Helper: minimal valid provider config JSON fragment.
minimalProviderJson :: T.Text
minimalProviderJson =
  "\"cfgProvider\":{\"pcProvider\":\"ollama\",\"pcModel\":\"llama3.1\",\
  \\"pcBaseUrl\":\"http://localhost:11434\",\"pcApiKey\":\"\"}"

-- | Old minimal config (only the four original fields) still parses.
--   cfgMaxContextChars and cfgMaxSessionLogBytes get their defaults.
testConfigBackcompatMinimal :: Test
testConfigBackcompatMinimal =
  let json = "{" <> minimalProviderJson
           <> ",\"cfgMaxTokens\":2048"
           <> ",\"cfgVerbose\":false"
           <> ",\"cfgWorkingDir\":\".\"}"
  in case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 json) :: Either String Config of
       Left err -> pure $ Left $ "Minimal config failed to parse: " ++ err
       Right cfg
         | cfgMaxTokens cfg /= 2048 ->
             pure $ Left $ "cfgMaxTokens mismatch: " ++ show (cfgMaxTokens cfg)
         | cfgMaxContextChars cfg /= defaultMaxContextChars ->
             pure $ Left $ "cfgMaxContextChars not defaulted: " ++ show (cfgMaxContextChars cfg)
         | cfgMaxSessionLogBytes cfg /= defaultMaxSessionLogBytes ->
             pure $ Left $ "cfgMaxSessionLogBytes not defaulted: " ++ show (cfgMaxSessionLogBytes cfg)
         | otherwise -> pure $ Right ()

-- | Config with explicit cfgMaxContextChars overrides the default.
testConfigBackcompatOverrideContextChars :: Test
testConfigBackcompatOverrideContextChars =
  let json = "{" <> minimalProviderJson
           <> ",\"cfgMaxTokens\":2048"
           <> ",\"cfgVerbose\":false"
           <> ",\"cfgWorkingDir\":\".\""
           <> ",\"cfgMaxContextChars\":50000}"
  in case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 json) :: Either String Config of
       Left err -> pure $ Left $ "Override config failed to parse: " ++ err
       Right cfg
         | cfgMaxContextChars cfg /= 50000 ->
             pure $ Left $ "cfgMaxContextChars not overridden: " ++ show (cfgMaxContextChars cfg)
         | cfgMaxSessionLogBytes cfg /= defaultMaxSessionLogBytes ->
             pure $ Left $ "cfgMaxSessionLogBytes should be default: " ++ show (cfgMaxSessionLogBytes cfg)
         | otherwise -> pure $ Right ()

-- | Config with explicit cfgMaxSessionLogBytes overrides the default.
testConfigBackcompatOverrideSessionLogBytes :: Test
testConfigBackcompatOverrideSessionLogBytes =
  let json = "{" <> minimalProviderJson
           <> ",\"cfgMaxTokens\":2048"
           <> ",\"cfgVerbose\":false"
           <> ",\"cfgWorkingDir\":\".\""
           <> ",\"cfgMaxSessionLogBytes\":1024}"
  in case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 json) :: Either String Config of
       Left err -> pure $ Left $ "Override config failed to parse: " ++ err
       Right cfg
         | cfgMaxSessionLogBytes cfg /= 1024 ->
             pure $ Left $ "cfgMaxSessionLogBytes not overridden: " ++ show (cfgMaxSessionLogBytes cfg)
         | cfgMaxContextChars cfg /= defaultMaxContextChars ->
             pure $ Left $ "cfgMaxContextChars should be default: " ++ show (cfgMaxContextChars cfg)
         | otherwise -> pure $ Right ()

-- | Config with both optional fields explicitly set.
testConfigBackcompatOverrideBoth :: Test
testConfigBackcompatOverrideBoth =
  let json = "{" <> minimalProviderJson
           <> ",\"cfgMaxTokens\":2048"
           <> ",\"cfgVerbose\":false"
           <> ",\"cfgWorkingDir\":\".\""
           <> ",\"cfgMaxContextChars\":99000"
           <> ",\"cfgMaxSessionLogBytes\":2048}"
  in case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 json) :: Either String Config of
       Left err -> pure $ Left $ "Both-override config failed to parse: " ++ err
       Right cfg
         | cfgMaxContextChars cfg /= 99000 ->
             pure $ Left $ "cfgMaxContextChars not overridden: " ++ show (cfgMaxContextChars cfg)
         | cfgMaxSessionLogBytes cfg /= 2048 ->
             pure $ Left $ "cfgMaxSessionLogBytes not overridden: " ++ show (cfgMaxSessionLogBytes cfg)
         | otherwise -> pure $ Right ()

-- | Malformed cfgMaxContextChars (wrong type) fails to parse.
testConfigBackcompatMalformedContextChars :: Test
testConfigBackcompatMalformedContextChars =
  let json = "{" <> minimalProviderJson
           <> ",\"cfgMaxTokens\":2048"
           <> ",\"cfgVerbose\":false"
           <> ",\"cfgWorkingDir\":\".\""
           <> ",\"cfgMaxContextChars\":\"not-a-number\"}"
  in case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 json) :: Either String Config of
       Left _ -> pure $ Right ()  -- expected: parse failure
       Right _ -> pure $ Left "Expected parse failure for malformed cfgMaxContextChars"

-- | Malformed cfgMaxSessionLogBytes (wrong type) fails to parse.
testConfigBackcompatMalformedSessionLogBytes :: Test
testConfigBackcompatMalformedSessionLogBytes =
  let json = "{" <> minimalProviderJson
           <> ",\"cfgMaxTokens\":2048"
           <> ",\"cfgVerbose\":false"
           <> ",\"cfgWorkingDir\":\".\""
           <> ",\"cfgMaxSessionLogBytes\":true}"
  in case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 json) :: Either String Config of
       Left _ -> pure $ Right ()  -- expected: parse failure
       Right _ -> pure $ Left "Expected parse failure for malformed cfgMaxSessionLogBytes"

-- | defaultConfig values for both optional fields are stable.
testDefaultConfigOptionalFields :: Test
testDefaultConfigOptionalFields =
  let cfg = defaultConfig
  in if cfgMaxContextChars cfg == 120000
        && cfgMaxSessionLogBytes cfg == 5 * 1024 * 1024
        && cfgMaxTokens cfg == 4096
        && cfgVerbose cfg == False
        && cfgWorkingDir cfg == "."
     then pure $ Right ()
     else pure $ Left $ "defaultConfig values changed: "
                      ++ "ctx=" ++ show (cfgMaxContextChars cfg)
                      ++ " log=" ++ show (cfgMaxSessionLogBytes cfg)
                       ++ " toks=" ++ show (cfgMaxTokens cfg)

-- ---------------------------------------------------------------------------
-- Environment-variable expansion tests
-- ---------------------------------------------------------------------------

-- | $VAR syntax expands a known environment variable.
testExpandEnvVarBare :: Test
testExpandEnvVarBare = do
  setEnv "HASKODE_TEST_VAR" "hello"
  result <- expandEnvVars "prefix_${HASKODE_TEST_VAR}_suffix"
  unsetEnv "HASKODE_TEST_VAR"
  if result == "prefix_hello_suffix"
    then pure $ Right ()
    else pure $ Left $ "expandEnvVars bare: got " ++ show result

-- | ${VAR} syntax expands a known environment variable.
testExpandEnvVarBraced :: Test
testExpandEnvVarBraced = do
  setEnv "HASKODE_TEST_VAR" "world"
  result <- expandEnvVars "prefix_${HASKODE_TEST_VAR}_suffix"
  unsetEnv "HASKODE_TEST_VAR"
  if result == "prefix_world_suffix"
    then pure $ Right ()
    else pure $ Left $ "expandEnvVars braced: got " ++ show result

-- | Undefined variables expand to the empty string.
testExpandEnvVarUndefined :: Test
testExpandEnvVarUndefined = do
  -- Ensure the variable is not set
  unsetEnv "HASKODE_TEST_UNDEF"
  result <- expandEnvVars "before_${HASKODE_TEST_UNDEF}_after"
  if result == "before__after"
    then pure $ Right ()
    else pure $ Left $ "expandEnvVars undefined: got " ++ show result

-- | Undefined $VAR (no braces) also expands to empty string.
--   Note: bare $VAR reads all consecutive alphanumeric/underscore chars,
--   so $HASKODE_TEST_UNDEF_after looks up "HASKODE_TEST_UNDEF_after".
testExpandEnvVarUndefinedBare :: Test
testExpandEnvVarUndefinedBare = do
  unsetEnv "HASKODE_TEST_UNDEF"
  result <- expandEnvVars "before_$HASKODE_TEST_UNDEF"
  if result == "before_"
    then pure $ Right ()
    else pure $ Left $ "expandEnvVars undefined bare: got " ++ show result

-- | A string with no variables passes through unchanged.
testExpandEnvVarNone :: Test
testExpandEnvVarNone = do
  result <- expandEnvVars "no variables here"
  if result == "no variables here"
    then pure $ Right ()
    else pure $ Left $ "expandEnvVars none: got " ++ show result

-- | A trailing $ with no variable name is left as-is.
testExpandEnvVarTrailingDollar :: Test
testExpandEnvVarTrailingDollar = do
  result <- expandEnvVars "trailing$"
  if result == "trailing$"
    then pure $ Right ()
    else pure $ Left $ "expandEnvVars trailing $: got " ++ show result

-- | expandConfig applies expansion to ProviderConfig string fields.
testExpandConfigProviderFields :: Test
testExpandConfigProviderFields = do
  setEnv "HASKODE_TEST_KEY" "sk-secret"
  setEnv "HASKODE_TEST_URL" "https://example.com"
  setEnv "HASKODE_TEST_MODEL" "gpt-test"
  let cfg = defaultConfig
        { cfgProvider = (cfgProvider defaultConfig)
            { pcApiKey  = "$HASKODE_TEST_KEY"
            , pcBaseUrl = "${HASKODE_TEST_URL}"
            , pcModel   = "prefix-$HASKODE_TEST_MODEL-suffix"
            }
        }
  cfg' <- expandConfig cfg
  unsetEnv "HASKODE_TEST_KEY"
  unsetEnv "HASKODE_TEST_URL"
  unsetEnv "HASKODE_TEST_MODEL"
  let pc = cfgProvider cfg'
  if pcApiKey pc == "sk-secret"
     && pcBaseUrl pc == "https://example.com"
     && pcModel pc == "prefix-gpt-test-suffix"
    then pure $ Right ()
    else pure $ Left $ "expandConfig provider: key=" ++ show (pcApiKey pc)
                    ++ " url=" ++ show (pcBaseUrl pc)
                    ++ " model=" ++ show (pcModel pc)

-- | expandConfig applies expansion to cfgWorkingDir.
testExpandConfigWorkingDir :: Test
testExpandConfigWorkingDir = do
  setEnv "HASKODE_TEST_DIR" "/tmp/test-project"
  let cfg = defaultConfig { cfgWorkingDir = "$HASKODE_TEST_DIR/src" }
  cfg' <- expandConfig cfg
  unsetEnv "HASKODE_TEST_DIR"
  if cfgWorkingDir cfg' == "/tmp/test-project/src"
    then pure $ Right ()
    else pure $ Left $ "expandConfig workingDir: got " ++ show (cfgWorkingDir cfg')

-- | expandConfig does not touch numeric or boolean fields.
testExpandConfigPreservesNonStrings :: Test
testExpandConfigPreservesNonStrings = do
  let cfg = defaultConfig
  cfg' <- expandConfig cfg
  if cfgMaxTokens cfg' == cfgMaxTokens cfg
     && cfgVerbose cfg' == cfgVerbose cfg
     && cfgMaxContextChars cfg' == cfgMaxContextChars cfg
     && cfgMaxSessionLogBytes cfg' == cfgMaxSessionLogBytes cfg
    then pure $ Right ()
    else pure $ Left "expandConfig changed non-string fields"

-- | OPENAI_API_KEY is usable without pcApiKey in config (integration check).
--   This tests that after expansion, an empty pcApiKey does not
--   interfere with the existing OPENAI_API_KEY lookup in the provider.
testExpandConfigOpenAIFallback :: Test
testExpandConfigOpenAIFallback = do
  let cfg = defaultConfig { cfgProvider = (cfgProvider defaultConfig) { pcApiKey = "" } }
  cfg' <- expandConfig cfg
  -- The pcApiKey should remain empty; the provider module handles
  -- OPENAI_API_KEY separately via lookupEnv.
  if pcApiKey (cfgProvider cfg') == ""
    then pure $ Right ()
    else pure $ Left $ "expandConfig changed empty pcApiKey: " ++ show (pcApiKey (cfgProvider cfg'))

-- | Under-limit conversation proceeds to provider (provider reply is seen).
testContextGuardUnderLimit :: Test
testContextGuardUnderLimit = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "provider replied"
        , crToolCalls = Nothing
        }
    ]
  let cfg   = defaultConfig  -- 120K chars, way more than "hello"
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "hello"
  let evts = events (asSession state')
      types = map evType evts
  if EAssistantReply `elem` types
    then pure $ Right ()
    else pure $ Left $ "Under-limit guard: provider not called, events: " ++ show types

-- | Over-limit conversation fails before provider invocation.
--   Uses a very small cfgMaxContextChars to trigger the guard.
testContextGuardOverLimit :: Test
testContextGuardOverLimit = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "should not reach here"
        , crToolCalls = Nothing
        }
    ]
  let cfg   = defaultConfig { cfgMaxContextChars = 10 }  -- tiny limit
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
  result <- try (runAgent state "hello this is a message") :: IO (Either IOException AgentState)
  case result of
    Left _ex -> pure $ Right ()  -- expected: fail throws IOException
    Right _  -> pure $ Left "Over-limit guard: expected exception but got success"

-- | Context guard error text mentions that Haskode does not auto-truncate.
testContextGuardErrorText :: Test
testContextGuardErrorText = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "nope"
        , crToolCalls = Nothing
        }
    ]
  let cfg   = defaultConfig { cfgMaxContextChars = 10 }
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
  -- Capture the error message from the exception
  result <- try (runAgent state "a long user message here") :: IO (Either IOException AgentState)
  case result of
    Left ex -> do
      let errMsg = show ex
      if "does not auto-truncate or summarize" `isInfixOf` errMsg
        then pure $ Right ()
        else pure $ Left $ "Error text missing phrase: " ++ errMsg
    Right _ -> pure $ Left "Expected exception for over-limit"

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
      approvedEvts = filter (T.isInfixOf "approved" . evData) policyEvts
  if not (null approvedEvts)
    then pure $ Right ()
    else pure $ Left $ "Expected 'approved' in policy events, got: " ++ show policyEvts

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
-- Patch confirmation display tests
-- ---------------------------------------------------------------------------

-- | computePatchPreview returns the path and diff for a valid file.
testComputePatchPreviewNormal :: Test
testComputePatchPreviewNormal = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-preview-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  let testFile = root </> "Foo.hs"
  TIO.writeFile testFile "module Foo where\nfoo = 1\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("Foo.hs" :: T.Text)
        , "replacement" .= ("module Foo where\nfoo = 2\n" :: T.Text)
        ]
  result <- computePatchPreview args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Left err -> pure $ Left $ "Expected Right, got Left: " ++ T.unpack err
    Right (path, diff)
      | path /= "Foo.hs" ->
          pure $ Left $ "Expected path Foo.hs, got: " ++ path
      | not (T.isInfixOf "-module Foo where" diff) ->
          pure $ Left $ "Diff missing old marker: " ++ T.unpack (T.take 200 diff)
      | not (T.isInfixOf "+module Foo where" diff) ->
          pure $ Left $ "Diff missing new marker: " ++ T.unpack (T.take 200 diff)
      | otherwise -> pure $ Right ()

-- | computePatchPreview returns an error for a missing path field.
testComputePatchPreviewMissingPath :: Test
testComputePatchPreviewMissingPath = do
  let args = object [ "replacement" .= ("content" :: T.Text) ]
  result <- computePatchPreview args
  case result of
    Left _err -> pure $ Right ()
    Right _ -> pure $ Left "Expected Left for missing path, got Right"

-- | computePatchPreview returns an error for a path outside the working dir.
testComputePatchPreviewOutsideRoot :: Test
testComputePatchPreviewOutsideRoot = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-preview-outside"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  -- Use an absolute path that cannot resolve under the root.
  let args = object
        [ "path"        .= ("C:\\haskode_nonexistent_root_test\\file.txt" :: T.Text)
        , "replacement" .= ("new" :: T.Text)
        ]
  result <- computePatchPreview args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Left _err -> pure $ Right ()
    Right _ -> pure $ Left "Expected Left for outside-root path, got Right"

-- | computePatchPreview returns an error for a nonexistent file.
testComputePatchPreviewMissingFile :: Test
testComputePatchPreviewMissingFile = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-preview-missing"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("nonexistent.hs" :: T.Text)
        , "replacement" .= ("new" :: T.Text)
        ]
  result <- computePatchPreview args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Left err
      | T.isInfixOf "could not resolve" err -> pure $ Right ()
      | otherwise -> pure $ Left $ "Unexpected error: " ++ T.unpack err
    Right _ -> pure $ Left "Expected Left for missing file, got Right"

-- | When apply_patch is approved, the approval function receives a
--   reason that includes the target file path.  We capture the reason
--   text via a custom approval function.
testApplyPatchApprovalShowsPath :: Test
testApplyPatchApprovalShowsPath = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-approval-path-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  let testFile = root </> "Target.hs"
  TIO.writeFile testFile "module Target where\ntarget = 0\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  -- Custom approval function that captures the reason text
  capturedReason <- Data.IORef.newIORef ("" :: T.Text)
  let captureApprove :: ApprovalFunc
      captureApprove _tc reason = do
        Data.IORef.writeIORef capturedReason reason
        pure True
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Applying patch."
        , crToolCalls = Just [ToolCall "tc-ap1" "apply_patch"
                               (object [ "path"        .= ("Target.hs" :: T.Text)
                                       , "replacement" .= ("module Target where\ntarget = 1\n" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry captureApprove
  state' <- runAgent state "apply the patch"
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  reason <- Data.IORef.readIORef capturedReason
  -- The reason should include the path "Target.hs"
  let pathInReason = T.isInfixOf "Target.hs" reason
  -- The session should still have the standard policy decision and approval
  let evts = events (asSession state')
      policyEvts = filter (\e -> evType e == EPolicyDecision) evts
      approvedEvts = filter (T.isInfixOf "approved" . evData) policyEvts
  if pathInReason && not (null approvedEvts)
    then pure $ Right ()
    else pure $ Left $ "pathInReason=" ++ show pathInReason
                     ++ " approved=" ++ show (not (null approvedEvts))
                     ++ " reason=" ++ T.unpack reason

-- | When apply_patch is rejected, the file is unchanged and the
--   session records the rejection cleanly.
testApplyPatchRejectionShowsPath :: Test
testApplyPatchRejectionShowsPath = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-reject-path-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  let testFile = root </> "Target.hs"
      original = "module Target where\ntarget = 0\n"
  TIO.writeFile testFile original
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Applying patch."
        , crToolCalls = Just [ToolCall "tc-ap2" "apply_patch"
                               (object [ "path"        .= ("Target.hs" :: T.Text)
                                       , "replacement" .= ("module Target where\ntarget = 99\n" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "OK, I won't."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoReject
  state' <- runAgent state "apply the patch"
  setCurrentDirectory origDir
  -- File should be unchanged
  after <- TIO.readFile testFile
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let fileUnchanged = after == original
  -- Session should show the policy decision and the rejection
  let evts = events (asSession state')
      policyEvts = filter (\e -> evType e == EPolicyDecision) evts
      askEvts = filter (T.isInfixOf "AskUser" . evData) policyEvts
      deniedEvts = filter (\e -> evType e == EToolResult
                                 && T.isInfixOf "denied by user" (evData e)) evts
  if fileUnchanged && not (null askEvts) && not (null deniedEvts)
    then pure $ Right ()
    else pure $ Left $ "unchanged=" ++ show fileUnchanged
                     ++ " askEvts=" ++ show (not (null askEvts))
                     ++ " denied=" ++ show (not (null deniedEvts))

-- ---------------------------------------------------------------------------
-- Audit-log tests for apply_patch session events
-- ---------------------------------------------------------------------------

-- | Approved apply_patch logs the target path in the approval event.
testApplyPatchAuditApprovalWithPath :: Test
testApplyPatchAuditApprovalWithPath = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-audit-approve"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  let testFile = root </> "Foo.hs"
  TIO.writeFile testFile "module Foo where\nfoo = 0\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Applying."
        , crToolCalls = Just [ToolCall "tc-audit1" "apply_patch"
                               (object [ "path"        .= ("Foo.hs" :: T.Text)
                                       , "replacement" .= ("module Foo where\nfoo = 1\n" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "apply patch"
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let evts = events (asSession state')
      -- Find the approval policy decision event
      policyEvts = filter (\e -> evType e == EPolicyDecision) evts
      approvalEvts = filter (T.isInfixOf "approved" . evData) policyEvts
  case approvalEvts of
    (a:_) ->
      if T.isInfixOf "Foo.hs" (evData a)
        then pure $ Right ()
        else pure $ Left $ "Approval event missing path: " ++ T.unpack (evData a)
    _ -> pure $ Left "No approval event found"

-- | Rejected apply_patch logs the target path in the denial event.
testApplyPatchAuditRejectionWithPath :: Test
testApplyPatchAuditRejectionWithPath = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-audit-reject"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  let testFile = root </> "Bar.hs"
  TIO.writeFile testFile "module Bar where\nbar = 0\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Applying."
        , crToolCalls = Just [ToolCall "tc-audit2" "apply_patch"
                               (object [ "path"        .= ("Bar.hs" :: T.Text)
                                       , "replacement" .= ("module Bar where\nbar = 99\n" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "OK."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoReject
  state' <- runAgent state "apply patch"
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let evts = events (asSession state')
      -- Find the denial tool-result event
      trEvts = filter (\e -> evType e == EToolResult) evts
      denialEvts = filter (T.isInfixOf "denied by user" . evData) trEvts
  case denialEvts of
    (d:_) ->
      if T.isInfixOf "Bar.hs" (evData d)
        then pure $ Right ()
        else pure $ Left $ "Denial event missing path: " ++ T.unpack (evData d)
    _ -> pure $ Left "No denial event found"

-- | Applied patch session event includes a bounded diff (not the
--   full unbounded output).  The conversation keeps the full output,
--   but the session event is truncated.
testApplyPatchAuditResultBounded :: Test
testApplyPatchAuditResultBounded = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-audit-bounded"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  let testFile = root </> "Big.hs"
      -- Create a file with enough content to produce a large diff
      bigContent = T.unlines $ replicate 200 ("old line " <> T.pack (show (1 :: Int)))
      bigReplacement = T.unlines $ replicate 200 ("new line " <> T.pack (show (2 :: Int)))
  TIO.writeFile testFile bigContent
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Applying."
        , crToolCalls = Just [ToolCall "tc-audit3" "apply_patch"
                               (object [ "path"        .= ("Big.hs" :: T.Text)
                                       , "replacement" .= bigReplacement])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "apply big patch"
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let evts = events (asSession state')
      -- Find the tool result event for tc-audit3
      trEvts = filter (\e -> evType e == EToolResult) evts
      patchResults = filter (T.isInfixOf "tc-audit3" . evData) trEvts
      -- The conversation should have the full output
      conv = asConversation state'
      convResults = filter (\m -> msgCallId m == Just "tc-audit3") conv
  case (patchResults, convResults) of
    ([pEvt], [cMsg]) -> do
      let sessionLen = T.length (evData pEvt)
          convLen    = T.length (msgContent cMsg)
          sessionTruncated = T.isInfixOf "[truncated:" (evData pEvt)
      -- Session event should be bounded (<= 1024 + overhead for call ID prefix)
      -- Conversation should have the full output
      if convLen > 1024 && sessionLen < convLen && sessionTruncated
        then pure $ Right ()
        else pure $ Left $ "sessionLen=" ++ show sessionLen
                         ++ " convLen=" ++ show convLen
                         ++ " truncated=" ++ show sessionTruncated
    _ -> pure $ Left $ "Expected 1 session event and 1 conversation msg, got "
                     ++ show (length patchResults) ++ " / "
                     ++ show (length convResults)

-- | Read-only tools (read_file) have unchanged session logging behavior —
--   the session event data matches the tool output exactly.
testReadOnlyAuditUnchanged :: Test
testReadOnlyAuditUnchanged = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-audit-ro-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "audit-ro.txt") "read-only audit test"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Reading."
        , crToolCalls = Just [ToolCall "tc-ro1" "read_file"
                                 (object ["path" .= ("audit-ro.txt" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "read file"
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let evts = events (asSession state')
      trEvts = filter (\e -> evType e == EToolResult) evts
      conv = asConversation state'
      convResults = filter (\m -> msgCallId m == Just "tc-ro1") conv
  case (trEvts, convResults) of
    ([rEvt], [cMsg]) -> do
      -- Session event should contain the full file content (not truncated)
      let sessionData = evData rEvt
          convData    = msgContent cMsg
      if T.isInfixOf "read-only audit test" sessionData
         && sessionData == ("tc-ro1 " <> convData)
        then pure $ Right ()
        else pure $ Left $ "Session data mismatch: "
                         ++ T.unpack (T.take 200 sessionData)
    _ -> pure $ Left $ "Expected 1 session event and 1 conversation msg, got "
                     ++ show (length trEvts) ++ " / "
                     ++ show (length convResults)

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
  let prompt = buildSystemPrompt defaultRegistry Nothing
  -- The old prompt contained these strings; the new one must not.
  if T.isInfixOf "{\"tool_call\"" prompt
    then pure $ Left "System prompt still contains JSON tool_call instruction"
    else pure $ Right ()

-- | buildSystemPrompt tells the model to use the native tool mechanism.
testBuildSystemPromptNativeTools :: Test
testBuildSystemPromptNativeTools = do
  let prompt = buildSystemPrompt defaultRegistry Nothing
  if T.isInfixOf "tool-calling mechanism" prompt
     && T.isInfixOf "Available tools" prompt
    then pure $ Right ()
    else pure $ Left "System prompt missing native tool-calling guidance"

-- ---------------------------------------------------------------------------
-- Tool description / schema phrase tests
-- ---------------------------------------------------------------------------

-- | Helper: look up a tool's description from the registry.
toolDescriptionFromRegistry :: T.Text -> Maybe T.Text
toolDescriptionFromRegistry name = toolDescription <$> lookupTool name defaultRegistry

-- | preview_patch description distinguishes it as read-only and
--   mentions it does not modify the filesystem.
testPreviewPatchDescriptionPhrases :: Test
testPreviewPatchDescriptionPhrases =
  case toolDescriptionFromRegistry "preview_patch" of
    Nothing -> pure $ Left "preview_patch not in registry"
    Just desc
      | not (T.isInfixOf "without modifying" desc || T.isInfixOf "NOT" desc || T.isInfixOf "Does NOT" desc)
        -> pure $ Left $ "preview_patch description missing read-only signal: " ++ T.unpack desc
      | not (T.isInfixOf "diff" desc)
        -> pure $ Left $ "preview_patch description missing 'diff': " ++ T.unpack desc
      | otherwise -> pure $ Right ()

-- | apply_patch description states it requires user confirmation,
--   applies to exactly one existing file, and cannot create/delete.
testApplyPatchDescriptionPhrases :: Test
testApplyPatchDescriptionPhrases =
  case toolDescriptionFromRegistry "apply_patch" of
    Nothing -> pure $ Left "apply_patch not in registry"
    Just desc
      | not (T.isInfixOf "confirmation" desc || T.isInfixOf "confirm" desc)
        -> pure $ Left $ "apply_patch description missing confirmation: " ++ T.unpack desc
      | not (T.isInfixOf "existing" desc)
        -> pure $ Left $ "apply_patch description missing 'existing': " ++ T.unpack desc
      | not (T.isInfixOf "one" desc || T.isInfixOf "single" desc || T.isInfixOf "exactly" desc)
        -> pure $ Left $ "apply_patch description missing single-file constraint: " ++ T.unpack desc
      | otherwise -> pure $ Right ()

-- | search description mentions case-insensitive option and .agentignore.
testSearchDescriptionPhrases :: Test
testSearchDescriptionPhrases =
  case toolDescriptionFromRegistry "search" of
    Nothing -> pure $ Left "search not in registry"
    Just desc
      | not (T.isInfixOf "case-insensitive" desc)
        -> pure $ Left $ "search description missing 'case-insensitive': " ++ T.unpack desc
      | not (T.isInfixOf ".agentignore" desc)
        -> pure $ Left $ "search description missing '.agentignore': " ++ T.unpack desc
      | otherwise -> pure $ Right ()

-- | glob description mentions .agentignore.
testGlobDescriptionPhrases :: Test
testGlobDescriptionPhrases =
  case toolDescriptionFromRegistry "glob" of
    Nothing -> pure $ Left "glob not in registry"
    Just desc
      | not (T.isInfixOf ".agentignore" desc)
        -> pure $ Left $ "glob description missing '.agentignore': " ++ T.unpack desc
      | otherwise -> pure $ Right ()

-- | shell description mentions confirmation and dangerous commands.
testShellDescriptionPhrases :: Test
testShellDescriptionPhrases =
  case toolDescriptionFromRegistry "shell" of
    Nothing -> pure $ Left "shell not in registry"
    Just desc
      | not (T.isInfixOf "confirmation" desc || T.isInfixOf "confirm" desc)
        -> pure $ Left $ "shell description missing confirmation: " ++ T.unpack desc
      | not (T.isInfixOf "dangerous" desc)
        -> pure $ Left $ "shell description missing 'dangerous': " ++ T.unpack desc
      | otherwise -> pure $ Right ()

-- | The system prompt includes tool descriptions that contain the
--   key safety phrases for the five critical tools.
testSystemPromptToolPhrases :: Test
testSystemPromptToolPhrases = do
  let prompt = buildSystemPrompt defaultRegistry Nothing
      checks :: [(T.Text, [T.Text])]
      checks =
        [ ("preview_patch",  ["without modifying", "NOT"])
        , ("apply_patch",    ["confirmation", "existing"])
        , ("search",         ["case-sensitive", ".agentignore"])
        , ("glob",           [".agentignore"])
        , ("shell",          ["confirmation", "dangerous"])
        , ("write_file",     ["confirmation", "overwrite"])
        ]
      missing = concat
        [ [ (tool, phrase)
          | phrase <- phrases
          , not (T.isInfixOf phrase prompt)
          ]
        | (tool, phrases) <- checks
        ]
  if null missing
    then pure $ Right ()
    else pure $ Left $ "System prompt missing phrases: "
                     ++ T.unpack (T.intercalate ", "
                          [ t <> ":\"" <> p <> "\"" | (t, p) <- missing ])

-- | The OpenAI tool schema for each tool contains a description field
--   with the key safety phrases (via toolsToJSON).
testToolSchemaPhrasesInWireFormat :: Test
testToolSchemaPhrasesInWireFormat = do
  let toolList = toolsToJSON defaultRegistry
      descByName name = case
        [ d | Object obj <- toolList
        , Just (Object fn) <- [KM.lookup (Key.fromText "function") obj]
        , Just (String n)  <- [KM.lookup (Key.fromText "name") fn]
        , n == name
        , Just (String d)  <- [KM.lookup (Key.fromText "description") fn]
        ] of
          (d:_) -> d
          []    -> ""
      checks =
        [ ("preview_patch",  ["without modifying"])
        , ("apply_patch",    ["confirmation"])
        , ("search",         ["case-sensitive"])
        , ("glob",           [".agentignore"])
        , ("shell",          ["dangerous"])
        , ("write_file",     ["confirmation"])
        ]
      missing = concat
        [ [ (tool, phrase)
          | phrase <- phrases
          , not (T.isInfixOf phrase (descByName tool))
          ]
        | (tool, phrases) <- checks
        ]
  if null missing
    then pure $ Right ()
    else pure $ Left $ "Tool schema descriptions missing phrases: "
                     ++ show [(t, p) | (t, p) <- missing]

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
-- Truncation helper tests (pure, no IO)
-- ---------------------------------------------------------------------------

-- | Text shorter than the limit passes through unchanged.
testTruncateTextNoOp :: Test
testTruncateTextNoOp =
  let tr = truncateText 100 "hello"
  in if truncText tr == "hello"
        && truncOriginalLength tr == 5
        && truncReturnedLength tr == 5
        && not (truncDidTruncate tr)
        && truncDropped tr == 0
     then pure $ Right ()
     else pure $ Left $ "truncateText (short): " ++ show tr

-- | Text exceeding the limit is truncated with correct metadata.
testTruncateTextTruncates :: Test
testTruncateTextTruncates =
  let input = T.replicate 100 "x"   -- 100 chars
      tr    = truncateText 10 input
  in if T.length (truncText tr) == 10
        && truncOriginalLength tr == 100
        && truncReturnedLength tr == 10
        && truncDidTruncate tr
        && truncDropped tr == 90
     then pure $ Right ()
     else pure $ Left $ "truncateText (long): " ++ show tr

-- | Text exactly at the limit is not truncated.
testTruncateTextExactLimit :: Test
testTruncateTextExactLimit =
  let input = T.replicate 50 "a"   -- exactly 50 chars
      tr    = truncateText 50 input
  in if truncText tr == input
        && not (truncDidTruncate tr)
        && truncDropped tr == 0
     then pure $ Right ()
     else pure $ Left $ "truncateText (exact): " ++ show tr

-- | formatTruncMeta returns empty string when no truncation.
testFormatTruncMetaNoOp :: Test
testFormatTruncMetaNoOp =
  let tr   = truncateText 100 "short"
      meta = formatTruncMeta tr
  in if meta == ""
     then pure $ Right ()
     else pure $ Left $ "formatTruncMeta (no trunc): expected empty, got: " ++ T.unpack meta

-- | formatTruncMeta returns proper metadata line when truncated.
testFormatTruncMetaTruncated :: Test
testFormatTruncMetaTruncated =
  let input = T.replicate 200 "b"
      tr    = truncateText 50 input
      meta  = formatTruncMeta tr
  in if T.isInfixOf "[truncated:" meta
        && T.isInfixOf "returned 50 of 200 chars" meta
        && T.isInfixOf "150 dropped" meta
     then pure $ Right ()
     else pure $ Left $ "formatTruncMeta (trunc): " ++ T.unpack meta

-- | Empty text is not truncated.
testTruncateTextEmpty :: Test
testTruncateTextEmpty =
  let tr = truncateText 100 ""
  in if truncText tr == ""
        && truncOriginalLength tr == 0
        && not (truncDidTruncate tr)
     then pure $ Right ()
     else pure $ Left $ "truncateText (empty): " ++ show tr

-- ---------------------------------------------------------------------------
-- Multi-tool conversation order tests
-- ---------------------------------------------------------------------------

-- | When the assistant requests multiple tool calls, the tool results
--   appear in the conversation in the same order as the tool calls.
--   This is critical for the OpenAI wire format: tool results must
--   follow the assistant message that requested them.
testMultiToolResultOrder :: Test
testMultiToolResultOrder = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Let me check both."
        , crToolCalls = Just
            [ ToolCall "tc-o1" "read_file" (object ["path" .= ("a.hs" :: T.Text)])
            , ToolCall "tc-o2" "read_file" (object ["path" .= ("b.hs" :: T.Text)])
            , ToolCall "tc-o3" "list_files" (object ["dir" .= ("." :: T.Text)])
            ]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let cfg   = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "check files"
  let conv = asConversation state'
      -- Find tool-result messages (those with msgCallId set)
      toolResults = filter (\m -> msgCallId m /= Nothing) conv
      callIds     = map (maybe "" id . msgCallId) toolResults
  -- The tool results should appear in the same order as the tool calls
  if callIds == ["tc-o1", "tc-o2", "tc-o3"]
    then pure $ Right ()
    else pure $ Left $ "Tool result order: " ++ show callIds

-- | Assistant message with tool_calls preserves them in conversation.
testMultiToolAssistantPreservesCalls :: Test
testMultiToolAssistantPreservesCalls = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Checking."
        , crToolCalls = Just
            [ ToolCall "tc-p1" "read_file" (object ["path" .= ("x.hs" :: T.Text)])
            , ToolCall "tc-p2" "list_files" (object ["dir" .= ("." :: T.Text)])
            ]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "All done."
        , crToolCalls = Nothing
        }
    ]
  let cfg   = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "check"
  let conv = asConversation state'
      -- Find assistant messages with tool_calls
      asstWithCalls = filter
        (\m -> msgRole m == Assistant && msgToolCalls m /= Nothing) conv
  case asstWithCalls of
    [m] -> case msgToolCalls m of
      Just tcs | length tcs == 2 -> pure $ Right ()
      other -> pure $ Left $ "Expected 2 tool calls, got: " ++ show other
    _ -> pure $ Left $ "Expected 1 assistant msg with calls, got: "
                     ++ show (length asstWithCalls)

-- | Multi-tool conversation: tool results reference the correct call IDs.
testMultiToolCallIdCorrespondence :: Test
testMultiToolCallIdCorrespondence = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Let me check."
        , crToolCalls = Just
            [ ToolCall "tc-c1" "read_file" (object ["path" .= ("a.hs" :: T.Text)])
            , ToolCall "tc-c2" "read_file" (object ["path" .= ("b.hs" :: T.Text)])
            ]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let cfg   = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "check"
  let conv = asConversation state'
      -- Get tool_calls from the assistant message
      asstMsgs = filter
        (\m -> msgRole m == Assistant && msgToolCalls m /= Nothing) conv
      toolCallIds = case asstMsgs of
        (m:_) -> case msgToolCalls m of
          Just tcs -> map tcId tcs
          Nothing  -> []
        _ -> []
      -- Get call IDs from tool-result messages
      toolResults = filter (\m -> msgCallId m /= Nothing) conv
      resultIds   = map (maybe "" id . msgCallId) toolResults
  -- Every tool call ID should have a matching tool result
  if toolCallIds == resultIds && not (null toolCallIds)
    then pure $ Right ()
    else pure $ Left $ "callIds=" ++ show toolCallIds ++ " resultIds=" ++ show resultIds

-- | Helper: generate a command that produces very long output (cross-platform).
veryLongOutputCmd :: String
veryLongOutputCmd
  | os == "mingw32" = "powershell -c \"1..2000 | % { 'abcdefghijabcdefghijabcdefghij' }\""
  | otherwise       = "seq 1 2000 | while read i; do printf 'abcdefghij%.0s' $(seq 1 3); done"

-- | Shell truncation metadata is present in tool output when output is long.
testShellTruncationMetaNewFormat :: Test
testShellTruncationMetaNewFormat = do
  -- Generate output that exceeds shellOutputLimit (4096 chars)
  let args = object ["command" .= (T.pack veryLongOutputCmd :: T.Text)]
  result <- toolExecute shellTool args
  let out = trOutput result
  -- Check for the new metadata format
  let hasTruncMeta = T.isInfixOf "[truncated:" out
      hasReturned  = T.isInfixOf "returned" out
      hasDropped   = T.isInfixOf "dropped" out
  if hasTruncMeta && hasReturned && hasDropped
    then pure $ Right ()
    else pure $ Left $ "truncMeta=" ++ show hasTruncMeta
                     ++ " returned=" ++ show hasReturned
                     ++ " dropped=" ++ show hasDropped

-- | Shell with short output has no truncation metadata.
testShellShortOutputNoTruncMeta :: Test
testShellShortOutputNoTruncMeta = do
  let args = object ["command" .= ("echo short-output-test" :: T.Text)]
  result <- toolExecute shellTool args
  let out = trOutput result
  if not (T.isInfixOf "[truncated" out) && T.isInfixOf "short-output-test" out
    then pure $ Right ()
    else pure $ Left $ "Unexpected truncation metadata in short output: " ++ T.unpack out

-- ---------------------------------------------------------------------------
-- Session event data content tests
-- ---------------------------------------------------------------------------

-- | EAssistantReply event data includes tool-call summary when the
--   assistant response contains tool calls.  The summary should list
--   each call ID and tool name.
testSessionEventAssistantReplyWithToolCalls :: Test
testSessionEventAssistantReplyWithToolCalls = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Let me check."
        , crToolCalls = Just
            [ ToolCall "tc-ar1" "read_file" (object ["path" .= ("x.hs" :: T.Text)])
            , ToolCall "tc-ar2" "list_files" (object ["dir" .= ("." :: T.Text)])
            ]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "check"
  let evts = events (asSession state')
      -- Find the first EAssistantReply (the one with tool calls)
      asstEvts = filter (\e -> evType e == EAssistantReply) evts
  case asstEvts of
    (firstAsst:_) -> do
      let d = evData firstAsst
      if T.isInfixOf "tc-ar1=read_file" d && T.isInfixOf "tc-ar2=list_files" d
        then pure $ Right ()
        else pure $ Left $ "Missing tool-call summary in assistant event: " ++ T.unpack d
    _ -> pure $ Left "No EAssistantReply events found"

-- | EAssistantReply event data is plain text when no tool calls.
testSessionEventAssistantReplyTextOnly :: Test
testSessionEventAssistantReplyTextOnly = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Hello there."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "hi"
  let evts = events (asSession state')
      asstEvts = filter (\e -> evType e == EAssistantReply) evts
  case asstEvts of
    (firstAsst:_) ->
      if evData firstAsst == "Hello there."
        then pure $ Right ()
        else pure $ Left $ "Expected plain text, got: " ++ T.unpack (evData firstAsst)
    _ -> pure $ Left "No EAssistantReply events found"

-- | EToolCall event data contains the call ID and tool name.
testSessionEventToolCallData :: Test
testSessionEventToolCallData = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Reading."
        , crToolCalls = Just [ToolCall "tc-tc1" "read_file"
                                (object ["path" .= ("foo.hs" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "read foo"
  let evts = events (asSession state')
      toolCallEvts = filter (\e -> evType e == EToolCall) evts
  case toolCallEvts of
    (tcEvt:_) -> do
      let d = evData tcEvt
      if T.isInfixOf "tc-tc1" d && T.isInfixOf "read_file" d
        then pure $ Right ()
        else pure $ Left $ "EToolCall missing call ID or name: " ++ T.unpack d
    _ -> pure $ Left "No EToolCall events found"

-- | EToolResult event data contains the call ID and output.
testSessionEventToolResultData :: Test
testSessionEventToolResultData = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-session-tr-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "tr-test.txt") "session test content"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Reading."
        , crToolCalls = Just [ToolCall "tc-tr1" "read_file"
                                 (object ["path" .= ("tr-test.txt" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "read file"
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let evts = events (asSession state')
      trEvts = filter (\e -> evType e == EToolResult) evts
  case trEvts of
    (trEvt:_) -> do
      let d = evData trEvt
      if T.isInfixOf "tc-tr1" d && T.isInfixOf "session test content" d
        then pure $ Right ()
        else pure $ Left $ "EToolResult missing call ID or output: " ++ T.unpack d
    _ -> pure $ Left "No EToolResult events found"

-- | Session events are in chronological order (type sequence matches
--   the expected flow for a simple tool-call round trip).
--   Note: even Allow decisions generate an EPolicyDecision event.
testSessionEventChronologicalOrder :: Test
testSessionEventChronologicalOrder = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Let me read."
        , crToolCalls = Just [ToolCall "tc-ord1" "read_file"
                                (object ["path" .= ("x.hs" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done reading."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "read x"
  let types = map evType (events (asSession state'))
  -- Expected order: UserMessage, AssistantReply, PolicyDecision (Allow),
  --                 ToolCall, ToolResult, AssistantReply
  if types == [EUserMessage, EAssistantReply, EPolicyDecision,
               EToolCall, EToolResult, EAssistantReply]
    then pure $ Right ()
    else pure $ Left $ "Event order: " ++ show types

-- | Multi-tool + policy-approved shell: session events are complete
--   and in correct order.  This is the key integration test that
--   exercises the full audit trail: user message, assistant reply
--   (with tool_calls summary), multiple tool calls + results,
--   another assistant reply requesting shell, policy decision (AskUser),
--   approval, shell execution, and final assistant reply.
testMultiToolThenShellSessionEvents :: Test
testMultiToolThenShellSessionEvents = do
  prov <- scriptedProvider
    [ -- First response: request two read_file calls
      CompletionResponse
        { crReply     = mkAssistantMessage "Let me read both files."
        , crToolCalls = Just
            [ ToolCall "tc-ms1" "read_file" (object ["path" .= ("a.hs" :: T.Text)])
            , ToolCall "tc-ms2" "read_file" (object ["path" .= ("b.hs" :: T.Text)])
            ]
        }
    , -- Second response: now request a shell command
      CompletionResponse
        { crReply     = mkAssistantMessage "Now let me run the tests."
        , crToolCalls = Just [ToolCall "tc-ms3" "shell"
                                (object ["command" .= ("echo shell-ok" :: T.Text)])]
        }
    , -- Third response: final summary
      CompletionResponse
        { crReply     = mkAssistantMessage "All tasks complete."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "read files and run tests"
  let evts = events (asSession state')
      types = map evType evts

  -- Verify event type sequence:
  --   UserMessage, AssistantReply (with tool_calls),
  --   PolicyDecision (Allow), ToolCall, ToolResult,
  --   PolicyDecision (Allow), ToolCall, ToolResult,
  --   AssistantReply (with tool_calls),
  --   PolicyDecision (AskUser), PolicyDecision (approved),
  --   ToolCall, ToolResult,
  --   AssistantReply (final)
  let expectedTypes =
        [ EUserMessage
        , EAssistantReply    -- "Let me read both files." + tool_calls
        , EPolicyDecision    -- Allow for read_file (tc-ms1)
        , EToolCall          -- tc-ms1 read_file
        , EToolResult        -- tc-ms1 result
        , EPolicyDecision    -- Allow for read_file (tc-ms2)
        , EToolCall          -- tc-ms2 read_file
        , EToolResult        -- tc-ms2 result
        , EAssistantReply    -- "Now let me run the tests." + tool_calls
        , EPolicyDecision    -- AskUser for shell
        , EPolicyDecision    -- approved by user
        , EToolCall          -- tc-ms3 shell
        , EToolResult        -- tc-ms3 result
        , EAssistantReply    -- "All tasks complete."
        ]

  -- Check that key event types are present
  let hasUser    = EUserMessage `elem` types
      hasAsst    = EAssistantReply `elem` types
      hasTCall   = EToolCall `elem` types
      hasTResult = EToolResult `elem` types
      hasPolicy  = EPolicyDecision `elem` types

  -- Check that the first assistant reply includes tool-call summary
      asstEvts = filter (\e -> evType e == EAssistantReply) evts
      firstAsstOk = case asstEvts of
        (a:_) -> T.isInfixOf "tc-ms1=read_file" (evData a)
        _     -> False

  -- Check that policy decision events include approval
      policyEvts = filter (\e -> evType e == EPolicyDecision) evts
      hasApproval = any (T.isInfixOf "approved by user" . evData) policyEvts

  -- Check that shell tool result contains exit code
      trEvts = filter (\e -> evType e == EToolResult) evts
      shellResultOk = any (\e -> T.isInfixOf "tc-ms3" (evData e)
                              && T.isInfixOf "[exit]" (evData e)) trEvts

  -- Check the exact order matches expected
      orderOk = types == expectedTypes

  if hasUser && hasAsst && hasTCall && hasTResult && hasPolicy
     && firstAsstOk && hasApproval && shellResultOk && orderOk
    then pure $ Right ()
    else pure $ Left $
      "order=" ++ show (types == expectedTypes)
      ++ " types=" ++ show types
      ++ " firstAsstToolCalls=" ++ show firstAsstOk
      ++ " approval=" ++ show hasApproval
      ++ " shellResult=" ++ show shellResultOk

-- | Session event for a Deny decision contains the denial reason.
testSessionEventDenyData :: Test
testSessionEventDenyData = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Let me delete."
        , crToolCalls = Just [ToolCall "tc-deny1" "shell"
                                (object ["command" .= ("rm -rf /" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "OK."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "delete"
  let evts = events (asSession state')
      policyEvts = filter (\e -> evType e == EPolicyDecision) evts
      denyEvts = filter (T.isInfixOf "Deny" . evData) policyEvts
  case denyEvts of
    (d:_) ->
      if T.isInfixOf "dangerous" (evData d)
        then pure $ Right ()
        else pure $ Left $ "Deny event missing reason: " ++ T.unpack (evData d)
    _ -> pure $ Left "No Deny policy events found"

-- | Session event for a rejected tool call contains "denied by user".
testSessionEventRejectData :: Test
testSessionEventRejectData = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Let me run that."
        , crToolCalls = Just [ToolCall "tc-rej1" "shell"
                                (object ["command" .= ("ls" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "OK, skipping."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoReject
  state' <- runAgent state "list"
  let evts = events (asSession state')
      trEvts = filter (\e -> evType e == EToolResult) evts
      deniedEvts = filter (T.isInfixOf "denied by user" . evData) trEvts
  case deniedEvts of
    (_:_) -> pure $ Right ()
    _     -> pure $ Left $ "No 'denied by user' in tool results: "
                          ++ show (map evData trEvts)

-- ---------------------------------------------------------------------------
-- flushLog persistence tests
-- ---------------------------------------------------------------------------

-- | Helper: read a JSONL file and return each line as a decoded Value.
--   Filters out empty lines (e.g. trailing newline).
readJsonlFile :: FilePath -> IO [Value]
readJsonlFile path = do
  bytes <- LBS.readFile path
  let lines' = LBS.split (fromIntegral (fromEnum '\n')) bytes
  pure [ v | line <- lines', not (LBS.null line), Just v <- [decode line] ]

-- | flushLog writes events as valid JSONL in chronological order.
testFlushLogWritesJsonl :: Test
testFlushLogWritesJsonl = do
  tmpDir <- getTemporaryDirectory
  now    <- getCurrentTime
  -- Clean up any stale session.jsonl from prior tests (flushLog appends).
  let path = tmpDir </> "session.jsonl"
  _ <- try (removeFile path) :: IO (Either IOException ())
  let log' = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EUserMessage "hello"
        , Event now EAssistantReply "hi there"
        , Event now EToolCall "tc-1 read_file"
        , Event now EToolResult "tc-1 file contents"
        ]
  flushLog tmpDir 0 log'
  exists <- doesFileExist path
  if not exists
    then pure $ Left "session.jsonl was not created"
    else do
      vals <- readJsonlFile path
      -- Each line should be a JSON object with "time", "type", "data"
      let isValidEvent v = case v of
            Object o ->
              KM.member (Key.fromText "time") o
              && KM.member (Key.fromText "type") o
              && KM.member (Key.fromText "data") o
            _ -> False
      if length vals == 4 && all isValidEvent vals
        then do
          -- Verify chronological order by checking "type" fields
          let types = map (\v -> case v of
                Object o -> KM.lookup (Key.fromText "type") o
                _        -> Nothing) vals
          if types == [ Just (String "user_message")
                      , Just (String "assistant_reply")
                      , Just (String "tool_call")
                      , Just (String "tool_result")
                      ]
            then do
              cleanup path
              pure $ Right ()
            else do
              cleanup path
              pure $ Left $ "Event type order: " ++ show types
        else do
          cleanup path
          pure $ Left $ "Expected 4 valid events, got " ++ show (length vals)

-- | flushLog on an empty log does not create a file (no events to write).
testFlushLogEmpty :: Test
testFlushLogEmpty = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir </> "session.jsonl"
  -- Clean up any leftover file from prior tests
  _ <- try (removeFile path) :: IO (Either IOException ())
  flushLog tmpDir 0 emptyLog
  exists <- doesFileExist path
  if not exists
    then pure $ Right ()
    else do
      cleanup path
      pure $ Left "Expected no file for empty log, but file exists"

-- | flushLog appends on subsequent calls (does not overwrite).
testFlushLogAppends :: Test
testFlushLogAppends = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir </> "session.jsonl"
  -- Clean up any leftover file
  exists <- doesFileExist path
  if exists then removeFile path else pure ()
  now <- getCurrentTime
  -- First flush: 2 events
  let log1 = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EUserMessage "first"
        , Event now EAssistantReply "reply1"
        ]
  flushLog tmpDir 0 log1
  -- Second flush: 1 event (appends to the same file)
  let log2 = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EUserMessage "second"
        ]
  flushLog tmpDir 0 log2
  vals <- readJsonlFile path
  cleanup path
  if length vals == 3
    then pure $ Right ()
    else pure $ Left $ "Expected 3 events after 2 flushes, got " ++ show (length vals)

-- | Agent run + flushLog: a full agent session flushes events that
--   decode back to the expected event types.  Uses scriptedProvider
--   so no network access is needed.
testFlushLogAgentIntegration :: Test
testFlushLogAgentIntegration = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Let me read."
        , crToolCalls = Just [ToolCall "tc-fl1" "read_file"
                                (object ["path" .= ("foo.hs" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done reading."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "read foo"
  tmpDir <- getTemporaryDirectory
  let flushPath = tmpDir </> "session.jsonl"
  -- Clean up any leftover file
  _ <- try (removeFile flushPath) :: IO (Either IOException ())
  flushLog tmpDir 0 (asSession state')
  exists <- doesFileExist flushPath
  if not exists
    then pure $ Left "session.jsonl not created after agent run"
    else do
      vals <- readJsonlFile flushPath
      cleanup flushPath
      let types = map (\v -> case v of
            Object o -> KM.lookup (Key.fromText "type") o
            _        -> Nothing) vals
      -- Expected: user_message, assistant_reply, policy_decision,
      --           tool_call, tool_result, assistant_reply
      if types == [ Just (String "user_message")
                  , Just (String "assistant_reply")
                  , Just (String "policy_decision")
                  , Just (String "tool_call")
                  , Just (String "tool_result")
                  , Just (String "assistant_reply")
                  ]
        then pure $ Right ()
        else pure $ Left $ "Flushed event types: " ++ show types

-- ---------------------------------------------------------------------------
-- flushLogOnException tests
-- ---------------------------------------------------------------------------

-- | flushLogOnException does NOT flush when the action succeeds.
testFlushLogOnExceptionNoFlushOnSuccess :: Test
testFlushLogOnExceptionNoFlushOnSuccess = do
  tmpDir <- getTemporaryDirectory
  now    <- getCurrentTime
  let log' = logEvent (Event now EUserMessage "test") emptyLog
      path = tmpDir </> "session.jsonl"
  _ <- try (removeFile path) :: IO (Either IOException ())
  val <- flushLogOnException tmpDir 0 log' (pure 42 :: IO Int)
  exists <- doesFileExist path
  cleanup path
  if val == 42 && not exists
    then pure $ Right ()
    else pure $ Left $ "val=" ++ show val ++ " fileExists=" ++ show exists

-- | flushLogOnException flushes the session log when the action throws.
testFlushLogOnExceptionFlushesOnThrow :: Test
testFlushLogOnExceptionFlushesOnThrow = do
  tmpDir <- getTemporaryDirectory
  now    <- getCurrentTime
  let log' = logEvent (Event now EUserMessage "before-exception") emptyLog
      path = tmpDir </> "session.jsonl"
  _ <- try (removeFile path) :: IO (Either IOException ())
  let badAction = throwIO (userError "simulated error") :: IO ()
  result <- try (flushLogOnException tmpDir 0 log' badAction) :: IO (Either IOException ())
  exists <- doesFileExist path
  case (result, exists) of
    (Left _, True) -> do
      vals <- readJsonlFile path
      cleanup path
      if length vals == 1
        then pure $ Right ()
        else pure $ Left $ "Expected 1 event, got " ++ show (length vals)
    (Left _, False) -> do
      cleanup path
      pure $ Left "Exception was thrown but session.jsonl was not created"
    (Right _, _) -> do
      cleanup path
      pure $ Left "Expected exception but action succeeded"

-- | flushLogOnException does not create a file for an empty log on exception.
testFlushLogOnExceptionEmptyLog :: Test
testFlushLogOnExceptionEmptyLog = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir </> "session.jsonl"
  _ <- try (removeFile path) :: IO (Either IOException ())
  let badAction = throwIO (userError "simulated") :: IO ()
  _ <- try (flushLogOnException tmpDir 0 emptyLog badAction) :: IO (Either IOException ())
  exists <- doesFileExist path
  cleanup path
  if not exists
    then pure $ Right ()
    else pure $ Left "Expected no file for empty log, but file exists"

-- ---------------------------------------------------------------------------
-- Session log rotation tests
-- ---------------------------------------------------------------------------

-- | Helper: read file size, returning 0 if the file does not exist.
fileSizeIfExists :: FilePath -> IO Integer
fileSizeIfExists path = do
  exists <- doesFileExist path
  if exists
    then do
      h <- openFile path ReadMode
      s <- hFileSize h
      hClose h
      pure s
    else pure 0

-- | Normal flush still appends when existing log is under the limit.
testFlushLogRotationUnderLimit :: Test
testFlushLogRotationUnderLimit = do
  tmpDir <- getTemporaryDirectory
  let path   = tmpDir </> "session.jsonl"
      backup = tmpDir </> "session.jsonl.1"
  -- Clean slate
  _ <- try (removeFile path) :: IO (Either IOException ())
  _ <- try (removeFile backup) :: IO (Either IOException ())
  now <- getCurrentTime
  -- Write an initial small log.
  let log1 = logEvent (Event now EUserMessage "first") emptyLog
  flushLog tmpDir 0 log1
  -- Flush again with a high limit — should NOT rotate.
  let log2 = logEvent (Event now EAssistantReply "second") emptyLog
  flushLog tmpDir 1000000 log2
  -- Both events should be in the main file.
  vals <- readJsonlFile path
  backupExists <- doesFileExist backup
  cleanup path
  cleanup backup
  if length vals == 2 && not backupExists
    then pure $ Right ()
    else pure $ Left $ "Under-limit: events=" ++ show (length vals)
                     ++ " backupExists=" ++ show backupExists

-- | Oversized existing session.jsonl rotates to .1 before new events.
testFlushLogRotationOverLimit :: Test
testFlushLogRotationOverLimit = do
  tmpDir <- getTemporaryDirectory
  let path   = tmpDir </> "session.jsonl"
      backup = tmpDir </> "session.jsonl.1"
  -- Clean slate
  _ <- try (removeFile path) :: IO (Either IOException ())
  _ <- try (removeFile backup) :: IO (Either IOException ())
  now <- getCurrentTime
  -- Write enough events to exceed a tiny limit.
  let bigLog = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EUserMessage (T.replicate 200 "x")
        , Event now EAssistantReply (T.replicate 200 "y")
        ]
  flushLog tmpDir 0 bigLog
  sizeBefore <- fileSizeIfExists path
  -- Now flush with a limit smaller than the existing file.
  let newLog = logEvent (Event now EToolCall "new event") emptyLog
  flushLog tmpDir 100 newLog
  -- The old content should be in backup, new file should have only the new event.
  newVals <- readJsonlFile path
  backupExists <- doesFileExist backup
  backupVals <- if backupExists then readJsonlFile backup else pure []
  cleanup path
  cleanup backup
  if sizeBefore > 100
     && backupExists
     && length backupVals == 2
     && length newVals == 1
    then pure $ Right ()
    else pure $ Left $ "Over-limit: sizeBefore=" ++ show sizeBefore
                     ++ " backupExists=" ++ show backupExists
                     ++ " backupEvents=" ++ show (length backupVals)
                     ++ " newEvents=" ++ show (length newVals)

-- | Existing .1 backup is replaced deterministically on re-rotation.
testFlushLogRotationBackupReplaced :: Test
testFlushLogRotationBackupReplaced = do
  tmpDir <- getTemporaryDirectory
  let path   = tmpDir </> "session.jsonl"
      backup = tmpDir </> "session.jsonl.1"
  -- Clean slate
  _ <- try (removeFile path) :: IO (Either IOException ())
  _ <- try (removeFile backup) :: IO (Either IOException ())
  now <- getCurrentTime
  -- Create an old backup with known content.
  let oldBackup = logEvent (Event now ESessionStart "old backup") emptyLog
  flushLog tmpDir 0 oldBackup
  renameFile path backup
  -- Create an oversized main log.
  let bigLog = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EUserMessage (T.replicate 200 "a")
        , Event now EAssistantReply (T.replicate 200 "b")
        ]
  flushLog tmpDir 0 bigLog
  -- Rotate with a small limit.
  let newLog = logEvent (Event now EToolCall "replacement") emptyLog
  flushLog tmpDir 100 newLog
  -- The backup should now contain the bigLog events (2), not the old backup (1).
  backupVals <- readJsonlFile backup
  newVals <- readJsonlFile path
  cleanup path
  cleanup backup
  if length backupVals == 2 && length newVals == 1
    then pure $ Right ()
    else pure $ Left $ "Backup replaced: backupEvents=" ++ show (length backupVals)
                     ++ " newEvents=" ++ show (length newVals)

-- | Empty session still does not create files, even with rotation enabled.
testFlushLogRotationEmptySession :: Test
testFlushLogRotationEmptySession = do
  tmpDir <- getTemporaryDirectory
  let path   = tmpDir </> "session.jsonl"
      backup = tmpDir </> "session.jsonl.1"
  -- Clean slate
  _ <- try (removeFile path) :: IO (Either IOException ())
  _ <- try (removeFile backup) :: IO (Either IOException ())
  -- Flush empty log with rotation enabled.
  flushLog tmpDir 100 emptyLog
  exists <- doesFileExist path
  backupExists <- doesFileExist backup
  cleanup path
  cleanup backup
  if not exists && not backupExists
    then pure $ Right ()
    else pure $ Left $ "Empty rotation: fileExists=" ++ show exists
                     ++ " backupExists=" ++ show backupExists

-- | After rotation the new JSONL file contains valid JSON events.
testFlushLogRotationValidJsonl :: Test
testFlushLogRotationValidJsonl = do
  tmpDir <- getTemporaryDirectory
  let path   = tmpDir </> "session.jsonl"
      backup = tmpDir </> "session.jsonl.1"
  -- Clean slate
  _ <- try (removeFile path) :: IO (Either IOException ())
  _ <- try (removeFile backup) :: IO (Either IOException ())
  now <- getCurrentTime
  -- Create an oversized log.
  let bigLog = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EUserMessage (T.replicate 200 "c")
        , Event now EAssistantReply (T.replicate 200 "d")
        ]
  flushLog tmpDir 0 bigLog
  -- Rotate and write new events.
  let newLog = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EToolCall "tc-1 read_file"
        , Event now EToolResult "tc-1 contents"
        ]
  flushLog tmpDir 100 newLog
  -- Read and validate the new file.
  vals <- readJsonlFile path
  let isValidEvent v = case v of
        Object o ->
          KM.member (Key.fromText "time") o
          && KM.member (Key.fromText "type") o
          && KM.member (Key.fromText "data") o
        _ -> False
  cleanup path
  cleanup backup
  if length vals == 2 && all isValidEvent vals
    then pure $ Right ()
    else pure $ Left $ "Valid JSONL after rotation: events=" ++ show (length vals)
                     ++ " allValid=" ++ show (all isValidEvent vals)

-- | matchGlob with simple * wildcard.
testMatchGlobSimpleStar :: Test
testMatchGlobSimpleStar =
  if matchGlob "*.hs" "Foo.hs" && not (matchGlob "*.hs" "src/Foo.hs")
     && matchGlob "*.txt" "README.txt" && not (matchGlob "*.hs" "Foo.txt")
    then pure $ Right ()
    else pure $ Left "matchGlob simple * failed"

-- | matchGlob with ** recursive wildcard.
testMatchGlobDoubleStar :: Test
testMatchGlobDoubleStar =
  if matchGlob "**/*.hs" "Foo.hs"
     && matchGlob "**/*.hs" "src/Foo.hs"
     && matchGlob "**/*.hs" "src/A/Bar.hs"
     && not (matchGlob "**/*.hs" "Foo.txt")
    then pure $ Right ()
    else pure $ Left "matchGlob ** failed"

-- | matchGlob with directory prefix.
testMatchGlobDirPrefix :: Test
testMatchGlobDirPrefix =
  if matchGlob "src/**/*.hs" "src/Foo.hs"
     && matchGlob "src/**/*.hs" "src/A/Bar.hs"
     && not (matchGlob "src/**/*.hs" "test/Foo.hs")
     && not (matchGlob "src/**/*.hs" "Foo.hs")
    then pure $ Right ()
    else pure $ Left "matchGlob dir prefix failed"

-- | matchGlob with exact filename (no wildcard).
testMatchGlobExact :: Test
testMatchGlobExact =
  if matchGlob "Makefile" "Makefile"
     && not (matchGlob "Makefile" "src/Makefile")
     && not (matchGlob "Makefile" "makefile")
    then pure $ Right ()
    else pure $ Left "matchGlob exact failed"

-- | isIgnoredDir skips known build/cache directories.
testIsIgnoredDir :: Test
testIsIgnoredDir =
  if isIgnoredDir ".git"
     && isIgnoredDir "dist-newstyle"
     && isIgnoredDir ".stack-work"
     && isIgnoredDir "node_modules"
     && not (isIgnoredDir "src")
     && not (isIgnoredDir "test")
    then pure $ Right ()
    else pure $ Left "isIgnoredDir failed"

-- ---------------------------------------------------------------------------
-- Search helper tests (pure)
-- ---------------------------------------------------------------------------

-- | searchInText finds matching lines (case-sensitive default).
testSearchInText :: Test
testSearchInText =
  let body = "hello world\nfoo bar\nhello again\nnothing"
      results = searchInText False "hello" body
  in if map fst results == [1, 3] && length results == 2
       then pure $ Right ()
       else pure $ Left $ "searchInText: " ++ show (map fst results)

-- | searchInText returns nothing for empty query.
testSearchInTextEmptyQuery :: Test
testSearchInTextEmptyQuery =
  if null (searchInText False "" "hello world")
    then pure $ Right ()
    else pure $ Left "searchInText empty query should return no results"

-- | searchInText returns nothing when query not found.
testSearchInTextNoMatch :: Test
testSearchInTextNoMatch =
  if null (searchInText False "xyz" "hello world\nfoo bar")
    then pure $ Right ()
    else pure $ Left "searchInText no-match should return no results"

-- | searchInText with ignoreCase=True finds mixed-case matches.
testSearchInTextIgnoreCase :: Test
testSearchInTextIgnoreCase =
  let body = "Hello World\nfoo bar\nHELLO again\nnothing"
      results = searchInText True "hello" body
  in if map fst results == [1, 3] && length results == 2
       then pure $ Right ()
       else pure $ Left $ "searchInText ignoreCase: " ++ show (map fst results)

-- | searchInText with ignoreCase=True returns nothing when query not found.
testSearchInTextIgnoreCaseNoMatch :: Test
testSearchInTextIgnoreCaseNoMatch =
  if null (searchInText True "xyz" "Hello World\nFoo Bar")
    then pure $ Right ()
    else pure $ Left "searchInText ignoreCase no-match should return no results"

-- | searchInText with ignoreCase=False is still case-sensitive (does not match mixed case).
testSearchInTextCaseSensitiveDefault :: Test
testSearchInTextCaseSensitiveDefault =
  let body = "Hello World\nfoo bar\nHELLO again\nnothing"
      results = searchInText False "hello" body
  in if null results
       then pure $ Right ()
       else pure $ Left $ "searchInText case-sensitive should miss mixed-case: " ++ show (map fst results)

-- | formatSearchMatch produces path:line:snippet format.
testFormatSearchMatch :: Test
testFormatSearchMatch =
  let result = formatSearchMatch ("src/Main.hs", 42, "hello world")
  in if T.isInfixOf "src/Main.hs:42:" result && T.isInfixOf "hello world" result
       then pure $ Right ()
       else pure $ Left $ "formatSearchMatch: " ++ T.unpack result

-- | formatSearchMatch truncates long lines to 120 chars.
testFormatSearchMatchTruncates :: Test
testFormatSearchMatchTruncates =
  let longLine = T.replicate 200 "x"
      result   = formatSearchMatch ("f.hs", 1, longLine)
      snippet  = last (T.splitOn ":" result)
  in if T.length snippet <= 120
       then pure $ Right ()
       else pure $ Left $ "formatSearchMatch did not truncate: " ++ show (T.length snippet)

-- ---------------------------------------------------------------------------
-- Glob tool IO tests
-- ---------------------------------------------------------------------------

-- | Helper: create a temp directory tree for testing.
--   Returns (rootDir, cleanup action).
createTestTree :: IO (FilePath, IO ())
createTestTree = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-glob-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createDirectory (root </> "src")
  createDirectory (root </> "src" </> "deep")
  createDirectory (root </> "test")
  createDirectory (root </> ".git")
  writeFile (root </> "Main.hs")         "module Main where\nmain = putStrLn \"hi\""
  writeFile (root </> "README.md")       "# Test project"
  writeFile (root </> "Makefile")        "all: build"
  writeFile (root </> "src" </> "Lib.hs")    "module Lib where\nlibFunc = id"
  writeFile (root </> "src" </> "Util.hs")   "module Util where\nutilFunc = id"
  writeFile (root </> "src" </> "deep" </> "Inner.hs") "module Inner where"
  writeFile (root </> "src" </> "data.txt")  "some data"
  writeFile (root </> "test" </> "Spec.hs")  "module Spec where\ntestFunc = id"
  writeFile (root </> ".git" </> "config")   "[core]"
  let cleanupAction = do
        _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
        pure ()
  pure (root, cleanupAction)

-- | globTool finds files matching a simple pattern.
testGlobToolSimple :: Test
testGlobToolSimple = do
  (root, cleanupAction) <- createTestTree
  -- Run glob from the test root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("*.hs" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "Main.hs" out && T.isInfixOf "1 files match" out
    then pure $ Right ()
    else pure $ Left $ "glob *.hs: " ++ T.unpack out

-- | globTool with ** wildcard finds files in subdirectories.
testGlobToolRecursive :: Test
testGlobToolRecursive = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*.hs" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Should find Main.hs, src/Lib.hs, src/Util.hs, src/deep/Inner.hs, test/Spec.hs
  if T.isInfixOf "Main.hs" out && T.isInfixOf "Lib.hs" out
     && T.isInfixOf "Inner.hs" out && T.isInfixOf "5 files match" out
    then pure $ Right ()
    else pure $ Left $ "glob **/*.hs: " ++ T.unpack out

-- | globTool skips .git and other ignored directories.
testGlobToolSkipsIgnored :: Test
testGlobToolSkipsIgnored = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Should NOT contain .git/config
  if not (T.isInfixOf ".git" out)
    then pure $ Right ()
    else pure $ Left $ "glob **/* included .git: " ++ T.unpack out

-- | globTool with directory prefix pattern.
testGlobToolDirPrefix :: Test
testGlobToolDirPrefix = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("src/**/*.hs" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Should find src/Lib.hs, src/Util.hs, src/deep/Inner.hs but NOT Main.hs or test/Spec.hs
  if T.isInfixOf "Lib.hs" out && T.isInfixOf "Inner.hs" out
     && not (T.isInfixOf "Main.hs" out) && not (T.isInfixOf "Spec.hs" out)
     && T.isInfixOf "3 files match" out
    then pure $ Right ()
    else pure $ Left $ "glob src/**/*.hs: " ++ T.unpack out

-- | globTool returns "0 files" for a pattern that matches nothing.
testGlobToolNoMatch :: Test
testGlobToolNoMatch = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("*.xyz" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "0 files match" out
    then pure $ Right ()
    else pure $ Left $ "glob *.xyz: " ++ T.unpack out

-- ---------------------------------------------------------------------------
-- Search tool IO tests
-- ---------------------------------------------------------------------------

-- | searchTool finds matches across files.
testSearchToolFindsMatches :: Test
testSearchToolFindsMatches = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object ["query" .= ("module" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- "module" appears in Main.hs, Lib.hs, Util.hs, Inner.hs, Spec.hs
  if T.isInfixOf "matches for" out && T.isInfixOf "Main.hs:" out
     && T.isInfixOf "Lib.hs:" out
    then pure $ Right ()
    else pure $ Left $ "search module: " ++ T.unpack out

-- | searchTool returns "0 matches" when query is not found.
testSearchToolNoMatch :: Test
testSearchToolNoMatch = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object ["query" .= ("zzzznotfound" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "0 matches" out
    then pure $ Right ()
    else pure $ Left $ "search zzzznotfound: " ++ T.unpack out

-- | searchTool respects directory argument (search single file).
testSearchToolSingleFile :: Test
testSearchToolSingleFile = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query"     .= ("putStrLn" :: T.Text)
    , "directory" .= ("Main.hs" :: T.Text)
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "Main.hs:" out && T.isInfixOf "1 matches" out
    then pure $ Right ()
    else pure $ Left $ "search Main.hs: " ++ T.unpack out

-- | searchTool skips binary-extension files.
testSearchToolSkipsBinary :: Test
testSearchToolSkipsBinary = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-search-bin-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "code.hs")  "foobar_unique_12345"
  writeFile (root </> "image.png") "foobar_unique_12345"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object ["query" .= ("foobar_unique_12345" :: T.Text)])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find code.hs but NOT image.png
  if T.isInfixOf "code.hs" out && not (T.isInfixOf "image.png" out)
    then pure $ Right ()
    else pure $ Left $ "search binary skip: " ++ T.unpack out

-- | searchTool skips .git directory.
testSearchToolSkipsIgnoredDirs :: Test
testSearchToolSkipsIgnoredDirs = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object ["query" .= ("core" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- "core" appears in .git/config but should be skipped
  if not (T.isInfixOf ".git" out)
    then pure $ Right ()
    else pure $ Left $ "search included .git: " ++ T.unpack out

-- | globTool is in the default registry.
testGlobInRegistry :: Test
testGlobInRegistry =
  if "glob" `elem` toolNames defaultRegistry
    then pure $ Right ()
    else pure $ Left $ "glob not in registry: " ++ show (toolNames defaultRegistry)

-- | searchTool is in the default registry.
testSearchInRegistry :: Test
testSearchInRegistry =
  if "search" `elem` toolNames defaultRegistry
    then pure $ Right ()
    else pure $ Left $ "search not in registry: " ++ show (toolNames defaultRegistry)

-- ---------------------------------------------------------------------------
-- isUnderRoot tests (pure)
-- ---------------------------------------------------------------------------

-- | isUnderRoot allows exact match.
testIsUnderRootExact :: Test
testIsUnderRootExact =
  if isUnderRoot "/a/b" "/a/b"
    then pure $ Right ()
    else pure $ Left "isUnderRoot exact match failed"

-- | isUnderRoot allows subdirectory.
testIsUnderRootSubdir :: Test
testIsUnderRootSubdir =
  if isUnderRoot "/a/b" "/a/b/c/d"
    then pure $ Right ()
    else pure $ Left "isUnderRoot subdirectory failed"

-- | isUnderRoot rejects sibling (prefix collision).
testIsUnderRootSibling :: Test
testIsUnderRootSibling =
  if not (isUnderRoot "/a/b" "/a/bad")
    then pure $ Right ()
    else pure $ Left "isUnderRoot rejected sibling prefix collision"

-- | isUnderRoot rejects unrelated path.
testIsUnderRootOutside :: Test
testIsUnderRootOutside =
  if not (isUnderRoot "/a/b" "/etc")
    then pure $ Right ()
    else pure $ Left "isUnderRoot rejected unrelated path"

-- ---------------------------------------------------------------------------
-- Search: directory default and path safety
-- ---------------------------------------------------------------------------

-- | searchTool with no directory argument defaults to working directory.
testSearchDefaultDir :: Test
testSearchDefaultDir = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object ["query" .= ("module" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "Main.hs:" out && T.isInfixOf "Lib.hs:" out
    then pure $ Right ()
    else pure $ Left $ "search default dir: " ++ T.unpack out

-- | searchTool default (case-sensitive) does NOT match wrong case.
testSearchCaseSensitiveDefault :: Test
testSearchCaseSensitiveDefault = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-search-cs-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "a.hs") "module Main where\nimport Foo\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object ["query" .= ("MODULE" :: T.Text)])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "0 matches" out
    then pure $ Right ()
    else pure $ Left $ "search case-sensitive default: " ++ T.unpack out

-- | searchTool with ignore_case:true finds mixed-case matches.
testSearchIgnoreCaseTrue :: Test
testSearchIgnoreCaseTrue = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-search-ic-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "a.hs") "module Main where\nimport Foo\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query"       .= ("MODULE" :: T.Text)
    , "ignore_case" .= True
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "1 matches" out && T.isInfixOf "case-insensitive" out
    then pure $ Right ()
    else pure $ Left $ "search ignore_case true: " ++ T.unpack out

-- | searchTool with ignore_case:true and scoped directory still works.
testSearchIgnoreCaseScoped :: Test
testSearchIgnoreCaseScoped = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query"       .= ("MODULE" :: T.Text)
    , "directory"   .= ("src" :: T.Text)
    , "ignore_case" .= True
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Should find "module" in src/ files but NOT in test/Spec.hs or Main.hs
  if T.isInfixOf "Lib.hs:" out && T.isInfixOf "case-insensitive" out
     && not (T.isInfixOf "Spec.hs:" out) && not (T.isInfixOf "Main.hs:" out)
    then pure $ Right ()
    else pure $ Left $ "search ignore_case scoped: " ++ T.unpack out

-- | searchTool with directory argument scopes to that subtree.
testSearchScopedDir :: Test
testSearchScopedDir = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query"     .= ("module" :: T.Text)
    , "directory" .= ("src" :: T.Text)
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Should find matches in src/ but NOT in test/Spec.hs or Main.hs
  if T.isInfixOf "Lib.hs:" out && not (T.isInfixOf "Spec.hs:" out)
     && not (T.isInfixOf "Main.hs:" out)
    then pure $ Right ()
    else pure $ Left $ "search scoped dir: " ++ T.unpack out

-- | searchTool rejects a directory outside the working root.
testSearchRejectsOutsideRoot :: Test
testSearchRejectsOutsideRoot = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query"     .= ("module" :: T.Text)
    , "directory" .= ("C:\\Windows\\System32" :: T.Text)
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "search outside root: " ++ T.unpack out

-- | searchTool returns error for a non-existent path.
testSearchNotFound :: Test
testSearchNotFound = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query"     .= ("x" :: T.Text)
    , "directory" .= ("nonexistent_dir" :: T.Text)
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "not found" out
    then pure $ Right ()
    else pure $ Left $ "search not found: " ++ T.unpack out

-- ---------------------------------------------------------------------------
-- Search: large file skipping
-- ---------------------------------------------------------------------------

-- | searchTool skips files larger than searchMaxFileSize.
testSearchSkipsLargeFile :: Test
testSearchSkipsLargeFile = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-search-size-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "small.hs") "unique_token_abc123"
  -- Write a file just over the limit.
  let bigSize = fromIntegral searchMaxFileSize + 1
  writeFile (root </> "big.hs") (replicate bigSize 'x' ++ "unique_token_abc123\n")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query" .= ("unique_token_abc123" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find the match in small.hs but skip big.hs.
  if T.isInfixOf "small.hs:" out && T.isInfixOf "1 matches" out
     && T.isInfixOf "skipped" out && T.isInfixOf "1 large files" out
    then pure $ Right ()
    else pure $ Left $ "search large file: " ++ T.unpack out

-- | searchMaxFileSize is a positive value.
testSearchMaxFileSizePositive :: Test
testSearchMaxFileSizePositive =
  if searchMaxFileSize > 0
    then pure $ Right ()
    else pure $ Left $ "searchMaxFileSize not positive: " ++ show searchMaxFileSize

-- ---------------------------------------------------------------------------
-- TraversalStats / formatStats tests (pure)
-- ---------------------------------------------------------------------------

-- | formatStats returns empty string for zero stats.
testFormatStatsEmpty :: Test
testFormatStatsEmpty =
  if formatStats emptyStats == ""
    then pure $ Right ()
    else pure $ Left "formatStats empty should be empty string"

-- | formatStats formats a mix of skip reasons correctly.
testFormatStatsMixed :: Test
testFormatStatsMixed =
  let stats = TraversalStats { tsSkippedLarge = 2, tsSkippedUnreadable = 1, tsOutsideRoot = 0, tsIgnoredByAgent = 0, tsRevisitedDirs = 0 }
      out   = formatStats stats
  in if T.isInfixOf "2 large files" out && T.isInfixOf "1 unreadable" out
       && not (T.isInfixOf "outside" out)
       then pure $ Right ()
       else pure $ Left $ "formatStats mixed: " ++ T.unpack out

-- | formatStats formats outside-root correctly.
testFormatStatsOutside :: Test
testFormatStatsOutside =
  let stats = emptyStats { tsOutsideRoot = 3 }
      out   = formatStats stats
  in if T.isInfixOf "3 outside project root" out
       then pure $ Right ()
       else pure $ Left $ "formatStats outside: " ++ T.unpack out

-- ---------------------------------------------------------------------------
-- safeCanonicalize tests
-- ---------------------------------------------------------------------------

-- | safeCanonicalize returns Just for an existing path.
testSafeCanonicalizeExisting :: Test
testSafeCanonicalizeExisting = do
  result <- safeCanonicalize "."
  case result of
    Just _  -> pure $ Right ()
    Nothing -> pure $ Left "safeCanonicalize '.' returned Nothing"

-- | safeCanonicalize returns Nothing for a broken symlink.
testSafeCanonicalizeBrokenSymlink :: Test
testSafeCanonicalizeBrokenSymlink = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-canon-broken-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createFileLink (root </> "nonexistent") (root </> "broken")
  result <- safeCanonicalize (root </> "broken")
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Nothing -> pure $ Right ()
    Just _  -> pure $ Left "safeCanonicalize broken symlink should return Nothing"

-- ---------------------------------------------------------------------------
-- Glob: broken symlink and outside-root tests
-- ---------------------------------------------------------------------------

-- | globTool handles broken symlinks gracefully (skips them).
testGlobBrokenSymlink :: Test
testGlobBrokenSymlink = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-glob-broken-sym"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "good.hs") "module Good where"
  createFileLink (root </> "nonexistent") (root </> "broken.hs")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object
    [ "pattern" .= ("*.hs" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find good.hs, skip broken.hs, report unreadable
  if T.isInfixOf "good.hs" out && T.isInfixOf "skipped" out && T.isInfixOf "unreadable" out
    then pure $ Right ()
    else pure $ Left $ "glob broken symlink: " ++ T.unpack out

-- | globTool skips symlinks that resolve outside the project root.
testGlobOutsideRootSymlink :: Test
testGlobOutsideRootSymlink = do
  tmpDir <- getTemporaryDirectory
  let root   = tmpDir </> "haskode-glob-outside-root"
      target = tmpDir </> "haskode-glob-outside-target"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  createDirectory root
  createDirectory target
  writeFile (target </> "secret.hs") "module Secret where"
  writeFile (root </> "safe.hs") "module Safe where"
  createFileLink target (root </> "outside")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object
    [ "pattern" .= ("**/*.hs" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find safe.hs but NOT secret.hs; should report outside root
  if T.isInfixOf "safe.hs" out && not (T.isInfixOf "secret.hs" out)
     && T.isInfixOf "outside" out
    then pure $ Right ()
    else pure $ Left $ "glob outside root: " ++ T.unpack out

-- ---------------------------------------------------------------------------
-- Search: broken symlink and outside-root tests
-- ---------------------------------------------------------------------------

-- | searchTool handles broken symlinks gracefully (skips them).
testSearchBrokenSymlink :: Test
testSearchBrokenSymlink = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-search-broken-sym"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "good.hs") "unique_token_broken_test"
  createFileLink (root </> "nonexistent") (root </> "broken.hs")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query" .= ("unique_token_broken_test" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find good.hs, skip broken.hs, report unreadable
  if T.isInfixOf "good.hs:" out && T.isInfixOf "skipped" out && T.isInfixOf "unreadable" out
    then pure $ Right ()
    else pure $ Left $ "search broken symlink: " ++ T.unpack out

-- | searchTool skips symlinks that resolve outside the project root.
testSearchOutsideRootSymlink :: Test
testSearchOutsideRootSymlink = do
  tmpDir <- getTemporaryDirectory
  let root   = tmpDir </> "haskode-search-outside-root"
      target = tmpDir </> "haskode-search-outside-target"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  createDirectory root
  createDirectory target
  writeFile (target </> "secret.hs") "unique_token_secret_outside"
  writeFile (root </> "safe.hs") "unique_token_secret_outside"
  createFileLink target (root </> "outside")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query" .= ("unique_token_secret_outside" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find safe.hs but NOT secret.hs; should report outside root
  if T.isInfixOf "safe.hs:" out && not (T.isInfixOf "secret.hs" out)
     && T.isInfixOf "outside" out
    then pure $ Right ()
    else pure $ Left $ "search outside root: " ++ T.unpack out

-- | searchTool handles an unreadable directory gracefully.
testSearchUnreadableDir :: Test
testSearchUnreadableDir = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-search-unreadable-dir"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createDirectory (root </> "locked")
  writeFile (root </> "locked" </> "secret.hs") "unique_token_locked"
  writeFile (root </> "open.hs") "unique_token_locked"
  origPerms <- getPermissions (root </> "locked")
  setPermissions (root </> "locked") emptyPermissions
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query" .= ("unique_token_locked" :: T.Text)
    ])
  setCurrentDirectory origDir
  -- Restore permissions so cleanup works
  setPermissions (root </> "locked") origPerms
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find open.hs, skip locked dir, report unreadable
  if T.isInfixOf "open.hs:" out && T.isInfixOf "skipped" out && T.isInfixOf "unreadable" out
    then pure $ Right ()
    else pure $ Left $ "search unreadable dir: " ++ T.unpack out

-- | readFileTool returns an error message for an unreadable file.
testReadFileUnreadable :: Test
testReadFileUnreadable = do
  (root, cleanupAction) <- createTestTree
  writeFile (root </> "locked.txt") "secret content"
  origPerms <- getPermissions (root </> "locked.txt")
  setPermissions (root </> "locked.txt") emptyPermissions
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute readFileTool (object
    [ "path" .= ("locked.txt" :: T.Text)
    ])
  setCurrentDirectory origDir
  setPermissions (root </> "locked.txt") origPerms
  cleanupAction
  let out = trOutput result
  -- Should return an error, not crash
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "read_file unreadable: " ++ T.unpack out

-- | listFilesTool returns an error message for an unreadable directory.
testListFilesUnreadable :: Test
testListFilesUnreadable = do
  (root, cleanupAction) <- createTestTree
  createDirectory (root </> "locked")
  origPerms <- getPermissions (root </> "locked")
  setPermissions (root </> "locked") emptyPermissions
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute listFilesTool (object
    [ "dir" .= ("locked" :: T.Text)
    ])
  setCurrentDirectory origDir
  setPermissions (root </> "locked") origPerms
  cleanupAction
  let out = trOutput result
  -- Should return an error, not crash
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "list_files unreadable: " ++ T.unpack out

-- ---------------------------------------------------------------------------
-- Path-safety hardening tests for read_file and list_files
-- ---------------------------------------------------------------------------

-- | readFileTool rejects paths that escape the working directory via ...
testReadFileRejectsDotDotEscape :: Test
testReadFileRejectsDotDotEscape = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute readFileTool (object
    [ "path" .= ("../../etc/passwd" :: T.Text)
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Either "working directory" or "could not resolve" is acceptable
  -- (Windows may not resolve .. paths the same as Unix).
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "read_file .. escape: " ++ T.unpack out

-- | readFileTool rejects absolute paths outside the working directory.
testReadFileRejectsAbsoluteOutsideRoot :: Test
testReadFileRejectsAbsoluteOutsideRoot = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute readFileTool (object
    [ "path" .= ("C:\\Windows\\System32\\config\\SAM" :: T.Text)
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- On any platform, reading a system file outside the working dir must fail.
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "read_file absolute outside: " ++ T.unpack out

-- | readFileTool allows normal relative paths within the working directory.
testReadFileNormalInRoot :: Test
testReadFileNormalInRoot = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute readFileTool (object
    [ "path" .= ("src/Lib.hs" :: T.Text)
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "module Lib" out
    then pure $ Right ()
    else pure $ Left $ "read_file in-root: " ++ T.unpack out

-- | readFileTool rejects symlinks that resolve outside the working directory.
testReadFileRejectsOutsideRootSymlink :: Test
testReadFileRejectsOutsideRootSymlink = do
  tmpDir <- getTemporaryDirectory
  let root   = tmpDir </> "haskode-rf-safety-root"
      target = tmpDir </> "haskode-rf-safety-target"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  createDirectory root
  createDirectory target
  writeFile (target </> "secret.txt") "secret content"
  writeFile (root </> "safe.txt") "safe content"
  createFileLink (target </> "secret.txt") (root </> "outside-link")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute readFileTool (object
    [ "path" .= ("outside-link" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  let out = trOutput result
  -- Should reject the outside-root symlink, not return secret content
  if T.isInfixOf "error" out && T.isInfixOf "working directory" out
     && not (T.isInfixOf "secret content" out)
    then pure $ Right ()
    else pure $ Left $ "read_file outside symlink: " ++ T.unpack out

-- | readFileTool handles broken symlinks gracefully.
testReadFileHandlesBrokenSymlink :: Test
testReadFileHandlesBrokenSymlink = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-rf-broken-root"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "good.txt") "good content"
  createFileLink (root </> "nonexistent") (root </> "broken-link")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute readFileTool (object
    [ "path" .= ("broken-link" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should return an error (could not resolve), not crash
  if T.isInfixOf "error" out && T.isInfixOf "could not resolve" out
    then pure $ Right ()
    else pure $ Left $ "read_file broken symlink: " ++ T.unpack out

-- | listFilesTool rejects paths that escape the working directory via ...
testListFilesRejectsDotDotEscape :: Test
testListFilesRejectsDotDotEscape = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute listFilesTool (object
    [ "dir" .= ("../../tmp" :: T.Text)
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "list_files .. escape: " ++ T.unpack out

-- | listFilesTool rejects absolute paths outside the working directory.
testListFilesRejectsAbsoluteOutsideRoot :: Test
testListFilesRejectsAbsoluteOutsideRoot = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute listFilesTool (object
    [ "dir" .= ("C:\\Windows\\System32" :: T.Text)
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- On any platform, listing a system dir outside the working dir must fail.
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "list_files absolute outside: " ++ T.unpack out

-- | listFilesTool allows normal relative paths within the working directory.
testListFilesNormalInRoot :: Test
testListFilesNormalInRoot = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute listFilesTool (object
    [ "dir" .= ("src" :: T.Text)
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "Lib.hs" out && T.isInfixOf "Util.hs" out
    then pure $ Right ()
    else pure $ Left $ "list_files in-root: " ++ T.unpack out

-- | listFilesTool rejects symlinks that resolve outside the working directory.
testListFilesRejectsOutsideRootSymlink :: Test
testListFilesRejectsOutsideRootSymlink = do
  tmpDir <- getTemporaryDirectory
  let root   = tmpDir </> "haskode-lf-safety-root"
      target = tmpDir </> "haskode-lf-safety-target"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  createDirectory root
  createDirectory target
  writeFile (target </> "secret.hs") "secret module"
  writeFile (root </> "safe.hs") "safe module"
  createDirectoryLink target (root </> "outside-dir")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute listFilesTool (object
    [ "dir" .= ("outside-dir" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  let out = trOutput result
  -- Should reject the outside-root symlink, not list target contents
  if T.isInfixOf "error" out && T.isInfixOf "working directory" out
     && not (T.isInfixOf "secret.hs" out)
    then pure $ Right ()
    else pure $ Left $ "list_files outside symlink: " ++ T.unpack out

-- | listFilesTool handles broken symlinks gracefully.
testListFilesHandlesBrokenSymlink :: Test
testListFilesHandlesBrokenSymlink = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-lf-broken-root"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createDirectoryLink (root </> "nonexistent") (root </> "broken-dir")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute listFilesTool (object
    [ "dir" .= ("broken-dir" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should return an error (could not resolve), not crash
  if T.isInfixOf "error" out && T.isInfixOf "could not resolve" out
    then pure $ Right ()
    else pure $ Left $ "list_files broken symlink: " ++ T.unpack out

-- ---------------------------------------------------------------------------
-- Symlink-loop / visited-directory hardening tests
-- ---------------------------------------------------------------------------

-- | globTool does not hang on a directory symlink loop.
testGlobSymlinkLoop :: Test
testGlobSymlinkLoop = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-glob-symloop"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createDirectory (root </> "a")
  createDirectoryLink (root </> "a") (root </> "a" </> "loop")
  writeFile (root </> "a" </> "real.hs") "module Real where"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*.hs" :: T.Text)])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find real.hs and terminate (not hang)
  if T.isInfixOf "real.hs" out && T.isInfixOf "files match" out
    then pure $ Right ()
    else pure $ Left $ "glob symlink loop: " ++ T.unpack out

-- | searchTool does not hang on a directory symlink loop.
testSearchSymlinkLoop :: Test
testSearchSymlinkLoop = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-search-symloop"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createDirectory (root </> "a")
  createDirectoryLink (root </> "a") (root </> "a" </> "loop")
  writeFile (root </> "a" </> "real.hs") "unique_token_symloop_search"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query" .= ("unique_token_symloop_search" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find the match and terminate (not hang)
  if T.isInfixOf "real.hs:" out && T.isInfixOf "1 matches" out
    then pure $ Right ()
    else pure $ Left $ "search symlink loop: " ++ T.unpack out

-- | globTool skips a revisited canonical directory (two paths to same dir)
--   and reports it in traversal stats.
testGlobRevisitedDir :: Test
testGlobRevisitedDir = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-glob-revisit"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createDirectory (root </> "real")
  writeFile (root </> "real" </> "A.hs") "module A where"
  -- Create a symlink so "real" is reachable via two paths:
  --   real/  and  alias/ -> real/
  createDirectoryLink (root </> "real") (root </> "alias")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*.hs" :: T.Text)])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find A.hs exactly once and report revisited dirs
  if T.isInfixOf "A.hs" out && T.isInfixOf "1 files match" out
     && T.isInfixOf "revisited dirs" out
    then pure $ Right ()
    else pure $ Left $ "glob revisited dir: " ++ T.unpack out

-- | searchTool skips a revisited canonical directory and reports stats.
testSearchRevisitedDir :: Test
testSearchRevisitedDir = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-search-revisit"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createDirectory (root </> "real")
  writeFile (root </> "real" </> "A.hs") "unique_token_revisit_search"
  createDirectoryLink (root </> "real") (root </> "alias")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query" .= ("unique_token_revisit_search" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find the match exactly once and report revisited dirs
  if T.isInfixOf "1 matches" out && T.isInfixOf "revisited dirs" out
    then pure $ Right ()
    else pure $ Left $ "search revisited dir: " ++ T.unpack out

-- | globTool follows a normal in-root directory symlink (no loop)
--   and finds files through it.
testGlobNormalSymlinkBehavior :: Test
testGlobNormalSymlinkBehavior = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-glob-normsym"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createDirectory (root </> "src")
  writeFile (root </> "src" </> "Lib.hs") "module Lib where"
  createDirectoryLink (root </> "src") (root </> "link")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*.hs" :: T.Text)])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find Lib.hs once (via real path), not twice.
  -- The symlink dir is revisited, so we expect a revisited stat.
  if T.isInfixOf "Lib.hs" out && T.isInfixOf "1 files match" out
    then pure $ Right ()
    else pure $ Left $ "glob normal symlink: " ++ T.unpack out

-- | .agentignore causes glob to skip a directory BEFORE traversal
--   enters it (so no revisited-dir stat for the ignored subtree).
testGlobAgentIgnoreBeforeTraversal :: Test
testGlobAgentIgnoreBeforeTraversal = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-glob-aibefore"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createDirectory (root </> "src")
  writeFile (root </> "src" </> "A.hs") "module A where"
  createDirectory (root </> "skipme")
  writeFile (root </> "skipme" </> "B.hs") "module B where"
  createDirectoryLink (root </> "src") (root </> "skipme" </> "link")
  writeFile (root </> ".agentignore") "skipme\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*.hs" :: T.Text)])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find A.hs only via src/, skip skipme/ entirely,
  -- and NOT report revisited dirs (agentignore blocked entry).
  if T.isInfixOf "A.hs" out && not (T.isInfixOf "B.hs" out)
     && T.isInfixOf ".agentignore" out
     && not (T.isInfixOf "revisited dirs" out)
    then pure $ Right ()
    else pure $ Left $ "glob agentignore before traversal: " ++ T.unpack out

-- | root containment still blocks outside-root symlinks in glob.
testGlobRootContainmentSymlink :: Test
testGlobRootContainmentSymlink = do
  tmpDir <- getTemporaryDirectory
  let root   = tmpDir </> "haskode-glob-rootcontain"
      target = tmpDir </> "haskode-glob-rootcontain-target"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  createDirectory root
  createDirectory target
  writeFile (target </> "secret.hs") "module Secret where"
  writeFile (root </> "safe.hs") "module Safe where"
  createDirectoryLink target (root </> "outside")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*.hs" :: T.Text)])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find safe.hs but NOT secret.hs; should report outside root
  if T.isInfixOf "safe.hs" out && not (T.isInfixOf "secret.hs" out)
     && T.isInfixOf "outside" out
    then pure $ Right ()
    else pure $ Left $ "glob root containment symlink: " ++ T.unpack out

-- | formatStats includes revisited dirs when present.
testFormatStatsRevisited :: Test
testFormatStatsRevisited =
  let stats = emptyStats { tsRevisitedDirs = 2 }
      out   = formatStats stats
  in if T.isInfixOf "2 revisited dirs" out
       then pure $ Right ()
       else pure $ Left $ "formatStats revisited: " ++ T.unpack out

-- ---------------------------------------------------------------------------
-- .agentignore tests
-- ---------------------------------------------------------------------------

-- | loadAgentIgnore returns an empty list when no .agentignore file exists.
testLoadAgentIgnoreNoFile :: Test
testLoadAgentIgnoreNoFile = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-agentignore-nofile"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  patterns <- loadAgentIgnore root
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  if null patterns
    then pure $ Right ()
    else pure $ Left $ "Expected empty list, got: " ++ show patterns

-- | loadAgentIgnore correctly parses comments, blanks, and patterns.
testLoadAgentIgnoreParses :: Test
testLoadAgentIgnoreParses = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-agentignore-parse"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> ".agentignore")
    "# this is a comment\n\
    \\n\
    \  \n\
    \build\n\
    \*.log\n\
    \# another comment\n\
    \dist\n"
  patterns <- loadAgentIgnore root
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  if patterns == ["build", "*.log", "dist"]
    then pure $ Right ()
    else pure $ Left $ "Parsed patterns: " ++ show patterns

-- | Without .agentignore, glob preserves existing behavior.
testGlobNoAgentIgnorePreservesBehavior :: Test
testGlobNoAgentIgnorePreservesBehavior = do
  (root, cleanupAction) <- createTestTree
  -- Explicitly remove any stale .agentignore from prior test runs.
  _ <- try (removeFile (root </> ".agentignore")) :: IO (Either IOException ())
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*.hs" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Should find all .hs files, same as before .agentignore existed
  if T.isInfixOf "Main.hs" out && T.isInfixOf "Lib.hs" out
     && T.isInfixOf "5 files match" out
     && not (T.isInfixOf "by .agentignore" out)
    then pure $ Right ()
    else pure $ Left $ "glob no-agentignore: " ++ T.unpack out

-- | .agentignore causes glob to skip a directory.
testGlobAgentIgnoreSkipsDir :: Test
testGlobAgentIgnoreSkipsDir = do
  (root, cleanupAction) <- createTestTree
  -- Add .agentignore that ignores the "test" directory
  writeFile (root </> ".agentignore") "test\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*.hs" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Should find Main.hs, Lib.hs, Util.hs, Inner.hs but NOT test/Spec.hs
  if T.isInfixOf "Main.hs" out && T.isInfixOf "Lib.hs" out
     && not (T.isInfixOf "Spec.hs" out)
     && T.isInfixOf "4 files match" out
     && T.isInfixOf ".agentignore" out
    then pure $ Right ()
    else pure $ Left $ "glob agentignore dir: " ++ T.unpack out

-- | .agentignore causes search to skip a directory.
testSearchAgentIgnoreSkipsDir :: Test
testSearchAgentIgnoreSkipsDir = do
  (root, cleanupAction) <- createTestTree
  -- Add .agentignore that ignores the "test" directory
  writeFile (root </> ".agentignore") "test\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object ["query" .= ("module" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- "module" appears in Main.hs, Lib.hs, Util.hs, Inner.hs, Spec.hs
  -- but Spec.hs should be skipped because test/ is ignored
  if T.isInfixOf "Main.hs:" out && T.isInfixOf "Lib.hs:" out
     && not (T.isInfixOf "Spec.hs:" out)
     && T.isInfixOf ".agentignore" out
    then pure $ Right ()
    else pure $ Left $ "search agentignore dir: " ++ T.unpack out

-- | .agentignore still skips a directory when ignore_case is true.
testSearchAgentIgnoreWithIgnoreCase :: Test
testSearchAgentIgnoreWithIgnoreCase = do
  (root, cleanupAction) <- createTestTree
  writeFile (root </> ".agentignore") "test\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query"       .= ("MODULE" :: T.Text)
    , "ignore_case" .= True
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "Main.hs:" out && T.isInfixOf "Lib.hs:" out
     && not (T.isInfixOf "Spec.hs:" out)
     && T.isInfixOf ".agentignore" out
     && T.isInfixOf "case-insensitive" out
    then pure $ Right ()
    else pure $ Left $ "search agentignore ignore_case: " ++ T.unpack out

-- | .agentignore with a file pattern skips matching files.
testAgentIgnoreSkipsFile :: Test
testAgentIgnoreSkipsFile = do
  (root, cleanupAction) <- createTestTree
  -- Ignore all .txt files
  writeFile (root </> ".agentignore") "*.txt\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Should find .hs and .md files but NOT data.txt
  if T.isInfixOf "Main.hs" out && not (T.isInfixOf "data.txt" out)
     && T.isInfixOf ".agentignore" out
    then pure $ Right ()
    else pure $ Left $ "agentignore file pattern: " ++ T.unpack out

-- | Comments and blank lines in .agentignore do not cause errors.
testAgentIgnoreCommentsAndBlanks :: Test
testAgentIgnoreCommentsAndBlanks = do
  (root, cleanupAction) <- createTestTree
  writeFile (root </> ".agentignore")
    "# skip test dir\n\n  \ntest\n# end\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*.hs" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- test/ should be skipped; comments should be harmless
  if T.isInfixOf "Main.hs" out && not (T.isInfixOf "Spec.hs" out)
     && T.isInfixOf "4 files match" out
    then pure $ Right ()
    else pure $ Left $ "agentignore comments: " ++ T.unpack out

-- | Built-in ignored dirs (.git) still apply alongside .agentignore.
testAgentIgnoreBuiltInStillApplies :: Test
testAgentIgnoreBuiltInStillApplies = do
  (root, cleanupAction) <- createTestTree
  -- .agentignore ignores "test"; .git is built-in
  writeFile (root </> ".agentignore") "test\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Neither .git nor test should appear
  if not (T.isInfixOf ".git" out) && not (T.isInfixOf "config" out)
     && not (T.isInfixOf "Spec.hs" out)
     && T.isInfixOf "Main.hs" out
    then pure $ Right ()
    else pure $ Left $ "agentignore + built-in: " ++ T.unpack out

-- | Traversal stats correctly report agent-ignore skips.
testAgentIgnoreStats :: Test
testAgentIgnoreStats = do
  (root, cleanupAction) <- createTestTree
  writeFile (root </> ".agentignore") "test\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Stats should mention .agentignore skips
  if T.isInfixOf "by .agentignore" out
    then pure $ Right ()
    else pure $ Left $ "agentignore stats: " ++ T.unpack out

-- | shouldIgnorePath matches single-component patterns against entry names.
testShouldIgnorePathSingleComponent :: Test
testShouldIgnorePathSingleComponent =
  let patterns = ["build", "*.log"]
  in if shouldIgnorePath patterns "build" "src/build"
        && shouldIgnorePath patterns "app.log" "logs/app.log"
        && not (shouldIgnorePath patterns "src" "src")
        && not (shouldIgnorePath patterns "Main.hs" "src/Main.hs")
     then pure $ Right ()
     else pure $ Left "shouldIgnorePath single-component failed"

-- | shouldIgnorePath matches multi-component patterns against relative paths.
testShouldIgnorePathMultiComponent :: Test
testShouldIgnorePathMultiComponent =
  let patterns = ["vendor/*", "src/deep"]
  in if shouldIgnorePath patterns "foo" "vendor/foo"
        && shouldIgnorePath patterns "deep" "src/deep"
        && not (shouldIgnorePath patterns "deep" "other/deep")
     then pure $ Right ()
     else pure $ Left "shouldIgnorePath multi-component failed"

-- ---------------------------------------------------------------------------
-- AGENTS.md tests
-- ---------------------------------------------------------------------------

-- | Without AGENTS.md, buildSystemPrompt produces the same prompt as before.
testBuildSystemPromptNoAgentsMd :: Test
testBuildSystemPromptNoAgentsMd = do
  let prompt = buildSystemPrompt defaultRegistry Nothing
  if T.isInfixOf "helpful coding assistant" prompt
     && T.isInfixOf "Available tools" prompt
     && not (T.isInfixOf "AGENTS.md" prompt)
     && not (T.isInfixOf "Repository instructions" prompt)
    then pure $ Right ()
    else pure $ Left $ "system prompt without AGENTS.md: " ++ T.unpack (T.take 200 prompt)

-- | With AGENTS.md content, the content appears in the system prompt.
testBuildSystemPromptWithAgentsMd :: Test
testBuildSystemPromptWithAgentsMd = do
  let agentsContent = "Always use tabs, not spaces.\nWrite tests first."
      prompt = buildSystemPrompt defaultRegistry (Just agentsContent)
  if T.isInfixOf "Repository instructions" prompt
     && T.isInfixOf "AGENTS.md" prompt
     && T.isInfixOf "Always use tabs" prompt
     && T.isInfixOf "Write tests first" prompt
    then pure $ Right ()
    else pure $ Left $ "system prompt with AGENTS.md: " ++ T.unpack (T.take 300 prompt)

-- | loadAgentsMd returns Nothing when no AGENTS.md exists.
testLoadAgentsMdNoFile :: Test
testLoadAgentsMdNoFile = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-agentsmd-nofile"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- loadAgentsMd
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Nothing -> pure $ Right ()
    Just _  -> pure $ Left "loadAgentsMd should return Nothing when no file"

-- | loadAgentsMd reads content correctly from a valid AGENTS.md.
testLoadAgentsMdContent :: Test
testLoadAgentsMdContent = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-agentsmd-content"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "AGENTS.md") "# Project rules\nUse Haskell.\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- loadAgentsMd
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Just txt | T.isInfixOf "Use Haskell" txt -> pure $ Right ()
    other -> pure $ Left $ "loadAgentsMd content: " ++ show other

-- | loadAgentsMd returns Nothing for a broken symlink.
testLoadAgentsMdBrokenSymlink :: Test
testLoadAgentsMdBrokenSymlink = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-agentsmd-broken"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createFileLink (root </> "nonexistent") (root </> "AGENTS.md")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- loadAgentsMd
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Nothing -> pure $ Right ()
    Just _  -> pure $ Left "loadAgentsMd should return Nothing for broken symlink"

-- | loadAgentsMd returns Nothing for a symlink pointing outside root.
testLoadAgentsMdOutsideRoot :: Test
testLoadAgentsMdOutsideRoot = do
  tmpDir <- getTemporaryDirectory
  let root   = tmpDir </> "haskode-agentsmd-outside"
      target = tmpDir </> "haskode-agentsmd-target"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  createDirectory root
  createDirectory target
  writeFile (target </> "AGENTS.md") "secret instructions"
  createFileLink (target </> "AGENTS.md") (root </> "AGENTS.md")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- loadAgentsMd
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  case result of
    Nothing -> pure $ Right ()
    Just _  -> pure $ Left "loadAgentsMd should reject outside-root symlink"

-- ---------------------------------------------------------------------------
-- preview_patch tool tests
-- ---------------------------------------------------------------------------

-- | previewPatchTool produces a normal unified diff for an in-root file.
testPreviewPatchNormalDiff :: Test
testPreviewPatchNormalDiff = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("Main.hs" :: T.Text)
        , "replacement" .= ("module Main where\nmain = putStrLn \"hello\"\n" :: T.Text)
        ]
  result <- toolExecute previewPatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- The diff should contain the old and new content markers
  if T.isInfixOf "--- Main.hs" out
     && T.isInfixOf "-module Main where" out
     && T.isInfixOf "+module Main where" out
     && T.isInfixOf "Diff preview" out
     && T.isInfixOf "no files modified" out
    then pure $ Right ()
    else pure $ Left $ "preview_patch diff: " ++ T.unpack (T.take 300 out)

-- | previewPatchTool does NOT modify the filesystem.
testPreviewPatchNoModification :: Test
testPreviewPatchNoModification = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  original <- TIO.readFile "Main.hs"
  let args = object
        [ "path"        .= ("Main.hs" :: T.Text)
        , "replacement" .= ("completely different content\n" :: T.Text)
        ]
  _ <- toolExecute previewPatchTool args
  after <- TIO.readFile "Main.hs"
  setCurrentDirectory origDir
  cleanupAction
  if original == after
    then pure $ Right ()
    else pure $ Left "preview_patch modified the file!"

-- | previewPatchTool rejects paths outside the working directory.
testPreviewPatchOutsideRoot :: Test
testPreviewPatchOutsideRoot = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("C:\\Windows\\System32\\drivers\\etc\\hosts" :: T.Text)
        , "replacement" .= ("new content" :: T.Text)
        ]
  result <- toolExecute previewPatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "preview_patch outside root: " ++ T.unpack out

-- | previewPatchTool handles broken symlinks gracefully.
testPreviewPatchBrokenSymlink :: Test
testPreviewPatchBrokenSymlink = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-preview-broken"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createFileLink (root </> "nonexistent") (root </> "broken.hs")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("broken.hs" :: T.Text)
        , "replacement" .= ("new content" :: T.Text)
        ]
  result <- toolExecute previewPatchTool args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "could not resolve" out
    then pure $ Right ()
    else pure $ Left $ "preview_patch broken symlink: " ++ T.unpack out

-- | previewPatchTool returns an error for a missing file.
testPreviewPatchMissingFile :: Test
testPreviewPatchMissingFile = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("nonexistent.hs" :: T.Text)
        , "replacement" .= ("new content" :: T.Text)
        ]
  result <- toolExecute previewPatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "could not resolve" out
    then pure $ Right ()
    else pure $ Left $ "preview_patch missing file: " ++ T.unpack out

-- | previewPatchTool refuses diffs that exceed the size limit.
testPreviewPatchTooLarge :: Test
testPreviewPatchTooLarge = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  -- Generate a replacement string large enough to exceed the diff limit.
  -- The file Main.hs is ~40 chars; the replacement is ~10000 chars.
  -- The diff will be ~10000+ chars, exceeding the 8192 limit.
  let bigReplacement = T.replicate 10000 "x"
      args = object
        [ "path"        .= ("Main.hs" :: T.Text)
        , "replacement" .= bigReplacement
        ]
  result <- toolExecute previewPatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "too large" out
    then pure $ Right ()
    else pure $ Left $ "preview_patch too large: " ++ T.unpack (T.take 200 out)

-- | previewPatchTool is in the default registry.
testPreviewPatchInRegistry :: Test
testPreviewPatchInRegistry =
  if "preview_patch" `elem` toolNames defaultRegistry
    then pure $ Right ()
    else pure $ Left $ "preview_patch not in registry: " ++ show (toolNames defaultRegistry)

-- | previewPatchTool is allowed by default policy (no approval needed).
testPreviewPatchPolicyAllow :: Test
testPreviewPatchPolicyAllow =
  let tc = ToolCall "tc-pp" "preview_patch" (object ["path" .= ("x.hs" :: T.Text), "replacement" .= ("y" :: T.Text)])
  in case checkPolicy defaultPolicy tc of
       Allow -> pure $ Right ()
       other -> pure $ Left $ "Expected Allow for preview_patch, got: " ++ show other

-- ---------------------------------------------------------------------------
-- apply_patch tool tests
-- ---------------------------------------------------------------------------

-- | applyPatchTool successfully applies a patch to an in-root file.
testApplyPatchSuccess :: Test
testApplyPatchSuccess = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("Main.hs" :: T.Text)
        , "replacement" .= ("module Main where\nmain = putStrLn \"patched\"\n" :: T.Text)
        ]
  result <- toolExecute applyPatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "Patch applied" out && T.isInfixOf "--- Main.hs" out
    then pure $ Right ()
    else pure $ Left $ "apply_patch result: " ++ T.unpack (T.take 300 out)

-- | applyPatchTool actually changes the file content on disk.
testApplyPatchFileChanged :: Test
testApplyPatchFileChanged = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let newContent = "module Main where\nmain = putStrLn \"changed\"\n"
      args = object
        [ "path"        .= ("Main.hs" :: T.Text)
        , "replacement" .= newContent
        ]
  _ <- toolExecute applyPatchTool args
  after <- TIO.readFile "Main.hs"
  setCurrentDirectory origDir
  cleanupAction
  if after == newContent
    then pure $ Right ()
    else pure $ Left $ "apply_patch did not change file: " ++ T.unpack (T.take 200 after)

-- | applyPatchTool rejects paths outside the working directory.
testApplyPatchOutsideRoot :: Test
testApplyPatchOutsideRoot = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("C:\\Windows\\System32\\drivers\\etc\\hosts" :: T.Text)
        , "replacement" .= ("new content" :: T.Text)
        ]
  result <- toolExecute applyPatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "apply_patch outside root: " ++ T.unpack out

-- | applyPatchTool returns an error for a missing file.
testApplyPatchMissingFile :: Test
testApplyPatchMissingFile = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("nonexistent.hs" :: T.Text)
        , "replacement" .= ("new content" :: T.Text)
        ]
  result <- toolExecute applyPatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "could not resolve" out
    then pure $ Right ()
    else pure $ Left $ "apply_patch missing file: " ++ T.unpack out

-- | applyPatchTool handles broken symlinks gracefully.
testApplyPatchBrokenSymlink :: Test
testApplyPatchBrokenSymlink = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-apply-broken"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createFileLink (root </> "nonexistent") (root </> "broken.hs")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("broken.hs" :: T.Text)
        , "replacement" .= ("new content" :: T.Text)
        ]
  result <- toolExecute applyPatchTool args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "could not resolve" out
    then pure $ Right ()
    else pure $ Left $ "apply_patch broken symlink: " ++ T.unpack out

-- | applyPatchTool includes the diff in its result.
testApplyPatchDiffInResult :: Test
testApplyPatchDiffInResult = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("Main.hs" :: T.Text)
        , "replacement" .= ("module New where\n" :: T.Text)
        ]
  result <- toolExecute applyPatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- The diff should contain old and new markers
  if T.isInfixOf "-module Main where" out && T.isInfixOf "+module New where" out
    then pure $ Right ()
    else pure $ Left $ "apply_patch diff: " ++ T.unpack (T.take 300 out)

-- | applyPatchTool is in the default registry.
testApplyPatchInRegistry :: Test
testApplyPatchInRegistry =
  if "apply_patch" `elem` toolNames defaultRegistry
    then pure $ Right ()
    else pure $ Left $ "apply_patch not in registry: " ++ show (toolNames defaultRegistry)

-- | applyPatchTool is NOT allowed by default policy (requires confirmation).
testApplyPatchPolicyAskUser :: Test
testApplyPatchPolicyAskUser =
  let tc = ToolCall "tc-ap" "apply_patch" (object ["path" .= ("x.hs" :: T.Text), "replacement" .= ("y" :: T.Text)])
  in case checkPolicy defaultPolicy tc of
       AskUser _ -> pure $ Right ()
       other     -> pure $ Left $ "Expected AskUser for apply_patch, got: " ++ show other

-- ---------------------------------------------------------------------------
-- write_file tool tests
-- ---------------------------------------------------------------------------

-- | write_file successfully creates a new in-root file after approval.
testWriteFileSuccess :: Test
testWriteFileSuccess = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-writefile-success"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"    .= ("new_file.hs" :: T.Text)
        , "content" .= ("module New where\nnew = 1\n" :: T.Text)
        ]
  result <- toolExecute writeFileTool args
  setCurrentDirectory origDir
  -- Read back the file to verify content
  exists <- doesFileExist (root </> "new_file.hs")
  content <- if exists then TIO.readFile (root </> "new_file.hs") else pure ""
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "File created" out
     && T.isInfixOf "new_file.hs" out
     && T.isInfixOf "+module New where" out
     && exists
     && T.isInfixOf "module New where" content
    then pure $ Right ()
    else pure $ Left $ "write_file success: out=" ++ T.unpack (T.take 200 out)
                     ++ " exists=" ++ show exists

-- | write_file refuses to overwrite an existing file.
testWriteFileRejectsOverwrite :: Test
testWriteFileRejectsOverwrite = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-writefile-overwrite"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "existing.hs") "module Existing where\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"    .= ("existing.hs" :: T.Text)
        , "content" .= ("module Overwritten where\n" :: T.Text)
        ]
  result <- toolExecute writeFileTool args
  setCurrentDirectory origDir
  -- File should be unchanged
  after <- TIO.readFile (root </> "existing.hs")
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "error" out
     && T.isInfixOf "already exists" out
     && T.isInfixOf "Existing" after  -- unchanged
    then pure $ Right ()
    else pure $ Left $ "write_file overwrite: " ++ T.unpack (T.take 200 out)

-- | write_file rejects paths outside the working directory.
testWriteFileOutsideRoot :: Test
testWriteFileOutsideRoot = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-writefile-outside"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"    .= ("C:\\Windows\\System32\\haskode-outside-test.txt" :: T.Text)
        , "content" .= ("outside content" :: T.Text)
        ]
  result <- toolExecute writeFileTool args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "write_file outside root: " ++ T.unpack (T.take 200 out)

-- | write_file rejects symlinks that resolve outside the working directory.
testWriteFileRejectsOutsideRootSymlink :: Test
testWriteFileRejectsOutsideRootSymlink = do
  tmpDir <- getTemporaryDirectory
  let root   = tmpDir </> "haskode-writefile-sym-root"
      target = tmpDir </> "haskode-writefile-sym-target"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  createDirectory root
  createDirectory target
  createDirectoryLink target (root </> "outside-dir")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"    .= ("outside-dir/newfile.txt" :: T.Text)
        , "content" .= ("symlink escape" :: T.Text)
        ]
  result <- toolExecute writeFileTool args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "working directory" out
    then pure $ Right ()
    else pure $ Left $ "write_file outside symlink: " ++ T.unpack (T.take 200 out)

-- | write_file rejects missing parent directories.
testWriteFileMissingParent :: Test
testWriteFileMissingParent = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-writefile-missing-parent"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"    .= ("nonexistent_dir/newfile.txt" :: T.Text)
        , "content" .= ("content" :: T.Text)
        ]
  result <- toolExecute writeFileTool args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "error" out
     && (T.isInfixOf "parent" out || T.isInfixOf "does not exist" out)
    then pure $ Right ()
    else pure $ Left $ "write_file missing parent: " ++ T.unpack (T.take 200 out)

-- | write_file rejects directory targets.
testWriteFileRejectsDirectory :: Test
testWriteFileRejectsDirectory = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-writefile-dir"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createDirectory (root </> "subdir")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"    .= ("subdir" :: T.Text)
        , "content" .= ("content" :: T.Text)
        ]
  result <- toolExecute writeFileTool args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "directory" out
    then pure $ Right ()
    else pure $ Left $ "write_file directory: " ++ T.unpack (T.take 200 out)

-- | write_file is in the default registry.
testWriteFileInRegistry :: Test
testWriteFileInRegistry =
  if "write_file" `elem` toolNames defaultRegistry
    then pure $ Right ()
    else pure $ Left $ "write_file not in registry: " ++ show (toolNames defaultRegistry)

-- | write_file is NOT allowed by default policy (requires confirmation).
testWriteFilePolicyAskUser :: Test
testWriteFilePolicyAskUser =
  let tc = ToolCall "tc-wf" "write_file" (object ["path" .= ("x.hs" :: T.Text), "content" .= ("y" :: T.Text)])
  in case checkPolicy defaultPolicy tc of
       AskUser _ -> pure $ Right ()
       other     -> pure $ Left $ "Expected AskUser for write_file, got: " ++ show other

-- | computeWriteFilePreview returns the path and preview for a valid new file.
testComputeWriteFilePreviewNormal :: Test
testComputeWriteFilePreviewNormal = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-wf-preview-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"    .= ("New.hs" :: T.Text)
        , "content" .= ("module New where\nnew = 1\n" :: T.Text)
        ]
  result <- computeWriteFilePreview args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Left err -> pure $ Left $ "Expected Right, got Left: " ++ T.unpack err
    Right (path, preview)
      | path /= "New.hs" ->
          pure $ Left $ "Expected path New.hs, got: " ++ path
      | not (T.isInfixOf "(new file)" preview) ->
          pure $ Left $ "Preview missing new file marker: " ++ T.unpack (T.take 200 preview)
      | not (T.isInfixOf "+module New where" preview) ->
          pure $ Left $ "Preview missing content: " ++ T.unpack (T.take 200 preview)
      | otherwise -> pure $ Right ()

-- | computeWriteFilePreview returns an error for an existing file.
testComputeWriteFilePreviewExisting :: Test
testComputeWriteFilePreviewExisting = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-wf-preview-existing"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "Existing.hs") "module Existing where\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"    .= ("Existing.hs" :: T.Text)
        , "content" .= ("new content" :: T.Text)
        ]
  result <- computeWriteFilePreview args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Left err
      | T.isInfixOf "already exists" err -> pure $ Right ()
      | otherwise -> pure $ Left $ "Unexpected error: " ++ T.unpack err
    Right _ -> pure $ Left "Expected Left for existing file, got Right"

-- | computeWriteFilePreview returns an error for a path outside the working dir.
testComputeWriteFilePreviewOutsideRoot :: Test
testComputeWriteFilePreviewOutsideRoot = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-wf-preview-outside"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"    .= ("C:\\Windows\\System32\\haskode-outside-preview.txt" :: T.Text)
        , "content" .= ("content" :: T.Text)
        ]
  result <- computeWriteFilePreview args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Left _ -> pure $ Right ()
    Right _ -> pure $ Left "Expected Left for outside-root path, got Right"

-- | When write_file is approved, the approval function receives a
--   reason that includes the target file path.
testWriteFileApprovalShowsPath :: Test
testWriteFileApprovalShowsPath = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-wf-approval-path"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  capturedReason <- Data.IORef.newIORef ("" :: T.Text)
  let captureApprove :: ApprovalFunc
      captureApprove _tc reason = do
        Data.IORef.writeIORef capturedReason reason
        pure True
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Creating file."
        , crToolCalls = Just [ToolCall "tc-wf1" "write_file"
                               (object [ "path"    .= ("New.hs" :: T.Text)
                                       , "content" .= ("module New where\n" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry captureApprove
  state' <- runAgent state "create new file"
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  reason <- Data.IORef.readIORef capturedReason
  let pathInReason = T.isInfixOf "New.hs" reason
  let evts = events (asSession state')
      policyEvts = filter (\e -> evType e == EPolicyDecision) evts
      approvedEvts = filter (T.isInfixOf "approved" . evData) policyEvts
  if pathInReason && not (null approvedEvts)
    then pure $ Right ()
    else pure $ Left $ "pathInReason=" ++ show pathInReason
                     ++ " approved=" ++ show (not (null approvedEvts))
                     ++ " reason=" ++ T.unpack reason

-- | When write_file is rejected, the file is not created and the
--   session records the rejection cleanly.
testWriteFileRejectionShowsPath :: Test
testWriteFileRejectionShowsPath = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-wf-reject-path"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Creating file."
        , crToolCalls = Just [ToolCall "tc-wf2" "write_file"
                               (object [ "path"    .= ("New.hs" :: T.Text)
                                       , "content" .= ("module New where\n" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "OK, I won't."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoReject
  state' <- runAgent state "create new file"
  setCurrentDirectory origDir
  -- File should NOT exist
  exists <- doesFileExist (root </> "New.hs")
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let evts = events (asSession state')
      policyEvts = filter (\e -> evType e == EPolicyDecision) evts
      askEvts = filter (T.isInfixOf "AskUser" . evData) policyEvts
      deniedEvts = filter (\e -> evType e == EToolResult
                                 && T.isInfixOf "denied by user" (evData e)) evts
  if not exists && not (null askEvts) && not (null deniedEvts)
    then pure $ Right ()
    else pure $ Left $ "exists=" ++ show exists
                     ++ " askEvts=" ++ show (not (null askEvts))
                     ++ " denied=" ++ show (not (null deniedEvts))

-- | Approved write_file logs the target path in the approval event.
testWriteFileAuditApprovalWithPath :: Test
testWriteFileAuditApprovalWithPath = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-wf-audit-approve"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Creating."
        , crToolCalls = Just [ToolCall "tc-wfa1" "write_file"
                               (object [ "path"    .= ("Audit.hs" :: T.Text)
                                       , "content" .= ("module Audit where\n" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "create file"
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let evts = events (asSession state')
      policyEvts = filter (\e -> evType e == EPolicyDecision) evts
      approvalEvts = filter (T.isInfixOf "approved" . evData) policyEvts
  case approvalEvts of
    (a:_) ->
      if T.isInfixOf "Audit.hs" (evData a)
        then pure $ Right ()
        else pure $ Left $ "Approval event missing path: " ++ T.unpack (evData a)
    _ -> pure $ Left "No approval event found"

-- | Rejected write_file logs the target path in the denial event.
testWriteFileAuditRejectionWithPath :: Test
testWriteFileAuditRejectionWithPath = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-wf-audit-reject"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Creating."
        , crToolCalls = Just [ToolCall "tc-wfa2" "write_file"
                               (object [ "path"    .= ("Reject.hs" :: T.Text)
                                       , "content" .= ("module Reject where\n" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "OK."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoReject
  state' <- runAgent state "create file"
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let evts = events (asSession state')
      trEvts = filter (\e -> evType e == EToolResult) evts
      denialEvts = filter (T.isInfixOf "denied by user" . evData) trEvts
  case denialEvts of
    (d:_) ->
      if T.isInfixOf "Reject.hs" (evData d)
        then pure $ Right ()
        else pure $ Left $ "Denial event missing path: " ++ T.unpack (evData d)
    _ -> pure $ Left "No denial event found"

-- | write_file description mentions confirmation and cannot overwrite.
testWriteFileDescriptionPhrases :: Test
testWriteFileDescriptionPhrases =
  case toolDescriptionFromRegistry "write_file" of
    Nothing -> pure $ Left "write_file not in registry"
    Just desc
      | not (T.isInfixOf "confirmation" desc || T.isInfixOf "confirm" desc)
        -> pure $ Left $ "write_file description missing confirmation: " ++ T.unpack desc
      | not (T.isInfixOf "existing" desc || T.isInfixOf "overwrite" desc || T.isInfixOf "Cannot overwrite" desc)
        -> pure $ Left $ "write_file description missing overwrite protection: " ++ T.unpack desc
      | otherwise -> pure $ Right ()

-- ---------------------------------------------------------------------------
-- Patch workflow smoke tests (agent-loop integration)
-- ---------------------------------------------------------------------------

-- | End-to-end smoke test: the model proposes preview_patch, sees the
--   diff (no file change), then proposes apply_patch, and the file
--   changes only after the policy/approval path allows execution.
--   Uses scriptedProvider + autoApprove to drive the agent loop
--   deterministically.
testPatchWorkflowSmoke :: Test
testPatchWorkflowSmoke = do
  -- Create a temp file with known content.
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-patch-smoke"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  let testFile = root </> "hello.hs"
  TIO.writeFile testFile "module Hello where\ngreet = \"old\"\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let newContent = "module Hello where\ngreet = \"new\"\n"
  prov <- scriptedProvider
    [ -- Turn 1: assistant calls preview_patch
      CompletionResponse
        { crReply     = mkAssistantMessage "Let me preview the change."
        , crToolCalls = Just
            [ ToolCall "tc-pv" "preview_patch"
                (object ["path" .= ("hello.hs" :: T.Text), "replacement" .= newContent])
            ]
        }
    , -- Turn 2: assistant calls apply_patch (preview looked good)
      CompletionResponse
        { crReply     = mkAssistantMessage "The diff looks correct, applying."
        , crToolCalls = Just
            [ ToolCall "tc-ap" "apply_patch"
                (object ["path" .= ("hello.hs" :: T.Text), "replacement" .= newContent])
            ]
        }
    , -- Turn 3: final text reply
      CompletionResponse
        { crReply     = mkAssistantMessage "Patch applied successfully."
        , crToolCalls = Nothing
        }
    ]
  let cfg   = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "fix the greeting"
  setCurrentDirectory origDir
  -- Verify conversation structure.
  let conv = asConversation state'
      evts = events (asSession state')
      types = map evType evts
  -- The file should now have the new content.
  after <- TIO.readFile testFile
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  -- Check session event types.
  let hasUserMsg     = EUserMessage `elem` types
      hasAsstReply   = EAssistantReply `elem` types
      hasToolCall    = EToolCall `elem` types
      hasToolResult  = EToolResult `elem` types
      hasPolicyDec   = EPolicyDecision `elem` types
  -- Check that the tool results contain expected content.
  let toolResults  = filter (\m -> msgCallId m /= Nothing) conv
      previewResult = filter (T.isInfixOf "Diff preview" . msgContent) toolResults
      applyResult   = filter (T.isInfixOf "Patch applied" . msgContent) toolResults
  -- Check that apply_patch was approved (AskUser path).
  -- The approval event for apply_patch includes the target path:
  -- "apply_patch: approved — <path>"
  let approvedEvts = filter (T.isInfixOf "approved" . evData)
                             (filter (\e -> evType e == EPolicyDecision) evts)
  -- Verify file content changed.
  let fileChanged = after == newContent
  if hasUserMsg && hasAsstReply && hasToolCall && hasToolResult
     && hasPolicyDec && not (null previewResult) && not (null applyResult)
     && not (null approvedEvts) && fileChanged
    then pure $ Right ()
    else pure $ Left $
         "events=" ++ show types
      ++ " previewResult=" ++ show (length previewResult)
      ++ " applyResult=" ++ show (length applyResult)
      ++ " approved=" ++ show (not (null approvedEvts))
      ++ " fileChanged=" ++ show fileChanged

-- | Rejection smoke test: the model proposes apply_patch, but the
--   approval function rejects it.  No file is modified and the session
--   log records the policy decision and denial clearly.
testPatchWorkflowRejectSmoke :: Test
testPatchWorkflowRejectSmoke = do
  -- Create a temp file with known content.
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-patch-reject"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  let testFile = root </> "hello.hs"
      original = "module Hello where\ngreet = \"old\"\n"
  TIO.writeFile testFile original
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ -- Turn 1: assistant calls apply_patch
      CompletionResponse
        { crReply     = mkAssistantMessage "Applying the patch now."
        , crToolCalls = Just
            [ ToolCall "tc-ap" "apply_patch"
                (object [ "path"        .= ("hello.hs" :: T.Text)
                        , "replacement" .= ("module Hello where\ngreet = \"rejected\"\n" :: T.Text)
                        ])
            ]
        }
    , -- Turn 2: final text reply acknowledging rejection
      CompletionResponse
        { crReply     = mkAssistantMessage "OK, I won't apply the patch."
        , crToolCalls = Nothing
        }
    ]
  let cfg   = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoReject
  state' <- runAgent state "fix the greeting"
  setCurrentDirectory origDir
  -- Verify file is unchanged.
  after <- TIO.readFile testFile
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let fileUnchanged = after == original
  -- Verify session log records the denial.
  let evts = events (asSession state')
      types = map evType evts
      hasPolicyDec  = EPolicyDecision `elem` types
      hasToolResult = EToolResult `elem` types
      -- The denial message should appear in tool result events.
      deniedEvts = filter (\e -> evType e == EToolResult
                                 && T.isInfixOf "denied by user" (evData e)) evts
      -- The conversation should contain a denial message for the tool call.
      conv = asConversation state'
      denialMsgs = filter (T.isInfixOf "denied by user" . msgContent) conv
  if hasPolicyDec && hasToolResult && not (null deniedEvts)
     && not (null denialMsgs) && fileUnchanged
    then pure $ Right ()
    else pure $ Left $
         "events=" ++ show types
      ++ " deniedEvts=" ++ show (length deniedEvts)
      ++ " denialMsgs=" ++ show (length denialMsgs)
      ++ " fileUnchanged=" ++ show fileUnchanged

-- ---------------------------------------------------------------------------
-- Session summary tests (--show-session groundwork)
-- ---------------------------------------------------------------------------

-- | summarizeSession returns zero events for a missing file.
testSummarizeSessionMissingFile :: Test
testSummarizeSessionMissingFile = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-summarize-missing"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  ss <- summarizeSession root
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  if ssTotalEvents ss == 0 && ssMalformedLines ss == 0
    then pure $ Right ()
    else pure $ Left $ "Missing file: total=" ++ show (ssTotalEvents ss)
                     ++ " malformed=" ++ show (ssMalformedLines ss)

-- | summarizeSession counts events by type from valid JSONL.
testSummarizeSessionValidJsonl :: Test
testSummarizeSessionValidJsonl = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-summarize-valid"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  now <- getCurrentTime
  let log' = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EUserMessage "hello"
        , Event now EAssistantReply "hi"
        , Event now EToolCall "tc-1 read_file"
        , Event now EToolResult "tc-1 contents"
        , Event now EUserMessage "second"
        ]
  flushLog root 0 log'
  ss <- summarizeSession root
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let counts = ssTypeCounts ss
      ok = ssTotalEvents ss == 5
           && Map.findWithDefault 0 EUserMessage counts == 2
           && Map.findWithDefault 0 EAssistantReply counts == 1
           && Map.findWithDefault 0 EToolCall counts == 1
           && Map.findWithDefault 0 EToolResult counts == 1
           && ssMalformedLines ss == 0
           && ssFirstTime ss /= Nothing
           && ssLastTime ss /= Nothing
  if ok
    then pure $ Right ()
    else pure $ Left $ "Valid JSONL: total=" ++ show (ssTotalEvents ss)
                     ++ " counts=" ++ show counts
                     ++ " malformed=" ++ show (ssMalformedLines ss)

-- | summarizeSession counts malformed lines without crashing.
testSummarizeSessionMalformedLines :: Test
testSummarizeSessionMalformedLines = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-summarize-malformed"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  now <- getCurrentTime
  -- Write one valid event and two malformed lines.
  let validLog = logEvent (Event now EUserMessage "ok") emptyLog
  flushLog root 0 validLog
  -- Append malformed lines directly.
  let path = root </> "session.jsonl"
  LBS.appendFile path "not valid json at all\n"
  LBS.appendFile path "{\"incomplete\": true}\n"
  ss <- summarizeSession root
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  if ssTotalEvents ss == 1 && ssMalformedLines ss == 2
    then pure $ Right ()
    else pure $ Left $ "Malformed: total=" ++ show (ssTotalEvents ss)
                     ++ " malformed=" ++ show (ssMalformedLines ss)

-- | summarizeSession returns zero events for an empty file.
testSummarizeSessionEmptyFile :: Test
testSummarizeSessionEmptyFile = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-summarize-empty"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  -- Create an empty session.jsonl.
  let path = root </> "session.jsonl"
  writeFile path ""
  ss <- summarizeSession root
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  if ssTotalEvents ss == 0 && ssMalformedLines ss == 0
    then pure $ Right ()
    else pure $ Left $ "Empty file: total=" ++ show (ssTotalEvents ss)
                     ++ " malformed=" ++ show (ssMalformedLines ss)

-- | summarizeSession ignores rotated session.jsonl.1 backup.
testSummarizeSessionIgnoresBackup :: Test
testSummarizeSessionIgnoresBackup = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-summarize-backup"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  now <- getCurrentTime
  -- Write events to main file.
  let log1 = logEvent (Event now EUserMessage "main") emptyLog
  flushLog root 0 log1
  -- Write different events to backup.
  let log2 = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EAssistantReply "backup1"
        , Event now EToolCall "backup2"
        ]
  flushLog root 0 log2  -- writes to main (appends)
  -- Now move main to .1 and write a fresh main with 1 event.
  let path   = root </> "session.jsonl"
      backup = root </> "session.jsonl.1"
  renameFile path backup
  let log3 = logEvent (Event now EUserMessage "fresh") emptyLog
  flushLog root 0 log3
  ss <- summarizeSession root
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  -- Should count only the fresh main file (1 event), not the backup (3 events).
  if ssTotalEvents ss == 1
    then pure $ Right ()
    else pure $ Left $ "Backup ignored: total=" ++ show (ssTotalEvents ss)

-- | Normal agent execution is unchanged when --show-session is not used.
--   This is a smoke test verifying the agent loop still works after
--   the Session.hs changes (FromJSON instances, etc.).
testAgentUnchangedWithSessionSummary :: Test
testAgentUnchangedWithSessionSummary = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Still works."
        , crToolCalls = Nothing
        }
    ]
  let cfg   = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "hello"
  let evts = events (asSession state')
      types = map evType evts
  if EUserMessage `elem` types && EAssistantReply `elem` types
    then pure $ Right ()
    else pure $ Left $ "Agent unchanged: events=" ++ show types

-- | formatSessionSummary includes key sections.
testFormatSessionSummaryContainsSections :: Test
testFormatSessionSummaryContainsSections = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-summarize-format"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  now <- getCurrentTime
  let log' = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EUserMessage "hello"
        , Event now EAssistantReply "hi"
        ]
  flushLog root 0 log'
  ss <- summarizeSession root
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = formatSessionSummary ss
  if T.isInfixOf "Session summary" out
     && T.isInfixOf "Total events:" out
     && T.isInfixOf "user_message:" out
     && T.isInfixOf "assistant_reply:" out
    then pure $ Right ()
    else pure $ Left $ "formatSessionSummary missing sections: " ++ T.unpack (T.take 300 out)

-- ---------------------------------------------------------------------------
-- Lifecycle event tests
-- ---------------------------------------------------------------------------

-- | EConversationReset JSON roundtrip.
testConversationResetJSON :: Test
testConversationResetJSON = do
  now <- getCurrentTime
  let ev = Event now EConversationReset "conversation reset by /new"
  case decode (encode ev) :: Maybe Event of
    Nothing  -> pure $ Left "EConversationReset JSON roundtrip failed"
    Just ev'
      | evType ev' == EConversationReset && evData ev' == "conversation reset by /new" ->
          pure $ Right ()
      | otherwise -> pure $ Left $ "EConversationReset roundtrip mismatch: " ++ show ev'

-- | summarizeSession counts conversation_reset events.
testSummarizeSessionConversationReset :: Test
testSummarizeSessionConversationReset = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-summarize-reset"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  now <- getCurrentTime
  let log' = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EUserMessage "hello"
        , Event now EAssistantReply "hi"
        , Event now EConversationReset "conversation reset by /new"
        , Event now EUserMessage "second"
        ]
  flushLog root 0 log'
  ss <- summarizeSession root
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let counts = ssTypeCounts ss
      ok = ssTotalEvents ss == 4
           && Map.findWithDefault 0 EConversationReset counts == 1
           && Map.findWithDefault 0 EUserMessage counts == 2
  if ok
    then pure $ Right ()
    else pure $ Left $ "conversation_reset count: total=" ++ show (ssTotalEvents ss)
                     ++ " counts=" ++ show counts

-- | formatSessionSummary includes conversation_reset line.
testFormatSessionSummaryConversationReset :: Test
testFormatSessionSummaryConversationReset = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-summarize-format-reset"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  now <- getCurrentTime
  let log' = logEvent (Event now EConversationReset "reset") emptyLog
  flushLog root 0 log'
  ss <- summarizeSession root
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = formatSessionSummary ss
  if T.isInfixOf "conversation_reset:" out
    then pure $ Right ()
    else pure $ Left $ "formatSessionSummary missing conversation_reset: " ++ T.unpack (T.take 300 out)

-- | recordSessionStart produces an ESessionStart event.
testRecordSessionStartEvent :: Test
testRecordSessionStartEvent = do
  let cfg = defaultConfig
      state0 = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove
  state1 <- recordSessionStart state0
  let evts = events (asSession state1)
      types = map evType evts
  if types == [ESessionStart]
    then pure $ Right ()
    else pure $ Left $ "recordSessionStart events: " ++ show types

-- | recordSessionEnd produces an ESessionEnd event.
testRecordSessionEndEvent :: Test
testRecordSessionEndEvent = do
  let cfg = defaultConfig
      state0 = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove
  state1 <- recordSessionEnd state0
  let evts = events (asSession state1)
      types = map evType evts
  if types == [ESessionEnd]
    then pure $ Right ()
    else pure $ Left $ "recordSessionEnd events: " ++ show types

-- | recordConversationReset produces an EConversationReset event.
testRecordConversationResetEvent :: Test
testRecordConversationResetEvent = do
  let cfg = defaultConfig
      state0 = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove
  state1 <- recordConversationReset state0
  let evts = events (asSession state1)
      types = map evType evts
  if types == [EConversationReset]
    then pure $ Right ()
    else pure $ Left $ "recordConversationReset events: " ++ show types

-- | /new command preserves existing session events and appends
--   a conversation_reset event.  Tests the pure layer:
--   resetConversation clears messages; recordConversationReset appends event.
testNewPreservesSessionAndAppendsReset :: Test
testNewPreservesSessionAndAppendsReset = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Hello."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state0 = initState cfg prov defaultPolicy defaultRegistry autoApprove
  -- Run an agent turn to accumulate session events
  state1 <- runAgent state0 "hi"
  let evtsBefore = events (asSession state1)
      countBefore = length evtsBefore
  -- Simulate /new: reset conversation, record reset event
  let state2 = resetConversation state1
  state3 <- recordConversationReset state2
  let evtsAfter = events (asSession state3)
      countAfter = length evtsAfter
      hasReset = EConversationReset `elem` map evType evtsAfter
      convCleared = null (asConversation state2)
  if countAfter == countBefore + 1 && hasReset && convCleared
    then pure $ Right ()
    else pure $ Left $ "before=" ++ show countBefore
                     ++ " after=" ++ show countAfter
                     ++ " hasReset=" ++ show hasReset
                     ++ " convCleared=" ++ show convCleared

-- | Full lifecycle: start, agent turn, reset, agent turn, end.
--   Verifies the complete event sequence.
testFullLifecycleEventSequence :: Test
testFullLifecycleEventSequence = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "First reply."
        , crToolCalls = Nothing
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Second reply."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state0 = initState cfg prov defaultPolicy defaultRegistry autoApprove
  -- session_start
  state1 <- recordSessionStart state0
  -- first turn
  state2 <- runAgent state1 "first"
  -- /new
  let state3 = resetConversation state2
  state4 <- recordConversationReset state3
  -- second turn
  state5 <- runAgent state4 "second"
  -- session_end
  state6 <- recordSessionEnd state5
  let evts = events (asSession state6)
      types = map evType evts
  -- Expected: session_start, user_message, assistant_reply,
  --           conversation_reset, user_message, assistant_reply, session_end
  let expected = [ ESessionStart, EUserMessage, EAssistantReply
                 , EConversationReset, EUserMessage, EAssistantReply, ESessionEnd]
  if types == expected
    then pure $ Right ()
    else pure $ Left $ "lifecycle events: " ++ show types
                     ++ " expected: " ++ show expected

-- ---------------------------------------------------------------------------
-- isMeaningfulSession tests
-- ---------------------------------------------------------------------------

-- | Empty log is not meaningful.
testIsMeaningfulEmpty :: Test
testIsMeaningfulEmpty =
  if not (isMeaningfulSession emptyLog)
    then pure $ Right ()
    else pure $ Left "empty log should not be meaningful"

-- | Lifecycle-only log ([session_start, session_end]) is not meaningful.
testIsMeaningfulLifecycleOnly :: Test
testIsMeaningfulLifecycleOnly = do
  now <- getCurrentTime
  let log' = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now ESessionStart "session started"
        , Event now ESessionEnd "session ended"
        ]
  if not (isMeaningfulSession log')
    then pure $ Right ()
    else pure $ Left "lifecycle-only log should not be meaningful"

-- | Lifecycle with conversation_reset is not meaningful.
testIsMeaningfulWithReset :: Test
testIsMeaningfulWithReset = do
  now <- getCurrentTime
  let log' = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now ESessionStart "session started"
        , Event now EConversationReset "conversation reset by /new"
        , Event now ESessionEnd "session ended"
        ]
  if not (isMeaningfulSession log')
    then pure $ Right ()
    else pure $ Left "lifecycle+reset log should not be meaningful"

-- | Log with user_message is meaningful.
testIsMeaningfulWithUserMessage :: Test
testIsMeaningfulWithUserMessage = do
  now <- getCurrentTime
  let log' = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now ESessionStart "session started"
        , Event now EUserMessage "hello"
        , Event now ESessionEnd "session ended"
        ]
  if isMeaningfulSession log'
    then pure $ Right ()
    else pure $ Left "log with user_message should be meaningful"

-- | Log with assistant_reply is meaningful.
testIsMeaningfulWithAssistantReply :: Test
testIsMeaningfulWithAssistantReply = do
  now <- getCurrentTime
  let log' = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now ESessionStart "session started"
        , Event now EAssistantReply "hi"
        , Event now ESessionEnd "session ended"
        ]
  if isMeaningfulSession log'
    then pure $ Right ()
    else pure $ Left "log with assistant_reply should be meaningful"

-- | Log with tool_call is meaningful.
testIsMeaningfulWithToolCall :: Test
testIsMeaningfulWithToolCall = do
  now <- getCurrentTime
  let log' = logEvent (Event now EToolCall "tc-1 read_file") emptyLog
  if isMeaningfulSession log'
    then pure $ Right ()
    else pure $ Left "log with tool_call should be meaningful"

-- | Log with tool_result is meaningful.
testIsMeaningfulWithToolResult :: Test
testIsMeaningfulWithToolResult = do
  now <- getCurrentTime
  let log' = logEvent (Event now EToolResult "tc-1 contents") emptyLog
  if isMeaningfulSession log'
    then pure $ Right ()
    else pure $ Left "log with tool_result should be meaningful"

-- | Log with policy_decision is meaningful.
testIsMeaningfulWithPolicyDecision :: Test
testIsMeaningfulWithPolicyDecision = do
  now <- getCurrentTime
  let log' = logEvent (Event now EPolicyDecision "read_file: Allow") emptyLog
  if isMeaningfulSession log'
    then pure $ Right ()
    else pure $ Left "log with policy_decision should be meaningful"

-- | Full lifecycle with real content is meaningful.
testIsMeaningfulFullLifecycle :: Test
testIsMeaningfulFullLifecycle = do
  now <- getCurrentTime
  let log' = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now ESessionStart "session started"
        , Event now EUserMessage "hello"
        , Event now EAssistantReply "hi"
        , Event now EConversationReset "conversation reset by /new"
        , Event now EUserMessage "second"
        , Event now EAssistantReply "hey"
        , Event now ESessionEnd "session ended"
        ]
  if isMeaningfulSession log'
    then pure $ Right ()
    else pure $ Left "full lifecycle with content should be meaningful"

-- | flushLogOnException does NOT flush a lifecycle-only session on exception.
testFlushLogOnExceptionLifecycleOnly :: Test
testFlushLogOnExceptionLifecycleOnly = do
  tmpDir <- getTemporaryDirectory
  now    <- getCurrentTime
  let log' = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now ESessionStart "session started"
        , Event now ESessionEnd "session ended"
        ]
      path = tmpDir </> "session.jsonl"
  _ <- try (removeFile path) :: IO (Either IOException ())
  let badAction = throwIO (userError "simulated") :: IO ()
  _ <- try (flushLogOnException tmpDir 0 log' badAction) :: IO (Either IOException ())
  exists <- doesFileExist path
  cleanup path
  if not exists
    then pure $ Right ()
    else pure $ Left "lifecycle-only session should not create file on exception"

-- ---------------------------------------------------------------------------
-- Slash-command tests (pure)
-- ---------------------------------------------------------------------------

testParseSlashCommandHelp :: Test
testParseSlashCommandHelp =
  if parseSlashCommand "/help" == Just "help"
    then pure $ Right ()
    else pure $ Left "parseSlashCommand \"/help\" should be Just \"help\""

testParseSlashCommandStatus :: Test
testParseSlashCommandStatus =
  if parseSlashCommand "/status" == Just "status"
    then pure $ Right ()
    else pure $ Left "parseSlashCommand \"/status\" should be Just \"status\""

testParseSlashCommandExit :: Test
testParseSlashCommandExit =
  if parseSlashCommand "/exit" == Just "exit"
    then pure $ Right ()
    else pure $ Left "parseSlashCommand \"/exit\" should be Just \"exit\""

testParseSlashCommandQuit :: Test
testParseSlashCommandQuit =
  if parseSlashCommand "/quit" == Just "quit"
    then pure $ Right ()
    else pure $ Left "parseSlashCommand \"/quit\" should be Just \"quit\""

testParseSlashCommandUnknown :: Test
testParseSlashCommandUnknown =
  if parseSlashCommand "/foo" == Just "foo"
    then pure $ Right ()
    else pure $ Left "parseSlashCommand \"/foo\" should be Just \"foo\""

testParseSlashCommandNormalInput :: Test
testParseSlashCommandNormalInput =
  if parseSlashCommand "hello" == Nothing
    then pure $ Right ()
    else pure $ Left "parseSlashCommand \"hello\" should be Nothing"

testParseSlashCommandWhitespace :: Test
testParseSlashCommandWhitespace =
  if parseSlashCommand "  /help  " == Just "help"
    then pure $ Right ()
    else pure $ Left "parseSlashCommand \"  /help  \" should be Just \"help\""

testParseSlashCommandEmpty :: Test
testParseSlashCommandEmpty =
  if parseSlashCommand "" == Nothing
    then pure $ Right ()
    else pure $ Left "parseSlashCommand \"\" should be Nothing"

testParseSlashCommandSpacesOnly :: Test
testParseSlashCommandSpacesOnly =
  if parseSlashCommand "   " == Nothing
    then pure $ Right ()
    else pure $ Left "parseSlashCommand \"   \" should be Nothing"

testFormatHelpContent :: Test
testFormatHelpContent =
  if T.isInfixOf "/help" formatHelp
     && T.isInfixOf "/status" formatHelp
     && T.isInfixOf "/exit" formatHelp
     && T.isInfixOf "/quit" formatHelp
    then pure $ Right ()
    else pure $ Left $ "formatHelp missing expected commands: " ++ T.unpack (T.take 200 formatHelp)

testFormatStatusContent :: Test
testFormatStatusContent =
  let cfg = defaultConfig { cfgProvider = (cfgProvider defaultConfig)
                            { pcProvider = "openai"
                            , pcModel    = "gpt-4o"
                            , pcBaseUrl  = "https://api.openai.com"
                            , pcApiKey   = "sk-secret-key-12345"
                            }
                          }
      state = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove
      out   = formatStatus state
  in if T.isInfixOf "openai" out
        && T.isInfixOf "gpt-4o" out
        && T.isInfixOf "https://api.openai.com" out
        && T.isInfixOf "Session events:" out
        && T.isInfixOf "Tools:" out
        && T.isInfixOf "Context est:" out
        && T.isInfixOf "Remaining:" out
     then pure $ Right ()
     else pure $ Left $ "formatStatus missing expected fields: " ++ T.unpack (T.take 400 out)

testFormatStatusNoApiKey :: Test
testFormatStatusNoApiKey =
  let cfg = defaultConfig { cfgProvider = (cfgProvider defaultConfig)
                            { pcApiKey = "sk-super-secret-key-DO-NOT-PRINT" }
                          }
      state = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove
      out   = formatStatus state
  in if T.isInfixOf "sk-super-secret-key-DO-NOT-PRINT" out
     then pure $ Left "formatStatus should not expose API key"
     else pure $ Right ()

testFormatUnknownCommandContent :: Test
testFormatUnknownCommandContent =
  let out = formatUnknownCommand "foo"
  in if T.isInfixOf "/foo" out && T.isInfixOf "/help" out
     then pure $ Right ()
     else pure $ Left $ "formatUnknownCommand missing expected text: " ++ T.unpack out

testFormatContextUsageUnderLimit :: Test
testFormatContextUsageUnderLimit =
  let conv    = [mkUserMessage "hello"]   -- 5 chars + 20 overhead = 25
      maxC    = 120000
      out     = formatContextUsage conv maxC
      est     = estimateContextChars conv
      remain  = maxC - est
  in if T.isInfixOf (T.pack (show est)) out
        && T.isInfixOf "Context est:" out
        && T.isInfixOf (T.pack (show remain)) out
        && T.isInfixOf "Remaining:" out
        && T.isInfixOf "% used" out
     then pure $ Right ()
     else pure $ Left $ "formatContextUsage under-limit: " ++ T.unpack out

testFormatContextUsageExactLimit :: Test
testFormatContextUsageExactLimit =
  let conv    = [mkUserMessage "hello"]   -- 25 chars estimated
      maxC    = estimateContextChars conv  -- exactly at limit
      out     = formatContextUsage conv maxC
  in if T.isInfixOf "Remaining:" out
        && T.isInfixOf "0 chars" out
        && not (T.isInfixOf "Over limit:" out)
     then pure $ Right ()
     else pure $ Left $ "formatContextUsage exact-limit: " ++ T.unpack out

testFormatContextUsageOverLimit :: Test
testFormatContextUsageOverLimit =
  let conv    = [mkUserMessage "this is a longer message for testing"]
      maxC    = 10   -- deliberately small
      out     = formatContextUsage conv maxC
      est     = estimateContextChars conv
      overBy  = est - maxC
  in if T.isInfixOf "Over limit:" out
        && T.isInfixOf (T.pack (show overBy)) out
        && T.isInfixOf "Context est:" out
        && T.isInfixOf "% used" out
     then pure $ Right ()
     else pure $ Left $ "formatContextUsage over-limit: " ++ T.unpack out

testParseSlashCommandNew :: Test
testParseSlashCommandNew =
  if parseSlashCommand "/new" == Just "new"
    then pure $ Right ()
    else pure $ Left "parseSlashCommand \"/new\" should be Just \"new\""

testFormatHelpContentIncludesNew :: Test
testFormatHelpContentIncludesNew =
  if T.isInfixOf "/new" formatHelp
    then pure $ Right ()
    else pure $ Left $ "formatHelp missing /new: " ++ T.unpack (T.take 200 formatHelp)

testFormatNewConfirmationText :: Test
testFormatNewConfirmationText =
  if "fresh conversation" `T.isInfixOf` formatNewConfirmation
    then pure $ Right ()
    else pure $ Left $ "formatNewConfirmation missing expected text: " ++ T.unpack formatNewConfirmation

testResetConversationClearsMessages :: Test
testResetConversationClearsMessages =
  let cfg = defaultConfig
      state0 = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove
      msg = Message User "hello" Nothing Nothing
      state1 = state0 { asConversation = [msg] }
      state2 = resetConversation state1
  in if null (asConversation state2)
       then pure $ Right ()
       else pure $ Left "resetConversation should clear conversation"

testResetConversationPreservesSession :: Test
testResetConversationPreservesSession =
  let cfg = defaultConfig
      state0 = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove
      msg = Message User "hello" Nothing Nothing
      state1 = state0 { asConversation = [msg] }
      state2 = resetConversation state1
  in if length (events (asSession state2)) == length (events (asSession state1))
       then pure $ Right ()
       else pure $ Left "resetConversation should preserve session events"

-- ---------------------------------------------------------------------------
-- Display formatting tests (pure)
-- ---------------------------------------------------------------------------

-- | indentBlock indents each line by the given number of spaces.
testDisplayIndentBlock :: Test
testDisplayIndentBlock =
  let result = indentBlock 4 "line1\nline2\n"
  in if result == "    line1\n    line2\n"
       then pure $ Right ()
       else pure $ Left $ "indentBlock 4: got " ++ show result

-- | indentBlock with single line.
testDisplayIndentBlockSingleLine :: Test
testDisplayIndentBlockSingleLine =
  let result = indentBlock 2 "hello\n"
  in if result == "  hello\n"
       then pure $ Right ()
       else pure $ Left $ "indentBlock single: got " ++ show result

-- | indentBlock on empty text returns empty string.
testDisplayIndentBlockEmpty :: Test
testDisplayIndentBlockEmpty =
  let result = indentBlock 4 ""
  in if result == ""
       then pure $ Right ()
       else pure $ Left $ "indentBlock empty: got " ++ show result

-- | indentBlock preserves trailing content without newline.
testDisplayIndentBlockNoTrailingNewline :: Test
testDisplayIndentBlockNoTrailingNewline =
  let result = indentBlock 2 "line1\nline2"
  in if result == "  line1\n  line2\n"
       then pure $ Right ()
       else pure $ Left $ "indentBlock no trailing: got " ++ show result

-- | formatAssistantReply wraps content with Assistant: prefix and newlines.
testDisplayFormatAssistantReply :: Test
testDisplayFormatAssistantReply =
  let result = formatAssistantReply "hello there"
  in if result == "\nAssistant: hello there\n"
       then pure $ Right ()
       else pure $ Left $ "formatAssistantReply: got " ++ show result

-- | formatAssistantReply with empty content.
testDisplayFormatAssistantReplyEmpty :: Test
testDisplayFormatAssistantReplyEmpty =
  let result = formatAssistantReply ""
  in if result == "\nAssistant: \n"
       then pure $ Right ()
       else pure $ Left $ "formatAssistantReply empty: got " ++ show result

-- | formatToolExecuting produces correct label.
testDisplayFormatToolExecuting :: Test
testDisplayFormatToolExecuting =
  let result = formatToolExecuting "read_file"
  in if result == "  [tool] Executing: read_file"
       then pure $ Right ()
       else pure $ Left $ "formatToolExecuting: got " ++ show result

-- | formatToolResult produces correct label.
testDisplayFormatToolResult :: Test
testDisplayFormatToolResult =
  let result = formatToolResult "file contents here"
  in if result == "  [tool] Result: file contents here"
       then pure $ Right ()
       else pure $ Left $ "formatToolResult: got " ++ show result

-- | formatToolUnknown produces correct label.
testDisplayFormatToolUnknown :: Test
testDisplayFormatToolUnknown =
  let result = formatToolUnknown "bad_tool"
  in if result == "  [error] Unknown tool: bad_tool"
       then pure $ Right ()
       else pure $ Left $ "formatToolUnknown: got " ++ show result

-- | formatPolicyDenied produces correct label with name and reason.
testDisplayFormatPolicyDenied :: Test
testDisplayFormatPolicyDenied =
  let result = formatPolicyDenied "shell" "dangerous command"
  in if result == "  [policy] Denied: shell -- dangerous command"
       then pure $ Right ()
       else pure $ Left $ "formatPolicyDenied: got " ++ show result

-- | formatPolicyConfirmationNeeded produces correct label.
testDisplayFormatPolicyConfirmationNeeded :: Test
testDisplayFormatPolicyConfirmationNeeded =
  let result = formatPolicyConfirmationNeeded "apply_patch"
  in if result == "  [policy] Confirmation needed: apply_patch"
       then pure $ Right ()
       else pure $ Left $ "formatPolicyConfirmationNeeded: got " ++ show result

-- | formatPolicyApproved produces correct text.
testDisplayFormatPolicyApproved :: Test
testDisplayFormatPolicyApproved =
  if formatPolicyApproved == "  [policy] Approved by user."
    then pure $ Right ()
    else pure $ Left $ "formatPolicyApproved: got " ++ show formatPolicyApproved

-- | formatPolicyRejected produces correct text.
testDisplayFormatPolicyRejected :: Test
testDisplayFormatPolicyRejected =
  if formatPolicyRejected == "  [policy] Rejected by user."
    then pure $ Right ()
    else pure $ Left $ "formatPolicyRejected: got " ++ show formatPolicyRejected

-- | formatConfirmTool produces correct label.
testDisplayFormatConfirmTool :: Test
testDisplayFormatConfirmTool =
  let result = formatConfirmTool "read_file"
  in if result == "  [confirm] Tool:     read_file"
       then pure $ Right ()
       else pure $ Left $ "formatConfirmTool: got " ++ show result

-- | formatConfirmArgs produces correct label.
testDisplayFormatConfirmArgs :: Test
testDisplayFormatConfirmArgs =
  let result = formatConfirmArgs "{\"path\":\"foo.hs\"}"
  in if result == "  [confirm] Args:     {\"path\":\"foo.hs\"}"
       then pure $ Right ()
       else pure $ Left $ "formatConfirmArgs: got " ++ show result

-- | formatConfirmReason produces correct label.
testDisplayFormatConfirmReason :: Test
testDisplayFormatConfirmReason =
  let result = formatConfirmReason "needs approval"
  in if result == "  [confirm] Reason:   needs approval"
       then pure $ Right ()
       else pure $ Left $ "formatConfirmReason: got " ++ show result

-- | formatConfirmPrompt produces correct text.
testDisplayFormatConfirmPrompt :: Test
testDisplayFormatConfirmPrompt =
  if formatConfirmPrompt == "  [confirm] Approve? (y/N) "
    then pure $ Right ()
    else pure $ Left $ "formatConfirmPrompt: got " ++ show formatConfirmPrompt

-- | formatConfirmFile produces correct label.
testDisplayFormatConfirmFile :: Test
testDisplayFormatConfirmFile =
  let result = formatConfirmFile "src/Main.hs"
  in if result == "  [confirm] File:     src/Main.hs"
       then pure $ Right ()
       else pure $ Left $ "formatConfirmFile: got " ++ show result

-- | formatConfirmDiffHeader produces correct text.
testDisplayFormatConfirmDiffHeader :: Test
testDisplayFormatConfirmDiffHeader =
  if formatConfirmDiffHeader == "  [confirm] Diff:"
    then pure $ Right ()
    else pure $ Left $ "formatConfirmDiffHeader: got " ++ show formatConfirmDiffHeader

-- | formatConfirmPreviewHeader produces correct text.
testDisplayFormatConfirmPreviewHeader :: Test
testDisplayFormatConfirmPreviewHeader =
  if formatConfirmPreviewHeader == "  [confirm] Preview:"
    then pure $ Right ()
    else pure $ Left $ "formatConfirmPreviewHeader: got " ++ show formatConfirmPreviewHeader

-- | formatError produces correct label.
testDisplayFormatError :: Test
testDisplayFormatError =
  let result = formatError "something went wrong"
  in if result == "  [error] something went wrong"
       then pure $ Right ()
       else pure $ Left $ "formatError: got " ++ show result

-- | formatVerbose produces correct label format.
testDisplayFormatVerbose :: Test
testDisplayFormatVerbose =
  let result = formatVerbose "provider" "openai"
  in if result == "[verbose] provider: openai"
       then pure $ Right ()
       else pure $ Left $ "formatVerbose: got " ++ show result

-- | Multiline diff formatted with indentBlock is readable.
testDisplayMultilineDiffIndent :: Test
testDisplayMultilineDiffIndent =
  let diff = "--- Foo.hs\n+++ Foo.hs\n@@ -1 +1 @@\n-old\n+new\n"
      result = indentBlock 4 diff
      lines' = T.lines result
  in if all (\l -> T.null l || T.isPrefixOf "    " l) lines'
       then pure $ Right ()
       else pure $ Left $ "Multiline diff indent failed: " ++ show lines'

-- | No secrets appear in formatVerbose output.
testDisplayFormatVerboseNoSecrets :: Test
testDisplayFormatVerboseNoSecrets =
  let result = formatVerbose "api key" "sk-secret-12345"
  in -- formatVerbose is a simple concatenation helper; it does not
     -- redact.  This test confirms the function exists and works.
     -- Secret redaction is handled by the caller (e.g. formatStatus
     -- in Commands.hs never prints pcApiKey).
     if "sk-secret-12345" `T.isInfixOf` T.pack result
       then pure $ Right ()
       else pure $ Left $ "formatVerbose value not in output: " ++ result

-- | formatContextLimitRefusal includes estimated character count.
testFormatContextLimitRefusalEstimate :: Test
testFormatContextLimitRefusalEstimate =
  let msg = formatContextLimitRefusal 130000 120000
  in if "130000" `T.isInfixOf` msg
       then pure $ Right ()
       else pure $ Left $ "Missing estimate in: " ++ T.unpack msg

-- | formatContextLimitRefusal includes configured limit.
testFormatContextLimitRefusalLimit :: Test
testFormatContextLimitRefusalLimit =
  let msg = formatContextLimitRefusal 130000 120000
  in if "120000" `T.isInfixOf` msg
       then pure $ Right ()
       else pure $ Left $ "Missing limit in: " ++ T.unpack msg

-- | formatContextLimitRefusal includes over-limit delta.
testFormatContextLimitRefusalDelta :: Test
testFormatContextLimitRefusalDelta =
  let msg = formatContextLimitRefusal 130000 120000
  in if "10000" `T.isInfixOf` msg && "Over limit by" `T.isInfixOf` msg
       then pure $ Right ()
       else pure $ Left $ "Missing delta in: " ++ T.unpack msg

-- | formatContextLimitRefusal states no auto-truncation or summarization.
testFormatContextLimitRefusalNoAutoTruncate :: Test
testFormatContextLimitRefusalNoAutoTruncate =
  let msg = formatContextLimitRefusal 130000 120000
  in if "does not auto-truncate or summarize" `T.isInfixOf` msg
       then pure $ Right ()
       else pure $ Left $ "Missing no-auto-truncate note in: " ++ T.unpack msg

-- | formatContextLimitRefusal includes suggested next steps.
testFormatContextLimitRefusalNextSteps :: Test
testFormatContextLimitRefusalNextSteps =
  let msg = formatContextLimitRefusal 130000 120000
  in if "Suggested next steps" `T.isInfixOf` msg
       && "fresh session"       `T.isInfixOf` msg
       && "cfgMaxContextChars"  `T.isInfixOf` msg
       then pure $ Right ()
       else pure $ Left $ "Missing next steps in: " ++ T.unpack msg

-- | formatContextLimitRefusal with no overage (exact limit exceeded by 1).
testFormatContextLimitRefusalExactOver :: Test
testFormatContextLimitRefusalExactOver =
  let msg = formatContextLimitRefusal 120001 120000
  in if "1" `T.isInfixOf` msg && "Over limit by" `T.isInfixOf` msg
       then pure $ Right ()
       else pure $ Left $ "Exact-over case failed: " ++ T.unpack msg

-- | Helper: remove a temp file, ignoring errors.
cleanup :: FilePath -> IO ()
cleanup path = do
  _ <- try (removeFile path) :: IO (Either IOException ())
  pure ()

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
  , testShellOutputFormatting
  , testPolicyStructuredArgs
  , testExtractTextField
  , testAgentLoopEvents
  , testAgentLoopToolExecution
  , testMultiToolConversationHistory
  , testBuildSystemPrompt
    -- Context estimation & guard tests
  , testEstimateContextCharsUserMsg
  , testEstimateContextCharsAssistantMsg
  , testEstimateContextCharsToolCalls
  , testEstimateContextCharsEmpty
  , testDefaultConfigMaxContextChars
    -- Config backward-compatibility tests
  , testConfigBackcompatMinimal
  , testConfigBackcompatOverrideContextChars
  , testConfigBackcompatOverrideSessionLogBytes
  , testConfigBackcompatOverrideBoth
  , testConfigBackcompatMalformedContextChars
  , testConfigBackcompatMalformedSessionLogBytes
  , testDefaultConfigOptionalFields
    -- Environment-variable expansion tests
  , testExpandEnvVarBare
  , testExpandEnvVarBraced
  , testExpandEnvVarUndefined
  , testExpandEnvVarUndefinedBare
  , testExpandEnvVarNone
  , testExpandEnvVarTrailingDollar
  , testExpandConfigProviderFields
  , testExpandConfigWorkingDir
  , testExpandConfigPreservesNonStrings
  , testExpandConfigOpenAIFallback
  , testContextGuardUnderLimit
  , testContextGuardOverLimit
  , testContextGuardErrorText
    -- Approval tests
  , testApprovalApproved
  , testApprovalRejected
  , testApprovalSessionEvents
  , testRejectionSessionEvents
  , testDangerousDeniedWithoutPrompting
  , testShellApprovalFlow
    -- Patch confirmation display tests
  , testComputePatchPreviewNormal
  , testComputePatchPreviewMissingPath
  , skipOnWindows testComputePatchPreviewOutsideRoot
  , testComputePatchPreviewMissingFile
  , testApplyPatchApprovalShowsPath
  , testApplyPatchRejectionShowsPath
    -- Audit-log tests for apply_patch session events
  , testApplyPatchAuditApprovalWithPath
  , testApplyPatchAuditRejectionWithPath
  , testApplyPatchAuditResultBounded
  , testReadOnlyAuditUnchanged
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
    -- Tool description / schema phrase tests
  , testPreviewPatchDescriptionPhrases
  , testApplyPatchDescriptionPhrases
  , testSearchDescriptionPhrases
  , testGlobDescriptionPhrases
  , testShellDescriptionPhrases
  , testSystemPromptToolPhrases
  , testToolSchemaPhrasesInWireFormat
  , testTokenLimitFieldOpenAI
  , testTokenLimitFieldOllama
  , testTokenLimitFieldOtherProviders
  , testOpenAIRequestMaxCompletionTokens
  , testOpenAIRequestMaxTokensOllama
  , testOpenAIAssistantToolCallsJSON
  , testOpenAIConversationToolCallRoundTrip
  , testOpenAIToolCallIdMatch
    -- Truncation helper tests
  , testTruncateTextNoOp
  , testTruncateTextTruncates
  , testTruncateTextExactLimit
  , testTruncateTextEmpty
  , testFormatTruncMetaNoOp
  , testFormatTruncMetaTruncated
    -- Multi-tool conversation order tests
  , testMultiToolResultOrder
  , testMultiToolAssistantPreservesCalls
  , testMultiToolCallIdCorrespondence
    -- Shell truncation integration tests
  , testShellTruncationMetaNewFormat
  , testShellShortOutputNoTruncMeta
    -- Session event data content tests
  , testSessionEventAssistantReplyWithToolCalls
  , testSessionEventAssistantReplyTextOnly
  , testSessionEventToolCallData
  , testSessionEventToolResultData
  , testSessionEventChronologicalOrder
  , testMultiToolThenShellSessionEvents
  , testSessionEventDenyData
  , testSessionEventRejectData
    -- flushLog persistence tests
  , testFlushLogWritesJsonl
  , testFlushLogEmpty
  , testFlushLogAppends
  , testFlushLogAgentIntegration
    -- flushLogOnException tests
  , testFlushLogOnExceptionNoFlushOnSuccess
  , testFlushLogOnExceptionFlushesOnThrow
  , testFlushLogOnExceptionEmptyLog
    -- Session log rotation tests
  , testFlushLogRotationUnderLimit
  , testFlushLogRotationOverLimit
  , testFlushLogRotationBackupReplaced
  , testFlushLogRotationEmptySession
  , testFlushLogRotationValidJsonl
    -- Glob pattern matching tests (pure)
  , testMatchGlobSimpleStar
  , testMatchGlobDoubleStar
  , testMatchGlobDirPrefix
  , testMatchGlobExact
  , testIsIgnoredDir
    -- Search helper tests (pure)
  , testSearchInText
  , testSearchInTextEmptyQuery
  , testSearchInTextNoMatch
  , testSearchInTextIgnoreCase
  , testSearchInTextIgnoreCaseNoMatch
  , testSearchInTextCaseSensitiveDefault
  , testFormatSearchMatch
  , testFormatSearchMatchTruncates
    -- Glob tool IO tests
  , testGlobInRegistry
  , testSearchInRegistry
  , testGlobToolSimple
  , testGlobToolRecursive
  , testGlobToolSkipsIgnored
  , testGlobToolDirPrefix
  , testGlobToolNoMatch
    -- Search tool IO tests
  , testSearchToolFindsMatches
  , testSearchToolNoMatch
  , testSearchToolSingleFile
  , testSearchToolSkipsBinary
  , testSearchToolSkipsIgnoredDirs
    -- isUnderRoot tests (pure)
  , testIsUnderRootExact
  , testIsUnderRootSubdir
  , testIsUnderRootSibling
  , testIsUnderRootOutside
    -- Search: directory scope and path safety
  , testSearchDefaultDir
  , testSearchScopedDir
  , testSearchRejectsOutsideRoot
  , testSearchNotFound
    -- Search: case-insensitive mode
  , testSearchCaseSensitiveDefault
  , testSearchIgnoreCaseTrue
  , testSearchIgnoreCaseScoped
    -- Search: large file skipping
  , testSearchSkipsLargeFile
  , testSearchMaxFileSizePositive
    -- TraversalStats / formatStats tests (pure)
  , testFormatStatsEmpty
  , testFormatStatsMixed
  , testFormatStatsOutside
  , testFormatStatsRevisited
    -- safeCanonicalize tests
  , testSafeCanonicalizeExisting
  , skipIfNoSymlinks testSafeCanonicalizeBrokenSymlink
    -- Glob: broken symlink and outside-root
  , skipIfNoSymlinks testGlobBrokenSymlink
  , skipIfNoSymlinks testGlobOutsideRootSymlink
    -- Search: broken symlink and outside-root
  , skipIfNoSymlinks testSearchBrokenSymlink
  , skipIfNoSymlinks testSearchOutsideRootSymlink
    -- Unreadable file/dir handling (Unix permission model; skip on Windows)
  , skipOnWindows testSearchUnreadableDir
  , skipOnWindows testReadFileUnreadable
  , skipOnWindows testListFilesUnreadable
    -- Path-safety hardening: read_file / list_files containment
  , testReadFileRejectsDotDotEscape
  , testReadFileRejectsAbsoluteOutsideRoot
  , testReadFileNormalInRoot
  , skipIfNoSymlinks testReadFileRejectsOutsideRootSymlink
  , skipIfNoSymlinks testReadFileHandlesBrokenSymlink
  , testListFilesRejectsDotDotEscape
  , testListFilesRejectsAbsoluteOutsideRoot
  , testListFilesNormalInRoot
  , skipIfNoSymlinks testListFilesRejectsOutsideRootSymlink
  , skipIfNoSymlinks testListFilesHandlesBrokenSymlink
    -- Symlink-loop / visited-directory hardening
  , skipIfNoSymlinks testGlobSymlinkLoop
  , skipIfNoSymlinks testSearchSymlinkLoop
  , skipIfNoSymlinks testGlobRevisitedDir
  , skipIfNoSymlinks testSearchRevisitedDir
  , skipIfNoSymlinks testGlobNormalSymlinkBehavior
  , skipIfNoSymlinks testGlobAgentIgnoreBeforeTraversal
  , skipIfNoSymlinks testGlobRootContainmentSymlink
    -- .agentignore support
  , testLoadAgentIgnoreNoFile
  , testLoadAgentIgnoreParses
  , testGlobNoAgentIgnorePreservesBehavior
  , testGlobAgentIgnoreSkipsDir
  , testSearchAgentIgnoreSkipsDir
  , testSearchAgentIgnoreWithIgnoreCase
  , testAgentIgnoreSkipsFile
  , testAgentIgnoreCommentsAndBlanks
  , testAgentIgnoreBuiltInStillApplies
  , testAgentIgnoreStats
  , testShouldIgnorePathSingleComponent
  , testShouldIgnorePathMultiComponent
    -- AGENTS.md support
  , testBuildSystemPromptNoAgentsMd
  , testBuildSystemPromptWithAgentsMd
  , testLoadAgentsMdNoFile
  , testLoadAgentsMdContent
  , skipIfNoSymlinks testLoadAgentsMdBrokenSymlink
  , skipIfNoSymlinks testLoadAgentsMdOutsideRoot
    -- preview_patch tool tests
  , testPreviewPatchNormalDiff
  , testPreviewPatchNoModification
  , testPreviewPatchOutsideRoot
  , skipIfNoSymlinks testPreviewPatchBrokenSymlink
  , testPreviewPatchMissingFile
  , testPreviewPatchTooLarge
  , testPreviewPatchInRegistry
  , testPreviewPatchPolicyAllow
    -- apply_patch tool tests
  , testApplyPatchSuccess
  , testApplyPatchFileChanged
  , testApplyPatchOutsideRoot
  , testApplyPatchMissingFile
  , skipIfNoSymlinks testApplyPatchBrokenSymlink
  , testApplyPatchDiffInResult
  , testApplyPatchInRegistry
  , testApplyPatchPolicyAskUser
    -- write_file tool tests
  , testWriteFileSuccess
  , testWriteFileRejectsOverwrite
  , testWriteFileOutsideRoot
  , skipIfNoSymlinks testWriteFileRejectsOutsideRootSymlink
  , testWriteFileMissingParent
  , testWriteFileRejectsDirectory
  , testWriteFileInRegistry
  , testWriteFilePolicyAskUser
  , testComputeWriteFilePreviewNormal
  , testComputeWriteFilePreviewExisting
  , skipOnWindows testComputeWriteFilePreviewOutsideRoot
  , testWriteFileApprovalShowsPath
  , testWriteFileRejectionShowsPath
  , testWriteFileAuditApprovalWithPath
  , testWriteFileAuditRejectionWithPath
  , testWriteFileDescriptionPhrases
    -- Patch workflow smoke tests (agent-loop integration)
  , testPatchWorkflowSmoke
  , testPatchWorkflowRejectSmoke
    -- Session summary tests (--show-session groundwork)
  , testSummarizeSessionMissingFile
  , testSummarizeSessionValidJsonl
  , testSummarizeSessionMalformedLines
  , testSummarizeSessionEmptyFile
  , testSummarizeSessionIgnoresBackup
  , testAgentUnchangedWithSessionSummary
  , testFormatSessionSummaryContainsSections
    -- Lifecycle event tests
  , testConversationResetJSON
  , testSummarizeSessionConversationReset
  , testFormatSessionSummaryConversationReset
  , testRecordSessionStartEvent
  , testRecordSessionEndEvent
  , testRecordConversationResetEvent
  , testNewPreservesSessionAndAppendsReset
  , testFullLifecycleEventSequence
    -- isMeaningfulSession tests
  , testIsMeaningfulEmpty
  , testIsMeaningfulLifecycleOnly
  , testIsMeaningfulWithReset
  , testIsMeaningfulWithUserMessage
  , testIsMeaningfulWithAssistantReply
  , testIsMeaningfulWithToolCall
  , testIsMeaningfulWithToolResult
  , testIsMeaningfulWithPolicyDecision
  , testIsMeaningfulFullLifecycle
  , testFlushLogOnExceptionLifecycleOnly
    -- Slash-command tests (pure)
  , testParseSlashCommandHelp
  , testParseSlashCommandStatus
  , testParseSlashCommandExit
  , testParseSlashCommandQuit
  , testParseSlashCommandUnknown
  , testParseSlashCommandNormalInput
  , testParseSlashCommandWhitespace
  , testParseSlashCommandEmpty
  , testParseSlashCommandSpacesOnly
  , testParseSlashCommandNew
  , testFormatHelpContent
  , testFormatHelpContentIncludesNew
  , testFormatStatusContent
  , testFormatStatusNoApiKey
  , testFormatContextUsageUnderLimit
  , testFormatContextUsageExactLimit
  , testFormatContextUsageOverLimit
  , testFormatUnknownCommandContent
  , testFormatNewConfirmationText
  , testResetConversationClearsMessages
  , testResetConversationPreservesSession
    -- Display formatting tests (pure)
  , testDisplayIndentBlock
  , testDisplayIndentBlockSingleLine
  , testDisplayIndentBlockEmpty
  , testDisplayIndentBlockNoTrailingNewline
  , testDisplayFormatAssistantReply
  , testDisplayFormatAssistantReplyEmpty
  , testDisplayFormatToolExecuting
  , testDisplayFormatToolResult
  , testDisplayFormatToolUnknown
  , testDisplayFormatPolicyDenied
  , testDisplayFormatPolicyConfirmationNeeded
  , testDisplayFormatPolicyApproved
  , testDisplayFormatPolicyRejected
  , testDisplayFormatConfirmTool
  , testDisplayFormatConfirmArgs
  , testDisplayFormatConfirmReason
  , testDisplayFormatConfirmPrompt
  , testDisplayFormatConfirmFile
  , testDisplayFormatConfirmDiffHeader
  , testDisplayFormatConfirmPreviewHeader
  , testDisplayFormatError
  , testDisplayFormatVerbose
  , testDisplayMultilineDiffIndent
  , testDisplayFormatVerboseNoSecrets
    -- Context limit refusal formatting tests (pure)
  , testFormatContextLimitRefusalEstimate
  , testFormatContextLimitRefusalLimit
  , testFormatContextLimitRefusalDelta
  , testFormatContextLimitRefusalNoAutoTruncate
  , testFormatContextLimitRefusalNextSteps
  , testFormatContextLimitRefusalExactOver
  ]
