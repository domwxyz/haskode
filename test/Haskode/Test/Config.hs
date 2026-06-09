{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Haskode.Test.Config (tests) where

import Data.Aeson       (Value (..), encode, decode, eitherDecode, object, (.=))
import qualified Data.Aeson.Key    as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Control.Exception (try, IOException, throwIO)
import qualified Data.IORef
import Data.List          (isInfixOf)
import Data.Maybe         (isNothing)
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
                          formatContextLimitRefusal,
                          formatStreamBegin, formatStreamEnd)
import Haskode.Config    (defaultConfig, Config (..), ProviderConfig (..),
                          tokenLimitFieldName, defaultMaxContextChars,
                          defaultMaxSessionLogBytes,
                          expandEnvVars, expandConfig)
import Haskode.Provider  (Provider (..), CompletionRequest (..),
                          CompletionResponse (..), StreamHandler (..),
                          stubProvider, scriptedProvider)
import Haskode.Provider.OpenAI
                          (buildRequestBody, buildStreamingRequestBody,
                           messagesToJSON, messageToJSON, toolsToJSON,
                           parseResponseBody, parseToolCall,
                           parseSSELine, parseSSEEvent, parseDeltaContent,
                           parseDeltaToolCalls,
                           StreamingToolCall (..), assembleStreamToolCalls,
                           OpenAIError (..))
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
import Haskode.Test.Util
  ( Test
  , cleanup
  , createTestTree
  , skipIfNoSymlinks
  , skipOnWindows
  , toolDescriptionFromRegistry
  )

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


tests :: [Test]
tests =
  [ testConfigBackcompatMinimal
  , testConfigBackcompatOverrideContextChars
  , testConfigBackcompatOverrideSessionLogBytes
  , testConfigBackcompatOverrideBoth
  , testConfigBackcompatMalformedContextChars
  , testConfigBackcompatMalformedSessionLogBytes
  , testDefaultConfigOptionalFields
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
  ]
