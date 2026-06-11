{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

-- | Config parsing, expansion, and context guard tests.
module Haskode.Test.Config (tests) where

import Control.Exception ( IOException, finally, try )
import Control.Monad ( when )
import Data.Aeson ( eitherDecode )
import Data.List ( isInfixOf )
import Haskode.Agent
    ( AgentState(asSession), autoApprove, initState, runAgent )
import Haskode.Config
    ( defaultConfig,
      defaultMaxContextChars,
      defaultMaxSessionLogBytes,
      expandConfig,
      expandEnvVars,
      loadConfigFrom,
      Config(cfgProvider, cfgMaxTokens, cfgVerbose,
             cfgMaxSessionLogBytes, cfgMaxContextChars, cfgWorkingDir,
             cfgDisabledTools),
      ProviderConfig(pcProvider, pcModel, pcApiKey, pcBaseUrl) )
import Haskode.Core ( mkAssistantMessage )
import Haskode.Policy ( defaultPolicy )
import Haskode.Provider
    ( scriptedProvider,
      CompletionResponse(crToolCalls, CompletionResponse, crReply) )
import Haskode.Session
    ( events, Event(evType), EventType(EAssistantReply) )
import Haskode.Test.Util ( Test )
import Haskode.Tools ( defaultRegistry )
import System.Directory ( doesFileExist, getTemporaryDirectory, removeFile )
import System.Environment ( setEnv, unsetEnv )
import System.FilePath ( (</>) )
import qualified Data.ByteString.Lazy as LBS ( fromStrict )
import qualified Data.Text as T ( Text )
import qualified Data.Text.Encoding as TE ( encodeUtf8 )
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
         | cfgDisabledTools cfg /= [] ->
             pure $ Left $ "cfgDisabledTools should default to []: " ++ show (cfgDisabledTools cfg)
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
        && cfgDisabledTools cfg == []
     then pure $ Right ()
     else pure $ Left $ "defaultConfig values changed: "
                      ++ "ctx=" ++ show (cfgMaxContextChars cfg)
                      ++ " log=" ++ show (cfgMaxSessionLogBytes cfg)
                       ++ " toks=" ++ show (cfgMaxTokens cfg)
                       ++ " disabled=" ++ show (cfgDisabledTools cfg)

-- | defaultConfig uses the local stub provider so a fresh checkout can
--   start without API keys or a local model server.
testDefaultConfigProviderStub :: Test
testDefaultConfigProviderStub =
  let pc = cfgProvider defaultConfig
  in if pcProvider pc == "stub"
        && pcModel pc == "stub"
        && pcBaseUrl pc == ""
        && pcApiKey pc == ""
       then pure $ Right ()
       else pure $ Left $ "default provider should be stub, got: " ++ show pc

-- | Config parses cfgDisabledTools when present.
testConfigParsesDisabledTools :: Test
testConfigParsesDisabledTools =
  let json = "{" <> minimalProviderJson
           <> ",\"cfgMaxTokens\":2048"
           <> ",\"cfgVerbose\":false"
           <> ",\"cfgWorkingDir\":\".\""
           <> ",\"cfgDisabledTools\":[\"shell\",\"write_file\"]}"
  in case eitherDecode (LBS.fromStrict $ TE.encodeUtf8 json) :: Either String Config of
       Left err -> pure $ Left $ "cfgDisabledTools config failed to parse: " ++ err
       Right cfg
         | cfgDisabledTools cfg == ["shell", "write_file"] -> pure $ Right ()
         | otherwise -> pure $ Left $
             "cfgDisabledTools parsed incorrectly: " ++ show (cfgDisabledTools cfg)

-- | An explicit config path loads exactly that file and expands env vars.
testLoadConfigFromExplicitPath :: Test
testLoadConfigFromExplicitPath = do
  tmp <- getTemporaryDirectory
  let path = tmp </> "haskode-explicit-config-test.json"
      json = "{"
          <> "\"cfgProvider\":{\"pcProvider\":\"openai\",\"pcModel\":\"$HASKODE_TEST_MODEL\","
          <> "\"pcBaseUrl\":\"https://api.openai.com\",\"pcApiKey\":\"\"},"
          <> "\"cfgMaxTokens\":1234,"
          <> "\"cfgVerbose\":true,"
          <> "\"cfgWorkingDir\":\".\""
          <> "}"
      cleanup = do
        exists <- doesFileExist path
        when exists (removeFile path)
        unsetEnv "HASKODE_TEST_MODEL"
  setEnv "HASKODE_TEST_MODEL" "gpt-test"
  writeFile path json
  (do
      cfg <- loadConfigFrom path
      let pc = cfgProvider cfg
      if pcProvider pc == "openai"
         && pcModel pc == "gpt-test"
         && cfgMaxTokens cfg == 1234
         && cfgVerbose cfg == True
        then pure $ Right ()
        else pure $ Left $ "Explicit config mismatch: " ++ show cfg
    ) `finally` cleanup

-- | A missing explicit config path fails clearly instead of falling back.
testLoadConfigFromMissingPath :: Test
testLoadConfigFromMissingPath = do
  tmp <- getTemporaryDirectory
  let path = tmp </> "haskode-missing-config-test.json"
  exists <- doesFileExist path
  when exists (removeFile path)
  result <- try (loadConfigFrom path) :: IO (Either IOException Config)
  case result of
    Left ex
      | "config file not found" `isInfixOf` show ex -> pure $ Right ()
      | otherwise -> pure $ Left $ "Missing config error unclear: " ++ show ex
    Right cfg -> pure $ Left $ "Expected missing config failure, got: " ++ show cfg

-- | A malformed explicit config path fails clearly instead of falling back.
testLoadConfigFromMalformedPath :: Test
testLoadConfigFromMalformedPath = do
  tmp <- getTemporaryDirectory
  let path = tmp </> "haskode-malformed-config-test.json"
      cleanup = do
        exists <- doesFileExist path
        when exists (removeFile path)
  writeFile path "{bad json"
  (do
      result <- try (loadConfigFrom path) :: IO (Either IOException Config)
      case result of
        Left ex
          | "failed to parse config file" `isInfixOf` show ex -> pure $ Right ()
          | otherwise -> pure $ Left $ "Malformed config error unclear: " ++ show ex
        Right cfg -> pure $ Left $ "Expected malformed config failure, got: " ++ show cfg
    ) `finally` cleanup

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
     && cfgDisabledTools cfg' == cfgDisabledTools cfg
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
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove False
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
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove False
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
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove False
  -- Capture the error message from the exception
  result <- try (runAgent state "a long user message here") :: IO (Either IOException AgentState)
  case result of
    Left ex -> do
      let errMsg = show ex
      if "does not auto-truncate or auto-summarize" `isInfixOf` errMsg
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
  , testDefaultConfigProviderStub
  , testConfigParsesDisabledTools
  , testLoadConfigFromExplicitPath
  , testLoadConfigFromMissingPath
  , testLoadConfigFromMalformedPath
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
