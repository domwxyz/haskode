{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Configuration loading and representation.
--
-- Haskode looks for a @haskode.json@ (or @haskode.jsonc@) in the current
-- working directory, falling back to @~\/.config\/haskode\/config.json@.
-- The config file is optional; sensible defaults apply when it is absent.
--
-- Future work:
--   * JSONC / YAML support
--   * Per-project overrides

module Haskode.Config
  ( Config (..)
  , ProviderConfig (..)
  , defaultConfig
  , defaultMaxContextChars
  , defaultMaxSessionLogBytes
  , loadConfig
  , loadConfigFrom
  , tokenLimitFieldName
  , expandEnvVars
  , expandConfig
  ) where

import Data.Aeson            (FromJSON, ToJSON, eitherDecode, parseJSON, withObject, (.:), (.:?))
import Data.Aeson.Types      ((.!=))
import Data.Char             (isAlpha, isAlphaNum)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Text             (Text)
import GHC.Generics          (Generic)
import System.Directory      (doesFileExist, getHomeDirectory)
import System.Environment    (lookupEnv)
import System.FilePath       ((</>))

-- ---------------------------------------------------------------------------
-- Provider config
-- ---------------------------------------------------------------------------

-- | Settings for the LLM provider.
data ProviderConfig = ProviderConfig
  { pcProvider :: !String   -- ^ e.g. "openai", "anthropic", "ollama"
  , pcModel    :: !String   -- ^ Model identifier
  , pcBaseUrl  :: !String   -- ^ API base URL (empty = provider default)
  , pcApiKey   :: !String   -- ^ API key (may come from env var instead)
  } deriving stock (Show, Eq, Generic)

instance ToJSON ProviderConfig
instance FromJSON ProviderConfig

-- | Safe defaults for out-of-the-box local use.
defaultProviderConfig :: ProviderConfig
defaultProviderConfig = ProviderConfig
  { pcProvider = "stub"
  , pcModel    = "stub"
  , pcBaseUrl  = ""
  , pcApiKey   = ""
  }

-- ---------------------------------------------------------------------------
-- Top-level config
-- ---------------------------------------------------------------------------

data Config = Config
  { cfgProvider           :: !ProviderConfig
  , cfgMaxTokens          :: !Int       -- ^ Max tokens per response
  , cfgVerbose            :: !Bool      -- ^ Print debug info
  , cfgWorkingDir         :: !FilePath  -- ^ Project root (default: cwd)
  , cfgMaxContextChars    :: !Int       -- ^ Conservative context-window limit in characters
  , cfgMaxSessionLogBytes :: !Int       -- ^ Max session.jsonl size before rotation (bytes)
  , cfgDisabledTools      :: ![Text]    -- ^ Built-in tool names to remove from the runtime registry
  } deriving stock (Show, Eq, Generic)

instance ToJSON Config

-- | Custom FromJSON instance that treats @cfgMaxContextChars@,
--   @cfgMaxSessionLogBytes@, and @cfgDisabledTools@ as optional fields
--   with sensible defaults.
--   This keeps old minimal config files (without these fields) working
--   after the fields were added.
instance FromJSON Config where
  parseJSON = withObject "Config" $ \o -> do
    prov      <- o .:  "cfgProvider"
    maxToks   <- o .:  "cfgMaxTokens"
    verbose   <- o .:  "cfgVerbose"
    workDir   <- o .:  "cfgWorkingDir"
    maxCtx    <- o .:? "cfgMaxContextChars"    .!= defaultMaxContextChars
    maxLogB   <- o .:? "cfgMaxSessionLogBytes" .!= defaultMaxSessionLogBytes
    disabled  <- o .:? "cfgDisabledTools"      .!= []
    pure Config
      { cfgProvider           = prov
      , cfgMaxTokens          = maxToks
      , cfgVerbose            = verbose
      , cfgWorkingDir         = workDir
      , cfgMaxContextChars    = maxCtx
      , cfgMaxSessionLogBytes = maxLogB
      , cfgDisabledTools      = disabled
      }

defaultConfig :: Config
defaultConfig = Config
  { cfgProvider           = defaultProviderConfig
  , cfgMaxTokens          = 4096
  , cfgVerbose            = False
  , cfgWorkingDir         = "."
  , cfgMaxContextChars    = defaultMaxContextChars
  , cfgMaxSessionLogBytes = defaultMaxSessionLogBytes
  , cfgDisabledTools      = []
  }

-- | Default context-window limit in characters.
--   This is a conservative estimate (~30K tokens at ~4 chars/token)
--   suitable for most 128K-token models.  Override via config file.
defaultMaxContextChars :: Int
defaultMaxContextChars = 120000

-- | Default maximum session log file size in bytes (5 MB).
--   When the existing @session.jsonl@ exceeds this limit, it is
--   rotated to @session.jsonl.1@ before new events are appended.
--   Set to 0 to disable rotation.
defaultMaxSessionLogBytes :: Int
defaultMaxSessionLogBytes = 5 * 1024 * 1024

-- ---------------------------------------------------------------------------
-- Environment-variable expansion
-- ---------------------------------------------------------------------------

-- | Expand environment-variable references in a string.
--
-- Supported syntax:
--
--   * @$VAR@ — bare variable name (letters, digits, underscores)
--   * @${VAR}@ — braced variable name
--
-- Undefined variables expand to the empty string.
-- A trailing @$@ with no variable name is left as-is.
expandEnvVars :: String -> IO String
expandEnvVars [] = pure []
expandEnvVars ('$' : '{' : rest) =
  case break (== '}') rest of
    (name, '}' : more) -> do
      val <- lookupEnv name
      rest' <- expandEnvVars more
      pure $ maybe [] id val ++ rest'
    _ -> do
      -- No closing brace: treat ${ as literal
      rest' <- expandEnvVars rest
      pure $ "${" ++ rest'
expandEnvVars ('$' : c : rest)
  | isAlpha c || c == '_' = do
      let (namePart, more) = span (\ch -> isAlphaNum ch || ch == '_') (c : rest)
      val <- lookupEnv namePart
      rest' <- expandEnvVars more
      pure $ maybe [] id val ++ rest'
expandEnvVars (c : rest) = do
  rest' <- expandEnvVars rest
  pure (c : rest')

-- | Expand environment-variable references in the string-valued fields
--   of a 'Config'.  Only 'cfgWorkingDir' and the string fields of
--   'ProviderConfig' are affected; numeric and boolean fields are left
--   untouched.
expandConfig :: Config -> IO Config
expandConfig cfg = do
  let pc = cfgProvider cfg
  pcApiKey'   <- expandEnvVars (pcApiKey   pc)
  pcBaseUrl'  <- expandEnvVars (pcBaseUrl  pc)
  pcModel'    <- expandEnvVars (pcModel    pc)
  pcProvider' <- expandEnvVars (pcProvider pc)
  workDir'    <- expandEnvVars (cfgWorkingDir cfg)
  pure cfg
    { cfgProvider = pc
        { pcApiKey   = pcApiKey'
        , pcBaseUrl  = pcBaseUrl'
        , pcModel    = pcModel'
        , pcProvider = pcProvider'
        }
    , cfgWorkingDir = workDir'
    }

-- ---------------------------------------------------------------------------
-- Loading
-- ---------------------------------------------------------------------------

-- | Attempt to load a config file.  Search order:
--
--   1. @.\/haskode.json@
--   2. @.\/haskode.jsonc@
--   3. @~\/.config\/haskode\/config.json@
--
--   Returns 'defaultConfig' when no file is found or parsing fails.
--
--   After successful JSON decode, environment-variable references in
--   string fields are expanded via 'expandConfig' before the result
--   is returned.
loadConfig :: IO Config
loadConfig = do
  candidates <- sequence
    [ pure "haskode.json"
    , pure "haskode.jsonc"
    , (</> ".config/haskode/config.json") <$> getHomeDirectory
    ]
  go candidates
  where
    go [] = pure defaultConfig
    go (path : rest) = do
      exists <- doesFileExist path
      if exists
        then do
          bytes <- BS.readFile path
          case eitherDecode (LBS.fromStrict bytes) of
            Right cfg -> expandConfig cfg
            Left  err -> do
              putStrLn $ "haskode: warning: failed to parse " ++ path
                       ++ " (" ++ err ++ "), using defaults"
              pure defaultConfig
        else go rest

-- | Load one explicit config path.
--
-- Unlike 'loadConfig', this does not search fallback paths and does not
-- silently fall back to defaults.  A missing or malformed explicit config
-- is a user-visible error because the CLI was told exactly what to load.
loadConfigFrom :: FilePath -> IO Config
loadConfigFrom path = do
  exists <- doesFileExist path
  if not exists
    then ioError $ userError $ "haskode: config file not found: " ++ path
    else do
      bytes <- BS.readFile path
      case eitherDecode (LBS.fromStrict bytes) of
        Right cfg -> expandConfig cfg
        Left err  -> ioError $ userError $
          "haskode: failed to parse config file " ++ path ++ ": " ++ err

-- ---------------------------------------------------------------------------
-- Token-limit field name
-- ---------------------------------------------------------------------------

-- | The JSON field name to use for the per-request token limit.
--
--   OpenAI's newer API requires @\"max_completion_tokens\"@ instead of
--   @\"max_tokens\"@.  Local and proxy providers (Ollama, vLLM,
--   LiteLLM, OpenRouter) still expect @\"max_tokens\"@.
tokenLimitFieldName :: ProviderConfig -> Text
tokenLimitFieldName pc = case pcProvider pc of
  "openai" -> "max_completion_tokens"
  _        -> "max_tokens"
