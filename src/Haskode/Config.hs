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
--   * Environment-variable expansion

module Haskode.Config
  ( Config (..)
  , ProviderConfig (..)
  , defaultConfig
  , loadConfig
  , tokenLimitFieldName
  ) where

import Data.Aeson            (FromJSON, ToJSON, eitherDecode)
import Data.ByteString.Lazy  (readFile)
import Data.Text             (Text)
import GHC.Generics          (Generic)
import Prelude               hiding (readFile)
import System.Directory      (doesFileExist, getHomeDirectory)
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

-- | Sane defaults for local development with Ollama.
defaultProviderConfig :: ProviderConfig
defaultProviderConfig = ProviderConfig
  { pcProvider = "ollama"
  , pcModel    = "llama3.1"
  , pcBaseUrl  = "http://localhost:11434"
  , pcApiKey   = ""
  }

-- ---------------------------------------------------------------------------
-- Top-level config
-- ---------------------------------------------------------------------------

data Config = Config
  { cfgProvider   :: !ProviderConfig
  , cfgMaxTokens  :: !Int       -- ^ Max tokens per response
  , cfgVerbose    :: !Bool      -- ^ Print debug info
  , cfgWorkingDir :: !FilePath  -- ^ Project root (default: cwd)
  } deriving stock (Show, Eq, Generic)

instance ToJSON Config
instance FromJSON Config

defaultConfig :: Config
defaultConfig = Config
  { cfgProvider   = defaultProviderConfig
  , cfgMaxTokens  = 4096
  , cfgVerbose    = False
  , cfgWorkingDir = "."
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
          bytes <- readFile path
          case eitherDecode bytes of
            Right cfg -> pure cfg
            Left  err -> do
              putStrLn $ "haskode: warning: failed to parse " ++ path
                       ++ " (" ++ err ++ "), using defaults"
              pure defaultConfig
        else go rest

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
