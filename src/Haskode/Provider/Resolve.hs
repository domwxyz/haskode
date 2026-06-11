{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Pure provider-name resolution helpers used during startup and by local
-- diagnostics.
--
-- This module deliberately does not construct providers or touch the
-- environment.  Provider constructors stay in the concrete transport modules;
-- this module only keeps the shared names, classifications, and user-facing
-- startup text in one small place.
module Haskode.Provider.Resolve
  ( ProviderKind (..)
  , openAICompatibleProviders
  , localOpenAICompatibleProviders
  , classifyProviderName
  , providerKindLabel
  , providerRequiresApiKey
  , providerDefaultBaseUrlHint
  , providerApiKeyEnvVar
  , formatOpenAICompatibleApiKeyError
  , formatAnthropicApiKeyError
  , formatUnknownProviderError
  ) where

import Data.Text (Text)

-- | Provider families known to startup.
data ProviderKind
  = ProviderOpenAICompatible
  | ProviderAnthropic
  | ProviderStub
  | ProviderUnknown
  deriving stock (Eq, Show)

-- | Provider names handled by the OpenAI-compatible chat-completions adapter.
openAICompatibleProviders :: [String]
openAICompatibleProviders = ["openai", "ollama", "vllm", "litellm", "openrouter"]

-- | OpenAI-compatible provider names that normally run without auth.
localOpenAICompatibleProviders :: [String]
localOpenAICompatibleProviders = ["ollama", "vllm"]

-- | Classify a provider name without doing any IO.
classifyProviderName :: String -> ProviderKind
classifyProviderName name
  | name `elem` openAICompatibleProviders = ProviderOpenAICompatible
  | name == "anthropic" = ProviderAnthropic
  | name == "stub" = ProviderStub
  | otherwise = ProviderUnknown

-- | Human-facing provider kind used by @/doctor@.
providerKindLabel :: String -> Maybe Text
providerKindLabel name = case name of
  "stub"       -> Just "local development"
  "ollama"     -> Just "local model server"
  "vllm"       -> Just "local model server"
  "openai"     -> Just "hosted"
  "litellm"    -> Just "hosted/proxy"
  "openrouter" -> Just "hosted/proxy"
  "anthropic"  -> Just "hosted"
  _            -> Nothing

-- | Whether startup should expect some API-key source for this provider.
providerRequiresApiKey :: String -> Bool
providerRequiresApiKey name =
  case classifyProviderName name of
    ProviderStub -> False
    ProviderOpenAICompatible ->
      name `notElem` localOpenAICompatibleProviders
    ProviderAnthropic -> True
    ProviderUnknown -> True

-- | Hint text for the default base URL of a known provider.
providerDefaultBaseUrlHint :: String -> Text
providerDefaultBaseUrlHint name = case name of
  "openai"     -> "provider default (https://api.openai.com)"
  "anthropic"  -> "provider default (https://api.anthropic.com)"
  "ollama"     -> "provider default (http://localhost:11434)"
  "vllm"       -> "provider default (http://localhost:8000)"
  "litellm"    -> "provider default (http://localhost:4000)"
  "openrouter" -> "provider default (https://openrouter.ai/api)"
  "stub"       -> "not needed"
  _            -> "provider default"

-- | The environment variable name for a provider's API key.
providerApiKeyEnvVar :: String -> String
providerApiKeyEnvVar "anthropic" = "ANTHROPIC_API_KEY"
providerApiKeyEnvVar _           = "OPENAI_API_KEY"

formatOpenAICompatibleApiKeyError :: String -> [String]
formatOpenAICompatibleApiKeyError err =
  [ "Error: " ++ err
  , ""
  , "To set an API key, either:"
  , "  1. Pass --api-key on the command line, or"
  , "  2. Set the OPENAI_API_KEY environment variable, or"
  , "  3. Add \"pcApiKey\" to your haskode.json config file."
  , ""
  , "Example:"
  , "  export OPENAI_API_KEY=\"sk-...\""
  , "  cabal run haskode -- --provider openai --prompt \"Hello\""
  ]

formatAnthropicApiKeyError :: String -> [String]
formatAnthropicApiKeyError err =
  [ "Error: " ++ err
  , ""
  , "To set an Anthropic API key, either:"
  , "  1. Pass --api-key on the command line, or"
  , "  2. Add \"pcApiKey\" to your haskode.json config file, or"
  , "  3. Set the ANTHROPIC_API_KEY environment variable."
  , ""
  , "Example:"
  , "  export ANTHROPIC_API_KEY=\"sk-ant-...\""
  , "  cabal run haskode -- --provider anthropic --model claude-3-5-sonnet-latest --prompt \"Hello\""
  ]

formatUnknownProviderError :: String -> [String]
formatUnknownProviderError name =
  [ "Error: unknown provider \"" ++ name ++ "\"."
  , ""
  , "Supported providers:"
  , "  " ++ unwords openAICompatibleProviders ++ "  (OpenAI-compatible HTTP)"
  , "  anthropic                                  (Anthropic Messages API)"
  , "  stub                                        (local echo, for development)"
  , ""
  , "Example:"
  , "  cabal run haskode -- --provider openai --prompt \"Hello\""
  ]
