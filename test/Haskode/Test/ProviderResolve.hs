{-# LANGUAGE OverloadedStrings #-}

-- | Pure provider startup resolution tests.
module Haskode.Test.ProviderResolve (tests) where

import Data.List (isInfixOf)

import Haskode.Provider.Resolve
  ( ProviderKind (..)
  , classifyProviderName
  , formatAnthropicApiKeyError
  , formatOpenAICompatibleApiKeyError
  , formatUnknownProviderError
  , openAICompatibleProviders
  , providerApiKeyEnvVar
  , providerDefaultBaseUrlHint
  , providerKindLabel
  , providerRequiresApiKey
  )
import Haskode.Test.Util (Test)

testProviderClassification :: Test
testProviderClassification =
  let openAIOk =
        all ((== ProviderOpenAICompatible) . classifyProviderName)
          openAICompatibleProviders
      others =
        [ classifyProviderName "anthropic" == ProviderAnthropic
        , classifyProviderName "stub" == ProviderStub
        , classifyProviderName "missing" == ProviderUnknown
        ]
  in if openAIOk && and others
       then pure $ Right ()
       else pure $ Left "Provider classification changed"

testProviderKindLabels :: Test
testProviderKindLabels =
  let cases =
        [ ("stub", Just "local development")
        , ("ollama", Just "local model server")
        , ("vllm", Just "local model server")
        , ("openai", Just "hosted")
        , ("litellm", Just "hosted/proxy")
        , ("openrouter", Just "hosted/proxy")
        , ("anthropic", Just "hosted")
        , ("missing", Nothing)
        ]
      bad =
        [ (name, providerKindLabel name)
        | (name, expected) <- cases
        , providerKindLabel name /= expected
        ]
  in if null bad
       then pure $ Right ()
       else pure $ Left $ "Provider kind labels changed: " ++ show bad

testProviderRequiresApiKey :: Test
testProviderRequiresApiKey =
  let noKey = ["stub", "ollama", "vllm"]
      needsKey = ["openai", "litellm", "openrouter", "anthropic", "missing"]
      ok = all (not . providerRequiresApiKey) noKey
        && all providerRequiresApiKey needsKey
  in if ok
       then pure $ Right ()
       else pure $ Left "Provider API-key requirement changed"

testProviderEnvVars :: Test
testProviderEnvVars =
  let cases =
        [ ("anthropic", "ANTHROPIC_API_KEY")
        , ("openai", "OPENAI_API_KEY")
        , ("ollama", "OPENAI_API_KEY")
        , ("missing", "OPENAI_API_KEY")
        ]
      bad =
        [ (name, providerApiKeyEnvVar name)
        | (name, expected) <- cases
        , providerApiKeyEnvVar name /= expected
        ]
  in if null bad
       then pure $ Right ()
       else pure $ Left $ "Provider env-var mapping changed: " ++ show bad

testProviderDefaultBaseUrlHints :: Test
testProviderDefaultBaseUrlHints =
  let cases =
        [ ("openai", "empty; set --base-url https://api.openai.com or pcBaseUrl")
        , ("anthropic", "provider default (https://api.anthropic.com)")
        , ("ollama", "empty; set --base-url http://localhost:11434 or pcBaseUrl")
        , ("vllm", "empty; set --base-url http://localhost:8000 or pcBaseUrl")
        , ("litellm", "empty; set --base-url http://localhost:4000 or pcBaseUrl")
        , ("openrouter", "empty; set --base-url https://openrouter.ai/api or pcBaseUrl")
        , ("stub", "not needed")
        , ("missing", "empty; no provider default known")
        ]
      bad =
        [ (name, providerDefaultBaseUrlHint name)
        | (name, expected) <- cases
        , providerDefaultBaseUrlHint name /= expected
        ]
  in if null bad
       then pure $ Right ()
       else pure $ Left $ "Provider base URL hints changed: " ++ show bad

testOpenAICompatibleApiKeyErrorText :: Test
testOpenAICompatibleApiKeyErrorText =
  let lines' = formatOpenAICompatibleApiKeyError "openai API key not found."
      expected =
        [ "Error: openai API key not found."
        , ""
        , "To set an API key, either:"
        , "  1. Pass --api-key on the command line, or"
        , "  2. Set the OPENAI_API_KEY environment variable, or"
        , "  3. Add \"pcApiKey\" to your haskode.json config file."
        , ""
        , "Example:"
        , "  export OPENAI_API_KEY=\"sk-...\""
        , "  cabal run haskode -- --provider openai --base-url https://api.openai.com --prompt \"Hello\""
        ]
  in if lines' == expected
       then pure $ Right ()
       else pure $ Left $ "OpenAI-compatible key help changed: " ++ show lines'

testAnthropicApiKeyErrorText :: Test
testAnthropicApiKeyErrorText =
  let lines' = formatAnthropicApiKeyError "Anthropic API key not found."
      expected =
        [ "Error: Anthropic API key not found."
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
  in if lines' == expected
       then pure $ Right ()
       else pure $ Left $ "Anthropic key help changed: " ++ show lines'

testUnknownProviderErrorText :: Test
testUnknownProviderErrorText =
  let out = unlines (formatUnknownProviderError "bogus")
      hasUnknown = "Error: unknown provider \"bogus\"." `isInfixOf` out
      hasOpenAIList =
        "openai ollama vllm litellm openrouter  (OpenAI-compatible HTTP)"
          `isInfixOf` out
      hasAnthropic = "anthropic" `isInfixOf` out
      hasStub = "stub" `isInfixOf` out
      hasExample =
        "cabal run haskode -- --provider openai --base-url https://api.openai.com --prompt \"Hello\""
          `isInfixOf` out
  in if hasUnknown && hasOpenAIList && hasAnthropic && hasStub && hasExample
       then pure $ Right ()
       else pure $ Left $ "Unknown provider help changed: " ++ out

tests :: [Test]
tests =
  [ testProviderClassification
  , testProviderKindLabels
  , testProviderRequiresApiKey
  , testProviderEnvVars
  , testProviderDefaultBaseUrlHints
  , testOpenAICompatibleApiKeyErrorText
  , testAnthropicApiKeyErrorText
  , testUnknownProviderErrorText
  ]
