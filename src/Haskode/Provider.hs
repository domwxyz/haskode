{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | LLM provider adapter.
--
-- This module defines the 'Provider' interface — a record of functions
-- that any LLM backend must implement.  Keeping the interface behind
-- a record-of-functions (rather than a typeclass) makes it easy to
-- swap providers at runtime and to mock them in tests.
--
-- Built-in providers:
--
--   * 'stubProvider' — echoes the last user message (for development)
--   * 'scriptedProvider' — replays a fixed sequence of responses (for testing)
--
-- The real OpenAI-compatible provider lives in "Haskode.Provider.OpenAI".
-- Import it directly:
--
-- > import Haskode.Provider.OpenAI (openaiProvider)
--
-- That module also exports the pure request\/response conversion
-- functions so they can be tested without network I\/O.

module Haskode.Provider
  ( -- * Provider interface
    Provider (..)
    -- * Built-in providers
  , stubProvider
  , scriptedProvider
    -- * Request / response types
  , CompletionRequest (..)
  , CompletionResponse (..)
  ) where

import Data.IORef         (newIORef, readIORef, writeIORef)
import Data.Text          (Text)

import Haskode.Core       (Message (..), Role (..), ToolCall (..),
                           mkAssistantMessage)

-- ---------------------------------------------------------------------------
-- Request / response
-- ---------------------------------------------------------------------------

-- | What we send to a provider.
data CompletionRequest = CompletionRequest
  { crMessages  :: ![Message]  -- ^ Conversation history (system + user + assistant)
  , crModel     :: !Text       -- ^ Model identifier
  , crMaxTokens :: !Int        -- ^ Token budget for this response
  } deriving stock (Show, Eq)

-- | What we get back.
data CompletionResponse = CompletionResponse
  { crReply     :: !Message         -- ^ The assistant's reply
  , crToolCalls :: !(Maybe [ToolCall]) -- ^ Tool calls requested by the assistant
  } deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Provider record
-- ---------------------------------------------------------------------------

-- | A pluggable LLM backend.
data Provider = Provider
  { providerName    :: !Text
  , providerComplete :: CompletionRequest -> IO CompletionResponse
  }

-- ---------------------------------------------------------------------------
-- Stub provider (for development)
-- ---------------------------------------------------------------------------

-- | A trivial provider that echoes the last user message.
--   Useful for testing the agent loop without burning tokens.
stubProvider :: Provider
stubProvider = Provider
  { providerName = "stub"
  , providerComplete = \req -> do
      let lastUser = last [ m | m <- crMessages req, msgRole m == User ]
          reply    = mkAssistantMessage $
            "[stub] I heard you say: " <> msgContent lastUser
      pure CompletionResponse
        { crReply     = reply
        , crToolCalls = Nothing
        }
  }

-- ---------------------------------------------------------------------------
-- Scripted provider (for testing)
-- ---------------------------------------------------------------------------

-- | A provider that replays a fixed list of 'CompletionResponse' values.
--   Each call to 'providerComplete' consumes the next response from the
--   list.  If the list is exhausted, the provider returns a plain text
--   message saying \"[scripted] no more responses\".
--
--   This is the primary mechanism for testing the agent loop with
--   deterministic tool-call sequences.
scriptedProvider :: [CompletionResponse] -> IO Provider
scriptedProvider responses = do
  ref <- newIORef responses
  pure Provider
    { providerName = "scripted"
    , providerComplete = \_req -> do
        remaining <- readIORef ref
        case remaining of
          [] -> pure CompletionResponse
            { crReply     = mkAssistantMessage "[scripted] no more responses"
            , crToolCalls = Nothing
            }
          (r:rest) -> do
            writeIORef ref rest
            pure r
    }
