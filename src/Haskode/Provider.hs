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
    -- * Streaming types
  , ProviderStream
  , StreamHandler (..)
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
-- Streaming types
--
-- These types define the optional streaming interface for providers.
-- The OpenAI provider streams text deltas to the handler and assembles
-- streamed tool-call deltas into the final 'CompletionResponse'.
-- The agent uses providerStream when available, falling back to
-- providerComplete when not.
-- ---------------------------------------------------------------------------

-- | A callback record for receiving streamed text chunks.
--
--   The streaming provider calls 'onToken' for each assistant text
--   chunk as it arrives.  The handler is responsible for display
--   (e.g. printing to stdout and flushing).  Streamed tool-call
--   deltas are assembled internally and returned in the final
--   'CompletionResponse'.
data StreamHandler = StreamHandler
  { onToken :: Text -> IO ()  -- ^ Called for each text chunk
  }

-- | A streaming completion function.
--
--   Takes a 'CompletionRequest' and a 'StreamHandler', emits text
--   chunks via the handler, and returns the final assembled
--   'CompletionResponse'.  This is the same return type as
--   'providerComplete' so that downstream consumers (tool calls,
--   session logging, conversation state) see no difference.
type ProviderStream = CompletionRequest -> StreamHandler -> IO CompletionResponse

-- ---------------------------------------------------------------------------
-- Provider record
-- ---------------------------------------------------------------------------

-- | A pluggable LLM backend.
data Provider = Provider
  { providerName     :: !Text
  , providerComplete :: CompletionRequest -> IO CompletionResponse
    -- | Optional streaming completion path.
    --
    --   When 'Just', the provider supports streaming assistant text
    --   chunks via a callback.  When 'Nothing' (the common case),
    --   only the non-streaming 'providerComplete' path is available.
    --
    --   The OpenAI provider streams text deltas and assembles
    --   streamed tool-call deltas into the final response.
  , providerStream   :: Maybe ProviderStream
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
  , providerStream = Nothing  -- streaming not implemented
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
    , providerStream = Nothing  -- streaming not implemented
    }
