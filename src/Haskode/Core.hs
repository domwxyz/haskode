{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Core data types for Haskode.
--
-- This module defines the fundamental vocabulary that every other module
-- shares: messages, roles, tool call / result pairs, and conversation
-- history.  Keeping these types small and boring makes the rest of the
-- codebase easier to reason about.
--
-- Design notes:
--   * We use strict 'Text' everywhere (not String) for efficiency.
--   * JSON instances follow the OpenAI chat-completion wire format so
--     that swapping providers later requires minimal changes.
--   * ToolCall and ToolResult are intentionally simple; a richer schema
--     layer can be added on top later without breaking the core.

module Haskode.Core
  ( -- * Roles
    Role (..)
    -- * Messages
  , Message (..)
  , mkUserMessage
  , mkAssistantMessage
  , mkSystemMessage
    -- * Tool interaction
  , ToolCall (..)
  , ToolResult (..)
    -- * Conversation history
  , Conversation
  , emptyConversation
  , appendMessage
  ) where

import Data.Aeson   (FromJSON, ToJSON, (.=), (.:), (.:?), object, parseJSON, toJSON)
import Data.Aeson   (Value (..))
import Data.Text    (Text)
import GHC.Generics (Generic)

-- ---------------------------------------------------------------------------
-- Role
-- ---------------------------------------------------------------------------

-- | Who produced a message.  Mirrors the three roles the OpenAI API
--   (and most LLM APIs) recognise.
data Role
  = System
  | User
  | Assistant
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)

instance ToJSON Role where
  toJSON System    = String "system"
  toJSON User      = String "user"
  toJSON Assistant = String "assistant"

instance FromJSON Role where
  parseJSON (String "system")    = pure System
  parseJSON (String "user")      = pure User
  parseJSON (String "assistant") = pure Assistant
  parseJSON other                = fail $ "unknown role: " <> show other

-- ---------------------------------------------------------------------------
-- Message
-- ---------------------------------------------------------------------------

-- | A single message in a conversation.
data Message = Message
  { msgRole      :: !Role
  , msgContent   :: !Text
  , msgCallId    :: !(Maybe Text)       -- ^ Set when this message is a tool result
  , msgToolCalls :: !(Maybe [ToolCall]) -- ^ Set when assistant requested tool calls
  } deriving stock (Show, Eq, Generic)

instance ToJSON Message where
  toJSON m = object $
    [ "role"    .= msgRole m
    , "content" .= msgContent m
    ] <> maybe [] (\cid -> ["tool_call_id" .= cid]) (msgCallId m)
      <> maybe [] (\tcs -> ["tool_calls"   .= tcs])  (msgToolCalls m)

instance FromJSON Message where
  parseJSON (Object o) = Message
    <$> o .:  "role"
    <*> o .:  "content"
    <*> o .:? "tool_call_id"
    <*> o .:? "tool_calls"
  parseJSON other = fail $ "expected object, got: " <> show other

-- | Convenience constructors.
mkUserMessage :: Text -> Message
mkUserMessage txt = Message User txt Nothing Nothing

mkAssistantMessage :: Text -> Message
mkAssistantMessage txt = Message Assistant txt Nothing Nothing

mkSystemMessage :: Text -> Message
mkSystemMessage txt = Message System txt Nothing Nothing

-- ---------------------------------------------------------------------------
-- Tool interaction
-- ---------------------------------------------------------------------------

-- | A request from the LLM to invoke a tool.
data ToolCall = ToolCall
  { tcId   :: !Text     -- ^ Unique call identifier (assigned by provider)
  , tcName :: !Text     -- ^ Tool name (must match a registered tool)
  , tcArgs :: !Value    -- ^ JSON-encoded arguments
  } deriving stock (Show, Eq, Generic)

instance ToJSON ToolCall
instance FromJSON ToolCall

-- | The outcome of executing a tool.
data ToolResult = ToolResult
  { trCallId :: !Text   -- ^ Must match the ToolCall tcId
  , trOutput :: !Text   -- ^ Plain-text output to feed back to the LLM
  } deriving stock (Show, Eq, Generic)

instance ToJSON ToolResult
instance FromJSON ToolResult

-- ---------------------------------------------------------------------------
-- Conversation
-- ---------------------------------------------------------------------------

-- | An ordered list of messages representing the conversation so far.
--   New messages are appended at the end (most recent last).
type Conversation = [Message]

emptyConversation :: Conversation
emptyConversation = []

appendMessage :: Message -> Conversation -> Conversation
appendMessage msg conv = conv ++ [msg]
