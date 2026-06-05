{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Session event log.
--
-- Every significant action (user message, assistant reply, tool call,
-- tool result, policy decision) is recorded as an 'Event' in the
-- session log.  This gives us:
--
--   1. A full audit trail for debugging.
--   2. The ability to replay sessions later.
--   3. Training data for fine-tuning (if desired).
--
-- Events are stored in-memory during a session and flushed to a
-- JSON-lines file on exit.

module Haskode.Session
  ( -- * Event types
    Event (..)
  , EventType (..)
    -- * Session log
  , SessionLog
  , emptyLog
  , logEvent
  , logEvents
  , events
    -- * Persistence
  , flushLog
  ) where

import Data.Aeson            (ToJSON (..), Value (..), encode, (.=), object)
import Data.ByteString.Lazy  (appendFile)
import Data.Text             (Text)
import Data.Time.Clock       (UTCTime)
import Prelude               hiding (appendFile)
import System.FilePath       ((</>))

-- ---------------------------------------------------------------------------
-- Events
-- ---------------------------------------------------------------------------

-- | What kind of thing happened.
data EventType
  = EUserMessage
  | EAssistantReply
  | EToolCall
  | EToolResult
  | EPolicyDecision
  | ESessionStart
  | ESessionEnd
  deriving stock (Show, Eq, Enum, Bounded)

instance ToJSON EventType where
  toJSON EUserMessage    = String "user_message"
  toJSON EAssistantReply = String "assistant_reply"
  toJSON EToolCall       = String "tool_call"
  toJSON EToolResult     = String "tool_result"
  toJSON EPolicyDecision = String "policy_decision"
  toJSON ESessionStart   = String "session_start"
  toJSON ESessionEnd     = String "session_end"

-- | A single session event.
data Event = Event
  { evTime  :: !UTCTime
  , evType  :: !EventType
  , evData  :: !Text        -- ^ JSON-encoded payload
  } deriving stock (Show)

instance ToJSON Event where
  toJSON ev = object
    [ "time" .= evTime ev
    , "type" .= evType ev
    , "data" .= evData ev
    ]

-- ---------------------------------------------------------------------------
-- Session log
-- ---------------------------------------------------------------------------

-- | An in-memory log of session events (newest first).
newtype SessionLog = SessionLog { unLog :: [Event] }

emptyLog :: SessionLog
emptyLog = SessionLog []

logEvent :: Event -> SessionLog -> SessionLog
logEvent e (SessionLog es) = SessionLog (e : es)

logEvents :: [Event] -> SessionLog -> SessionLog
logEvents newEs (SessionLog es) = SessionLog (reverse newEs ++ es)

events :: SessionLog -> [Event]
events = reverse . unLog  -- chronological order

-- ---------------------------------------------------------------------------
-- Persistence
-- ---------------------------------------------------------------------------

-- | Append all events to @<dir>\/session.jsonl@.
--   Creates the file if it does not exist.
flushLog :: FilePath -> SessionLog -> IO ()
flushLog dir sess = do
  let path = dir </> "session.jsonl"
  -- We encode each event as a separate JSON line.
  mapM_ (\ev -> appendFile path (encode ev <> "\n")) (events sess)
