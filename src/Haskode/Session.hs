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
--   2. Read-only inspection via @--show-session@ (event counts, timestamps).
--   3. Potential training data for fine-tuning (export only; no replay).
--
-- Events are stored in-memory during a session and flushed to a
-- JSON-lines file on exit.  The log is write-only: no conversation
-- restoration, replay, or tool/provider re-execution is supported.

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
  , flushLogOnException
    -- * Session summary (read-only inspection)
  , SessionSummary (..)
  , summarizeSession
  , formatSessionSummary
  ) where

import Control.Exception     (IOException, onException, try)
import Control.Monad         (when)
import Data.Aeson            (ToJSON (..), FromJSON (..), Value (..),
                              encode, (.=), object, withObject, (.:))
import qualified Data.Aeson  as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy  (appendFile)
import Data.Map.Strict       (Map)
import qualified Data.Map.Strict as Map
import Data.Text             (Text)
import qualified Data.Text   as T
import Data.Time.Clock       (UTCTime)
import Data.Time.Format      (defaultTimeLocale, formatTime)
import Prelude               hiding (appendFile)
import System.Directory      (doesFileExist, getFileSize, renameFile)
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
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance ToJSON EventType where
  toJSON EUserMessage    = String "user_message"
  toJSON EAssistantReply = String "assistant_reply"
  toJSON EToolCall       = String "tool_call"
  toJSON EToolResult     = String "tool_result"
  toJSON EPolicyDecision = String "policy_decision"
  toJSON ESessionStart   = String "session_start"
  toJSON ESessionEnd     = String "session_end"

instance FromJSON EventType where
  parseJSON (String "user_message")    = pure EUserMessage
  parseJSON (String "assistant_reply") = pure EAssistantReply
  parseJSON (String "tool_call")       = pure EToolCall
  parseJSON (String "tool_result")     = pure EToolResult
  parseJSON (String "policy_decision") = pure EPolicyDecision
  parseJSON (String "session_start")   = pure ESessionStart
  parseJSON (String "session_end")     = pure ESessionEnd
  parseJSON other                      = fail $ "unknown EventType: " ++ show other

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

instance FromJSON Event where
  parseJSON = withObject "Event" $ \o ->
    Event <$> o .: "time"
          <*> o .: "type"
          <*> o .: "data"

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
--
--   When @maxBytes@ is positive and the existing file exceeds that
--   size, the file is rotated to @session.jsonl.1@ (replacing any
--   previous backup) before new events are written.  This keeps the
--   active log bounded without losing history.
--
--   Pass @maxBytes = 0@ to disable rotation.
flushLog :: FilePath -> Int -> SessionLog -> IO ()
flushLog dir maxBytes sess = do
  let path    = dir </> "session.jsonl"
      backup  = dir </> "session.jsonl.1"
  -- Rotate if the existing log exceeds the size limit.
  when (maxBytes > 0) $ do
    exists <- doesFileExist path
    if exists
      then do
        sizeResult <- try (getFileSize path) :: IO (Either IOException Integer)
        case sizeResult of
          Right size | fromIntegral size > maxBytes ->
            -- Rename current log to backup (replaces any previous
            -- .1 file atomically on POSIX).
            renameFile path backup
          _ -> pure ()
      else pure ()
  -- We encode each event as a separate JSON line.
  mapM_ (\ev -> appendFile path (encode ev <> "\n")) (events sess)

-- | Run an action; if it throws an exception, flush the session log
--   and re-raise.  On success the log is left for the caller to flush.
--
--   This is the building block for exception-safe session persistence:
--   wrap any agent action with this helper to guarantee that accumulated
--   events are written even when the action fails.
flushLogOnException :: FilePath -> Int -> SessionLog -> IO a -> IO a
flushLogOnException dir maxBytes sess action =
  action `onException` flushLog dir maxBytes sess

-- ---------------------------------------------------------------------------
-- Session summary (read-only inspection)
-- ---------------------------------------------------------------------------

-- | A concise summary of a @session.jsonl@ file.
--   Used by @\-\-show-session@ to display audit-log statistics
--   without replaying or restoring the session.
data SessionSummary = SessionSummary
  { ssTotalEvents   :: !Int
  , ssFirstTime     :: !(Maybe UTCTime)
  , ssLastTime      :: !(Maybe UTCTime)
  , ssTypeCounts    :: !(Map EventType Int)
  , ssMalformedLines :: !Int
  } deriving stock (Show)

-- | Read a @session.jsonl@ file and produce a concise summary.
--
--   * Returns a 'SessionSummary' with zero events when the file is
--     missing or empty.
--   * Lines that fail to decode as valid 'Event' values are counted
--     in 'ssMalformedLines' rather than causing an error.
--   * Only the active @session.jsonl@ is inspected; any rotated
--     @session.jsonl.1@ backup is ignored.
summarizeSession :: FilePath -> IO SessionSummary
summarizeSession dir = do
  let path = dir </> "session.jsonl"
  exists <- doesFileExist path
  if not exists
    then pure emptySummary
    else do
      bytes <- LBS.readFile path
      if LBS.null bytes
        then pure emptySummary
        else pure $ summarizeLines (LBS.split (fromIntegral (fromEnum '\n')) bytes)
  where
    emptySummary = SessionSummary 0 Nothing Nothing Map.empty 0

-- | Pure summary builder from a list of lazy byte-string lines.
--   Empty lines are silently skipped.
summarizeLines :: [LBS.ByteString] -> SessionSummary
summarizeLines = foldl step emptySummary
  where
    emptySummary = SessionSummary 0 Nothing Nothing Map.empty 0

    step acc line
      | LBS.null line = acc
      | otherwise = case Aeson.decode line :: Maybe Event of
          Nothing -> acc { ssMalformedLines = ssMalformedLines acc + 1 }
          Just ev ->
            let t = evTime ev
                newTotal = ssTotalEvents acc + 1
                newFirst = case ssFirstTime acc of
                  Nothing -> Just t
                  old     -> old
                newLast  = Just t
                newCounts = Map.insertWith (+) (evType ev) 1 (ssTypeCounts acc)
            in acc { ssTotalEvents  = newTotal
                   , ssFirstTime    = newFirst
                   , ssLastTime     = newLast
                   , ssTypeCounts   = newCounts
                   }

-- | Render a 'SessionSummary' as human-readable text for terminal output.
formatSessionSummary :: SessionSummary -> Text
formatSessionSummary ss =
  let total = ssTotalEvents ss
      malformed = ssMalformedLines ss
      fmtTime t = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" t)
      timeRange = case (ssFirstTime ss, ssLastTime ss) of
        (Just a, Just b)
          | a == b    -> "  Time:          " <> fmtTime a
          | otherwise -> "  First event:   " <> fmtTime a <> "\n"
                      <> "  Last event:    " <> fmtTime b
        _             -> "  Time:          (none)"
      typeLines = map fmtTypeCount
        [ (EUserMessage,    "user_message")
        , (EAssistantReply, "assistant_reply")
        , (EToolCall,       "tool_call")
        , (EToolResult,     "tool_result")
        , (EPolicyDecision, "policy_decision")
        , (ESessionStart,   "session_start")
        , (ESessionEnd,     "session_end")
        ]
      fmtTypeCount (ety, label) =
        let n = Map.findWithDefault 0 ety (ssTypeCounts ss)
        in "  " <> label <> ": " <> T.pack (show n)
      malformedLine = if malformed > 0
        then "\n  Malformed lines: " <> T.pack (show malformed)
        else ""
  in "Session summary:\n"
  <> "  Total events:  " <> T.pack (show total) <> "\n"
  <> timeRange <> "\n"
  <> T.unlines typeLines
  <> malformedLine
