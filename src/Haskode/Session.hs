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
--   3. Conservative session resume via @--resume@ (non-replaying).
--   4. Potential training data for fine-tuning (export only; no replay).
--
-- Events are stored in-memory during a session and flushed to a
-- JSON-lines file on exit.  Resume reconstructs safe text context
-- (user messages and assistant replies after the last reset boundary)
-- without re-executing tools or provider calls.

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
  , isMeaningfulSession
    -- * Persistence
  , flushLog
  , flushLogOnException
    -- * Session summary (read-only inspection)
  , SessionSummary (..)
  , summarizeSession
  , formatSessionSummary
    -- * Resume
  , ResumeResult (..)
  , loadResumeEvents
  , resumeContextEventsToConversation
  , formatResumeSummary
  ) where

import Control.Exception     (IOException, onException, try)
import Control.Monad         (when)
import Data.Aeson            (ToJSON (..), FromJSON (..), Value (..),
                              encode, (.=), object, withObject, (.:))
import qualified Data.Aeson  as Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.ByteString.Lazy  (appendFile)
import Data.Either           (partitionEithers)
import Data.Map.Strict       (Map)
import qualified Data.Map.Strict as Map
import Data.Text             (Text)
import qualified Data.Text   as T
import Data.Time.Clock       (UTCTime)
import Data.Time.Format      (defaultTimeLocale, formatTime)
import Prelude               hiding (appendFile)
import System.Directory      (doesFileExist, getFileSize, renameFile)
import System.FilePath       ((</>))

import Haskode.Core          (Conversation,
                              mkUserMessage, mkAssistantMessage, mkSystemMessage)

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
  | EConversationReset
  | EConversationCompacted
  | ERunLimitReached
  deriving stock (Show, Eq, Ord, Enum, Bounded)

instance ToJSON EventType where
  toJSON EUserMessage    = String "user_message"
  toJSON EAssistantReply = String "assistant_reply"
  toJSON EToolCall       = String "tool_call"
  toJSON EToolResult     = String "tool_result"
  toJSON EPolicyDecision = String "policy_decision"
  toJSON ESessionStart       = String "session_start"
  toJSON ESessionEnd         = String "session_end"
  toJSON EConversationReset  = String "conversation_reset"
  toJSON EConversationCompacted = String "conversation_compacted"
  toJSON ERunLimitReached    = String "run_limit_reached"

instance FromJSON EventType where
  parseJSON (String "user_message")    = pure EUserMessage
  parseJSON (String "assistant_reply") = pure EAssistantReply
  parseJSON (String "tool_call")       = pure EToolCall
  parseJSON (String "tool_result")     = pure EToolResult
  parseJSON (String "policy_decision") = pure EPolicyDecision
  parseJSON (String "session_start")       = pure ESessionStart
  parseJSON (String "session_end")         = pure ESessionEnd
  parseJSON (String "conversation_reset")  = pure EConversationReset
  parseJSON (String "conversation_compacted") = pure EConversationCompacted
  parseJSON (String "run_limit_reached")   = pure ERunLimitReached
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

-- | True when the log contains at least one content event (anything
--   other than session_start, session_end, or conversation_reset).
--   Used to decide whether a session is worth flushing: a run where
--   the user immediately exits (or only uses /help, /status, /new)
--   produces only lifecycle events and should not create a noisy log.
isMeaningfulSession :: SessionLog -> Bool
isMeaningfulSession = any isContentEvent . events
  where
    isContentEvent ev = case evType ev of
      EUserMessage       -> True
      EAssistantReply    -> True
      EToolCall          -> True
      EToolResult        -> True
      EPolicyDecision    -> True
      ESessionStart      -> False
      ESessionEnd        -> False
      EConversationReset -> False
      EConversationCompacted -> True
      ERunLimitReached   -> True

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
--
--   When the session contains only lifecycle events (no content), the
--   exception guard is skipped — a crash in a no-op session should not
--   create a noisy lifecycle-only log.
flushLogOnException :: FilePath -> Int -> SessionLog -> IO a -> IO a
flushLogOnException dir maxBytes sess action
  | isMeaningfulSession sess = action `onException` flushLog dir maxBytes sess
  | otherwise                = action

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
  , ssLogPath       :: !FilePath     -- ^ Absolute path to the inspected log
  , ssBackupExists  :: !Bool         -- ^ Whether session.jsonl.1 exists
  } deriving stock (Show)

-- | Read a @session.jsonl@ file and produce a concise summary.
--
--   * Returns a 'SessionSummary' with zero events when the file is
--     missing or empty.
--   * Lines that fail to decode as valid 'Event' values are counted
--     in 'ssMalformedLines' rather than causing an error.
--   * Only the active @session.jsonl@ is inspected; any rotated
--     @session.jsonl.1@ backup is reported but not read.
summarizeSession :: FilePath -> IO SessionSummary
summarizeSession dir = do
  let path   = dir </> "session.jsonl"
      backup = dir </> "session.jsonl.1"
  exists   <- doesFileExist path
  backupOk <- doesFileExist backup
  if not exists
    then pure (emptySummary path backupOk)
    else do
      bytes <- LBS.readFile path
      if LBS.null bytes
        then pure (emptySummary path backupOk)
        else pure $ summarizeLines path backupOk (LBS.split (fromIntegral (fromEnum '\n')) bytes)
  where
    emptySummary path backupOk =
      SessionSummary 0 Nothing Nothing Map.empty 0 path backupOk

-- | Pure summary builder from a list of lazy byte-string lines.
--   Empty lines are silently skipped.
summarizeLines :: FilePath -> Bool -> [LBS.ByteString] -> SessionSummary
summarizeLines path backupOk = foldl step emptySummary
  where
    emptySummary = SessionSummary 0 Nothing Nothing Map.empty 0 path backupOk

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
        [ (EUserMessage,       "user_message")
        , (EAssistantReply,    "assistant_reply")
        , (EToolCall,          "tool_call")
        , (EToolResult,        "tool_result")
        , (EPolicyDecision,    "policy_decision")
        , (ESessionStart,      "session_start")
        , (ESessionEnd,        "session_end")
        , (EConversationReset, "conversation_reset")
        , (EConversationCompacted, "conversation_compacted")
        , (ERunLimitReached,   "run_limit_reached")
        ]
      fmtTypeCount (ety, label) =
        let n = Map.findWithDefault 0 ety (ssTypeCounts ss)
        in "  " <> label <> ": " <> T.pack (show n)
      backupLine = if ssBackupExists ss
        then "  Backup:        session.jsonl.1 exists"
        else "  Backup:        (none)"
  in "Session summary:\n"
  <> "  Log:           " <> T.pack (ssLogPath ss) <> "\n"
  <> "  Total events:  " <> T.pack (show total) <> "\n"
  <> timeRange <> "\n"
  <> T.unlines typeLines
  <> "  Malformed:     " <> T.pack (show malformed) <> "\n"
  <> backupLine

-- ---------------------------------------------------------------------------
-- Resume
-- ---------------------------------------------------------------------------

-- | The result of loading a session log for resume context.
data ResumeResult = ResumeResult
  { rrResumeContext      :: !Conversation   -- ^ Safe text messages used as resume context
  , rrMessageEventCount  :: !Int            -- ^ Conversation message events in the file (user + assistant, before reset filter)
  , rrMalformed          :: !Int            -- ^ Number of malformed lines skipped
  , rrParsedValid        :: !Int            -- ^ Total valid events parsed (all types)
  , rrSkipped            :: !Int            -- ^ Valid events skipped (tool, policy, lifecycle types)
  , rrUsedResetBoundary  :: !Bool           -- ^ True if a conversation_reset boundary was found
  , rrUsedCompactionBoundary :: !Bool       -- ^ True if a conversation_compacted boundary was found
  , rrMessageCount       :: !Int            -- ^ Safe text messages in the resume context (after reset filter)
  } deriving stock (Show)

-- | Load conservative resume context from @session.jsonl@ in the given directory.
--
--   * Returns 'Nothing' when the file does not exist or is empty.
--   * Skips malformed lines (counted in 'rrMalformed').
--   * Only the active @session.jsonl@ is read; rotated backups are ignored.
--   * Reconstructs only safe text messages; tool calls, tool results,
--     policy decisions, and lifecycle events are never replayed.
loadResumeEvents :: FilePath -> IO (Maybe ResumeResult)
loadResumeEvents dir = do
  let path = dir </> "session.jsonl"
  exists <- doesFileExist path
  if not exists
    then pure Nothing
    else do
      bytes <- LBS.readFile path
      if LBS.null bytes
        then pure Nothing
        else do
          let rawLines = LBS.split (fromIntegral (fromEnum '\n')) bytes
              -- Drop empty lines (e.g. trailing newline)
              nonEmpty = filter (not . LBS.null) rawLines
              (badLines, goodEvents) = partitionEithers
                [ case Aeson.decode line :: Maybe Event of
                    Nothing -> Left ()
                    Just ev -> Right ev
                | line <- nonEmpty
                ]
              -- Keep only events that can contribute to text resume context.
              -- Include EConversationReset so the converter can apply the
              -- last-reset boundary correctly. Include EConversationCompacted
              -- so resume can discard pre-compaction text and keep only the
              -- compact memory.
              resumeContextEvents = filter isResumeContextEvent goodEvents
              malCount  = length badLines
              validCount = length goodEvents
              resumeContextEventCount = length resumeContextEvents
              -- Skipped = valid events that are not safe text context
              -- (lifecycle, tool, and policy events are completely ignored).
              skippedCount = validCount - resumeContextEventCount
              hasReset = any (\ev -> evType ev == EConversationReset) goodEvents
              hasCompaction = any (\ev -> evType ev == EConversationCompacted) goodEvents
              -- Count only actual conversation messages (not reset markers)
              messageEventCount = length
                [ ev | ev <- resumeContextEvents
                , evType ev `elem` [EUserMessage, EAssistantReply]
                ]
              conv = resumeContextEventsToConversation resumeContextEvents
          pure $ Just ResumeResult
            { rrResumeContext     = conv
            , rrMessageEventCount = messageEventCount
            , rrMalformed         = malCount
            , rrParsedValid       = validCount
            , rrSkipped           = skippedCount
            , rrUsedResetBoundary = hasReset
            , rrUsedCompactionBoundary = hasCompaction
            , rrMessageCount      = length conv
            }
  where
    -- | Conservative resume: user messages, assistant replies, conversation
    --   reset boundaries, and accepted compaction memories contribute to
    --   resume context. Tool calls, tool results, policy decisions, and
    --   lifecycle events are intentionally skipped rather than replayed or
    --   restored.
    isResumeContextEvent :: Event -> Bool
    isResumeContextEvent ev =
      evType ev `elem`
        [ EUserMessage
        , EAssistantReply
        , EConversationReset
        , EConversationCompacted
        ]

-- | Convert resume-context events into safe text 'Conversation' messages.
--
--   * @EUserMessage@ → user message
--   * @EAssistantReply@ → assistant message (including any tool-call
--     summary text that was recorded in the event data)
--   * @EConversationReset@ clears the conversation (loads only events
--     after the last reset)
--   * @EConversationCompacted@ replaces the conversation with one safe
--     system-memory message, so old pre-compaction messages do not resume
--     as live context.
--
--   Events appear in the order provided (expected: chronological).
resumeContextEventsToConversation :: [Event] -> Conversation
resumeContextEventsToConversation = foldl step []
  where
    step :: Conversation -> Event -> Conversation
    step msgs ev = case evType ev of
      EUserMessage ->
        msgs ++ [mkUserMessage (evData ev)]
      EAssistantReply ->
        msgs ++ [mkAssistantMessage (evData ev)]
      EConversationReset ->
        -- Clear conversation on reset; resume from this point forward
        []
      EConversationCompacted ->
        [mkSystemMessage ("Compacted conversation memory:\n\n" <> evData ev)]
      _ -> msgs

-- | Format a concise resume summary for terminal output.
--
--   Shows the active log path, safe text message count, parsed
--   event counts, skipped events, malformed lines, and whether a
--   conversation reset boundary was found.
formatResumeSummary :: FilePath -> ResumeResult -> Text
formatResumeSummary logPath rr =
  T.unlines
    [ "Resumed from:   " <> T.pack logPath
    , "Messages:       " <> T.pack (show (rrMessageCount rr))
    , "Valid events:   " <> T.pack (show (rrParsedValid rr))
    , "Message events: " <> T.pack (show (rrMessageEventCount rr))
    , "Skipped:        " <> T.pack (show (rrSkipped rr))
    , "Malformed:      " <> T.pack (show (rrMalformed rr))
    , "Reset boundary: " <> (if rrUsedResetBoundary rr then "yes" else "no")
    , "Compact boundary: " <> (if rrUsedCompactionBoundary rr then "yes" else "no")
    ]
