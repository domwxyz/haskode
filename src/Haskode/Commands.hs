{-# LANGUAGE OverloadedStrings #-}

-- | Pure helpers for interactive slash commands.
--
-- These functions handle parsing and formatting for the interactive
-- REPL commands (/help, /status, /exit).  Keeping them pure makes
-- them easy to test without IO.

module Haskode.Commands
  ( parseSlashCommand
  , formatHelp
  , formatStatus
  , formatUnknownCommand
  , formatNewConfirmation
  , resetConversation
  , formatContextUsage
  ) where

import Data.Text          (Text)
import qualified Data.Text as T

import Haskode.Agent      (AgentState (..), estimateContextChars)
import Haskode.Config     (Config (..), ProviderConfig (..))
import Haskode.Core       (Conversation, emptyConversation)
import Haskode.Session    (events)
import Haskode.Tools      (toolNames)

-- ---------------------------------------------------------------------------
-- Command parsing
-- ---------------------------------------------------------------------------

-- | Parse a slash command from user input.
--
--   * @\"\/help\"@  → @Just \"help\"@
--   * @\"\/foo\"@   → @Just \"foo\"@
--   * @\"hello\"@   → @Nothing@
--
-- Leading and trailing whitespace is stripped before checking for the
-- leading @\'\/\'@.
parseSlashCommand :: Text -> Maybe Text
parseSlashCommand input =
  case T.strip input of
    t | T.null t      -> Nothing
      | T.head t == '/' -> Just (T.tail t)
      | otherwise       -> Nothing

-- ---------------------------------------------------------------------------
-- Help
-- ---------------------------------------------------------------------------

-- | Concise help text for interactive mode.
formatHelp :: Text
formatHelp = T.unlines
  [ "Interactive commands:"
  , "  /help    — show this help"
  , "  /new     — start a fresh conversation"
  , "  /status  — show current runtime/config status"
  , "  /exit    — save session log and exit"
  , "  /quit    — same as /exit"
  ]

-- ---------------------------------------------------------------------------
-- Status
-- ---------------------------------------------------------------------------

-- | Format current runtime status.
--
-- Shows provider, model, base URL, working directory, context limits,
-- session event count, and registered tool names.
--
-- API keys are never printed.
formatStatus :: AgentState -> Text
formatStatus st =
  let cfg = asConfig st
      pc  = cfgProvider cfg
      evs = events (asSession st)
      maxC = cfgMaxContextChars cfg
  in T.unlines
       [ "Provider:        " <> T.pack (pcProvider pc)
       , "Model:           " <> T.pack (pcModel pc)
       , "Base URL:        " <> T.pack (pcBaseUrl pc)
       , "Working dir:     " <> T.pack (cfgWorkingDir cfg)
       , "Max context:     " <> T.pack (show maxC) <> " chars"
       , formatContextUsage (asConversation st) maxC
       , "Max session log: " <> T.pack (show (cfgMaxSessionLogBytes cfg)) <> " bytes"
       , "Session events:  " <> T.pack (show (length evs))
       , "Tools:           " <> T.intercalate ", " (toolNames (asRegistry st))
       ]

-- ---------------------------------------------------------------------------
-- Context usage
-- ---------------------------------------------------------------------------

-- | Estimated conversation context usage relative to the configured limit.
--
-- Uses the same character-estimate logic as the context guardrail in
-- "Haskode.Agent".  Shows estimated chars, limit, and remaining headroom
-- (or overage).  This is an estimate, not exact token count.
formatContextUsage :: Conversation -> Int -> Text
formatContextUsage conv maxChars =
  let est     = estimateContextChars conv
      overBy  = est - maxChars
      pct     = if maxChars > 0 then est * 100 `div` maxChars else 0
      overLine
        | overBy > 0 = "Over limit:     " <> T.pack (show overBy) <> " chars"
        | otherwise  = "Remaining:      " <> T.pack (show (maxChars - est)) <> " chars"
  in T.unlines
       [ "Context est:    " <> T.pack (show est) <> " chars (" <> T.pack (show pct) <> "% used)"
       , overLine
       ]

-- ---------------------------------------------------------------------------
-- Unknown command
-- ---------------------------------------------------------------------------

-- | Format a message for an unrecognized slash command.
formatUnknownCommand :: Text -> Text
formatUnknownCommand cmd =
  "Unknown command: /" <> cmd <> ". Type /help for commands."

-- ---------------------------------------------------------------------------
-- /new
-- ---------------------------------------------------------------------------

-- | Confirmation message printed after /new resets the conversation.
formatNewConfirmation :: Text
formatNewConfirmation = "Started a fresh conversation."

-- | Reset an agent state's conversation to empty.
--
-- Session events are left untouched; only the in-memory conversation
-- is cleared so the next user prompt starts a fresh agent context.
resetConversation :: AgentState -> AgentState
resetConversation st = st { asConversation = emptyConversation }
