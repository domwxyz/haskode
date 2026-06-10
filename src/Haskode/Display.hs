{-# LANGUAGE OverloadedStrings #-}

-- | Pure formatting helpers for terminal output.
--
-- These functions produce consistent, bracketed-label output for
-- the agent loop, tool execution, policy decisions, and errors.
-- No ANSI colors, no TUI dependencies.
--
-- Style:
--
-- * @[assistant]@, @[tool]@, @[policy]@, @[error]@, @[confirm]@ labels
-- * 2-space indent for body lines under a label
-- * Multiline content is indented uniformly
-- * Readable on dumb terminals and Windows consoles
--
-- Note: The startup banner ('Haskode.Main.printBanner') and batch
-- preview separators ('Haskode.Patch.batchOpPreview') use Unicode
-- box-drawing characters (@┌@, @─@, @│@, @└@) for cosmetic
-- framing.  These are intentional and not required for functionality.
-- On Windows consoles without UTF-8 support, they may render as
-- fallback glyphs but the surrounding text remains readable.

module Haskode.Display
  ( -- * Indentation
    indentBlock
    -- * Structured display seam
  , DisplayEvent (..)
  , renderDisplayEvent
    -- * Assistant output
  , formatAssistantReply
    -- * Streaming assistant output
    --
    -- These helpers provide the display boundary for streaming
    -- providers.  The pure formatters are tested; the IO helpers
    -- centralise @putStr@/@hFlush@ so that 'Haskode.Agent'
    -- never needs to call them directly for streaming output.
    -- The OpenAI provider now uses these via the agent's streaming
    -- path.
  , formatStreamBegin
  , formatStreamEnd
  , streamBegin
  , streamChunk
  , streamEnd
    -- * Tool output
  , formatToolExecuting
  , formatToolResult
  , formatToolUnknown
    -- * Policy output
  , formatPolicyDenied
  , formatPolicyConfirmationNeeded
  , formatPolicyApproved
  , formatPolicyRejected
    -- * Confirmation flow
  , formatConfirmTool
  , formatConfirmArgs
  , formatConfirmReason
  , formatConfirmPrompt
  , formatConfirmFile
  , formatConfirmDiffHeader
  , formatConfirmPreviewHeader
    -- * Error output
  , formatError
    -- * Context limit refusal
  , formatContextLimitRefusal
    -- * Verbose diagnostics
  , formatVerbose
  ) where

import Data.Text          (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import System.IO             (hFlush, hSetBuffering, stdout, BufferMode (..))

-- ---------------------------------------------------------------------------
-- Structured display seam
-- ---------------------------------------------------------------------------

-- | Small structured boundary for terminal-visible agent output.
--
-- This is the CLI/TUI seam, not a general event bus.  The CLI renders these
-- events back to the existing terminal strings; a future TUI can consume the
-- same values without parsing formatted text.  Streaming and confirmation
-- previews intentionally stay on their existing terminal-specific paths for
-- now.
data DisplayEvent
  = DisplayAssistant Text
  | DisplayToolExecuting Text
  | DisplayToolResult Text
  | DisplayToolUnknown Text
  | DisplayPolicyDenied Text Text
  | DisplayPolicyConfirmationNeeded Text
  | DisplayPolicyApproved
  | DisplayPolicyRejected
  | DisplayError Text
  | DisplayContextLimitRefusal Int Int
  deriving (Eq, Show)

-- | Render one display event to the same plain terminal text used by the
-- existing CLI.  ANSI color remains outside this boundary.
renderDisplayEvent :: DisplayEvent -> Text
renderDisplayEvent event =
  case event of
    DisplayAssistant content ->
      formatAssistantReply content
    DisplayToolExecuting name ->
      formatToolExecuting name
    DisplayToolResult output ->
      formatToolResult output
    DisplayToolUnknown name ->
      formatToolUnknown name
    DisplayPolicyDenied name reason ->
      formatPolicyDenied name reason
    DisplayPolicyConfirmationNeeded name ->
      formatPolicyConfirmationNeeded name
    DisplayPolicyApproved ->
      formatPolicyApproved
    DisplayPolicyRejected ->
      formatPolicyRejected
    DisplayError msg ->
      formatError msg
    DisplayContextLimitRefusal estimated maxChars ->
      formatAssistantReply (formatContextLimitRefusal estimated maxChars)

-- ---------------------------------------------------------------------------
-- Indentation
-- ---------------------------------------------------------------------------

-- | Indent every line of a text block by a given number of spaces.
--
-- >>> indentBlock 2 "line1\nline2\n"
-- "  line1\n  line2\n"
--
-- Empty input returns empty output.
indentBlock :: Int -> Text -> Text
indentBlock n t
  | T.null t  = ""
  | otherwise = T.unlines . map (indentPrefix <>) $ T.lines t
  where
    indentPrefix = T.replicate n " "

-- ---------------------------------------------------------------------------
-- Assistant output
-- ---------------------------------------------------------------------------

-- | Format an assistant reply for terminal display.
--
-- Produces:
--
-- @
--
-- Assistant: <content>
--
-- @
formatAssistantReply :: Text -> Text
formatAssistantReply content = "\nAssistant: " <> content <> "\n"

-- ---------------------------------------------------------------------------
-- Streaming assistant output
--
-- These helpers provide the display boundary for streaming providers.
-- The pure formatters ('formatStreamBegin', 'formatStreamEnd') produce
-- the label text and are unit-tested.
--
-- The IO helpers ('streamBegin', 'streamChunk', 'streamEnd') centralise
-- @putStr@ and @hFlush stdout@ so that 'Haskode.Agent' never needs to
-- call them directly.  The OpenAI provider uses these via the agent's
-- streaming path.
-- ---------------------------------------------------------------------------

-- | Pure formatter: the label that precedes streaming assistant text.
--
-- Produces: @"\nAssistant: "@
formatStreamBegin :: Text
formatStreamBegin = "\nAssistant: "

-- | Pure formatter: the suffix that follows streaming assistant text.
--
-- Produces: @"\n"@
formatStreamEnd :: Text
formatStreamEnd = "\n"

-- | Begin streaming assistant output.
--
-- Prints the @"\\nAssistant: "@ label and sets stdout to
-- 'NoBuffering' so that subsequent 'streamChunk' calls
-- appear immediately on the terminal.
streamBegin :: IO ()
streamBegin = do
  hSetBuffering stdout NoBuffering
  TIO.putStr formatStreamBegin
  hFlush stdout

-- | Append one text chunk to the streaming assistant output.
--
-- Prints the chunk and flushes stdout.  Safe to call from a
-- token callback; does not add newlines.
streamChunk :: Text -> IO ()
streamChunk chunk = do
  TIO.putStr chunk
  hFlush stdout

-- | End streaming assistant output.
--
-- Prints a trailing newline and restores stdout buffering to
-- 'LineBuffering' (the common default).
streamEnd :: IO ()
streamEnd = do
  TIO.putStr formatStreamEnd
  hFlush stdout
  hSetBuffering stdout LineBuffering

-- ---------------------------------------------------------------------------
-- Tool output
-- ---------------------------------------------------------------------------

-- | Format a tool-execution notice.
--
-- Produces: @  [tool] Executing: <name>@
formatToolExecuting :: Text -> Text
formatToolExecuting name = "  [tool] Executing: " <> name

-- | Format a tool result for terminal display.
--
-- Produces: @  [tool] Result: <output>@
formatToolResult :: Text -> Text
formatToolResult output = "  [tool] Result: " <> output

-- | Format an unknown-or-disabled-tool error.
--
-- Produces: @  [error] Unknown or disabled tool: <name>@
formatToolUnknown :: Text -> Text
formatToolUnknown name = "  [error] Unknown or disabled tool: " <> name

-- ---------------------------------------------------------------------------
-- Policy output
-- ---------------------------------------------------------------------------

-- | Format a policy denial.
--
-- Produces: @  [policy] Denied: <name> -- <reason>@
formatPolicyDenied :: Text -> Text -> Text
formatPolicyDenied name reason =
  "  [policy] Denied: " <> name <> " -- " <> reason

-- | Format a policy confirmation-needed notice.
--
-- Produces: @  [policy] Confirmation needed: <name>@
formatPolicyConfirmationNeeded :: Text -> Text
formatPolicyConfirmationNeeded name =
  "  [policy] Confirmation needed: " <> name

-- | Format a policy approval notice.
--
-- Produces: @  [policy] Approved by user.@
formatPolicyApproved :: Text
formatPolicyApproved = "  [policy] Approved by user."

-- | Format a policy rejection notice.
--
-- Produces: @  [policy] Rejected by user.@
formatPolicyRejected :: Text
formatPolicyRejected = "  [policy] Rejected by user."

-- ---------------------------------------------------------------------------
-- Confirmation flow
-- ---------------------------------------------------------------------------

-- | Format the tool name line in a confirmation prompt.
--
-- Produces: @  [confirm] Tool:     <name>@
formatConfirmTool :: Text -> Text
formatConfirmTool name = "  [confirm] Tool:     " <> name

-- | Format the args line in a confirmation prompt.
--
-- Produces: @  [confirm] Args:     <args>@
formatConfirmArgs :: Text -> Text
formatConfirmArgs args = "  [confirm] Args:     " <> args

-- | Format the reason line in a confirmation prompt.
--
-- Produces: @  [confirm] Reason:   <reason>@
formatConfirmReason :: Text -> Text
formatConfirmReason reason = "  [confirm] Reason:   " <> reason

-- | Format the approve prompt.
--
-- Produces: @  [confirm] Approve? (y/N) @
formatConfirmPrompt :: Text
formatConfirmPrompt = "  [confirm] Approve? (y/N) "

-- | Format the file path line in a confirmation preview.
--
-- Produces: @  [confirm] File:     <path>@
formatConfirmFile :: Text -> Text
formatConfirmFile path = "  [confirm] File:     " <> path

-- | Format the diff header in a confirmation preview.
--
-- Produces: @  [confirm] Diff:@
formatConfirmDiffHeader :: Text
formatConfirmDiffHeader = "  [confirm] Diff:"

-- | Format the preview header in a confirmation preview.
--
-- Produces: @  [confirm] Preview:@
formatConfirmPreviewHeader :: Text
formatConfirmPreviewHeader = "  [confirm] Preview:"

-- ---------------------------------------------------------------------------
-- Error output
-- ---------------------------------------------------------------------------

-- | Format an error message.
--
-- Produces: @  [error] <message>@
formatError :: Text -> Text
formatError msg = "  [error] " <> msg

-- ---------------------------------------------------------------------------
-- Context limit refusal
-- ---------------------------------------------------------------------------

-- | Format a refusal message when the conversation exceeds the context limit.
--
-- Includes estimated size, configured limit, percentage used, over-limit
-- delta, and a note that this is a character-based estimate (not token-based).
-- The layout mirrors 'Haskode.Commands.formatContextUsage' used by /status.
--
-- >>> formatContextLimitRefusal 130000 120000
-- "Error: conversation is too large to send\n  Context est: 130000 chars (108% used)\n  ..."
formatContextLimitRefusal :: Int -> Int -> Text
formatContextLimitRefusal estimated maxChars =
  let overBy = estimated - maxChars
      pct    = if maxChars > 0 then estimated * 100 `div` maxChars else 0
  in "Error: conversation is too large to send\n"
     <> "  Context est:    " <> T.pack (show estimated) <> " chars ("
                            <> T.pack (show pct) <> "% used)\n"
     <> "  Limit:          " <> T.pack (show maxChars) <> " chars\n"
     <> "  Over limit by:  " <> T.pack (show overBy) <> " chars\n"
     <> "  Note: This is a character-based estimate, not a token count.\n"
     <> "  Haskode does not auto-truncate or summarize conversations.\n"
     <> "  Suggested next steps:\n"
     <> "    - Use /new to start a fresh session\n"
     <> "    - Reduce the size of your prompt or context\n"
     <> "    - Start a new session or raise cfgMaxContextChars"

-- ---------------------------------------------------------------------------
-- Verbose diagnostics
-- ---------------------------------------------------------------------------

-- | Format a verbose diagnostic line.
--
-- Produces: @[verbose] <label>: <value>@
formatVerbose :: String -> String -> String
formatVerbose label value = "[verbose] " ++ label ++ ": " ++ value
