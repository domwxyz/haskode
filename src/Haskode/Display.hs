{-# LANGUAGE OverloadedStrings #-}

-- | Pure formatting helpers for terminal output.
--
-- These functions produce consistent, bracketed-label output for
-- the agent loop, tool execution, policy decisions, and errors.
-- All output is plain text — no ANSI colors, no TUI dependencies.
--
-- Style:
--
-- * @[assistant]@, @[tool]@, @[policy]@, @[error]@, @[confirm]@ labels
-- * 2-space indent for body lines under a label
-- * Multiline content is indented uniformly
-- * Readable on dumb terminals and Windows consoles

module Haskode.Display
  ( -- * Indentation
    indentBlock
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

-- | Format an unknown-tool error.
--
-- Produces: @  [error] Unknown tool: <name>@
formatToolUnknown :: Text -> Text
formatToolUnknown name = "  [error] Unknown tool: " <> name

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
-- Includes estimated size, configured limit, over-limit delta, and a note
-- that Haskode does not auto-truncate or summarize.
--
-- >>> formatContextLimitRefusal 130000 120000
-- "Error: conversation is too large to send (130000 chars estimated, limit 120000).\n  ..."
formatContextLimitRefusal :: Int -> Int -> Text
formatContextLimitRefusal estimated maxChars =
  let overBy = estimated - maxChars
      overLine | overBy > 0 = "  Over limit by: " <> T.pack (show overBy) <> " chars\n"
               | otherwise  = ""
  in "Error: conversation is too large to send ("
     <> T.pack (show estimated) <> " chars estimated, limit "
     <> T.pack (show maxChars) <> ").\n"
     <> overLine
     <> "  Note: Haskode does not auto-truncate or summarize conversations.\n"
     <> "  Suggested next steps:\n"
     <> "    - Start a fresh session (/new or restart)\n"
     <> "    - Reduce the size of your prompt or context\n"
     <> "    - Raise cfgMaxContextChars in your config"

-- ---------------------------------------------------------------------------
-- Verbose diagnostics
-- ---------------------------------------------------------------------------

-- | Format a verbose diagnostic line.
--
-- Produces: @[verbose] <label>: <value>@
formatVerbose :: String -> String -> String
formatVerbose label value = "[verbose] " ++ label ++ ": " ++ value
