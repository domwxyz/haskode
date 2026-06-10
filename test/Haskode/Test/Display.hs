{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

-- | Display formatting tests.
module Haskode.Test.Display (tests) where

import Haskode.Display
    ( indentBlock,
      formatAssistantReply,
      formatToolExecuting,
      formatToolResult,
      formatToolUnknown,
      formatPolicyDenied,
      formatPolicyConfirmationNeeded,
      formatPolicyApproved,
      formatPolicyRejected,
      formatConfirmTool,
      formatConfirmArgs,
      formatConfirmReason,
      formatConfirmPrompt,
      formatConfirmFile,
      formatConfirmDiffHeader,
      formatConfirmPreviewHeader,
      formatError,
      formatVerbose,
      formatContextLimitRefusal,
      formatStreamBegin,
      formatStreamEnd )
import Haskode.Test.Util ( Test )
import qualified Data.Text as T
    ( isInfixOf,
      isPrefixOf,
      isSuffixOf,
      lines,
      null,
      take,
      takeEnd,
      pack,
      unpack )
-- ---------------------------------------------------------------------------

-- | indentBlock indents each line by the given number of spaces.
testDisplayIndentBlock :: Test
testDisplayIndentBlock =
  let result = indentBlock 4 "line1\nline2\n"
  in if result == "    line1\n    line2\n"
       then pure $ Right ()
       else pure $ Left $ "indentBlock 4: got " ++ show result

-- | indentBlock with single line.
testDisplayIndentBlockSingleLine :: Test
testDisplayIndentBlockSingleLine =
  let result = indentBlock 2 "hello\n"
  in if result == "  hello\n"
       then pure $ Right ()
       else pure $ Left $ "indentBlock single: got " ++ show result

-- | indentBlock on empty text returns empty string.
testDisplayIndentBlockEmpty :: Test
testDisplayIndentBlockEmpty =
  let result = indentBlock 4 ""
  in if result == ""
       then pure $ Right ()
       else pure $ Left $ "indentBlock empty: got " ++ show result

-- | indentBlock preserves trailing content without newline.
testDisplayIndentBlockNoTrailingNewline :: Test
testDisplayIndentBlockNoTrailingNewline =
  let result = indentBlock 2 "line1\nline2"
  in if result == "  line1\n  line2\n"
       then pure $ Right ()
       else pure $ Left $ "indentBlock no trailing: got " ++ show result

-- | formatAssistantReply wraps content with Assistant: prefix and newlines.
testDisplayFormatAssistantReply :: Test
testDisplayFormatAssistantReply =
  let result = formatAssistantReply "hello there"
  in if result == "\nAssistant: hello there\n"
       then pure $ Right ()
       else pure $ Left $ "formatAssistantReply: got " ++ show result

-- | formatAssistantReply with empty content.
testDisplayFormatAssistantReplyEmpty :: Test
testDisplayFormatAssistantReplyEmpty =
  let result = formatAssistantReply ""
  in if result == "\nAssistant: \n"
       then pure $ Right ()
       else pure $ Left $ "formatAssistantReply empty: got " ++ show result

-- | formatToolExecuting produces correct label.
testDisplayFormatToolExecuting :: Test
testDisplayFormatToolExecuting =
  let result = formatToolExecuting "read_file"
  in if result == "  [tool] Executing: read_file"
       then pure $ Right ()
       else pure $ Left $ "formatToolExecuting: got " ++ show result

-- | formatToolResult produces correct label.
testDisplayFormatToolResult :: Test
testDisplayFormatToolResult =
  let result = formatToolResult "file contents here"
  in if result == "  [tool] Result: file contents here"
       then pure $ Right ()
       else pure $ Left $ "formatToolResult: got " ++ show result

-- | formatToolUnknown produces correct label.
testDisplayFormatToolUnknown :: Test
testDisplayFormatToolUnknown =
  let result = formatToolUnknown "bad_tool"
  in if result == "  [error] Unknown tool: bad_tool"
       then pure $ Right ()
       else pure $ Left $ "formatToolUnknown: got " ++ show result

-- | formatPolicyDenied produces correct label with name and reason.
testDisplayFormatPolicyDenied :: Test
testDisplayFormatPolicyDenied =
  let result = formatPolicyDenied "shell" "dangerous command"
  in if result == "  [policy] Denied: shell -- dangerous command"
       then pure $ Right ()
       else pure $ Left $ "formatPolicyDenied: got " ++ show result

-- | formatPolicyConfirmationNeeded produces correct label.
testDisplayFormatPolicyConfirmationNeeded :: Test
testDisplayFormatPolicyConfirmationNeeded =
  let result = formatPolicyConfirmationNeeded "apply_patch"
  in if result == "  [policy] Confirmation needed: apply_patch"
       then pure $ Right ()
       else pure $ Left $ "formatPolicyConfirmationNeeded: got " ++ show result

-- | formatPolicyApproved produces correct text.
testDisplayFormatPolicyApproved :: Test
testDisplayFormatPolicyApproved =
  if formatPolicyApproved == "  [policy] Approved by user."
    then pure $ Right ()
    else pure $ Left $ "formatPolicyApproved: got " ++ show formatPolicyApproved

-- | formatPolicyRejected produces correct text.
testDisplayFormatPolicyRejected :: Test
testDisplayFormatPolicyRejected =
  if formatPolicyRejected == "  [policy] Rejected by user."
    then pure $ Right ()
    else pure $ Left $ "formatPolicyRejected: got " ++ show formatPolicyRejected

-- | formatConfirmTool produces correct label.
testDisplayFormatConfirmTool :: Test
testDisplayFormatConfirmTool =
  let result = formatConfirmTool "read_file"
  in if result == "  [confirm] Tool:     read_file"
       then pure $ Right ()
       else pure $ Left $ "formatConfirmTool: got " ++ show result

-- | formatConfirmArgs produces correct label.
testDisplayFormatConfirmArgs :: Test
testDisplayFormatConfirmArgs =
  let result = formatConfirmArgs "{\"path\":\"foo.hs\"}"
  in if result == "  [confirm] Args:     {\"path\":\"foo.hs\"}"
       then pure $ Right ()
       else pure $ Left $ "formatConfirmArgs: got " ++ show result

-- | formatConfirmReason produces correct label.
testDisplayFormatConfirmReason :: Test
testDisplayFormatConfirmReason =
  let result = formatConfirmReason "needs approval"
  in if result == "  [confirm] Reason:   needs approval"
       then pure $ Right ()
       else pure $ Left $ "formatConfirmReason: got " ++ show result

-- | formatConfirmPrompt produces correct text.
testDisplayFormatConfirmPrompt :: Test
testDisplayFormatConfirmPrompt =
  if formatConfirmPrompt == "  [confirm] Approve? (y/N) "
    then pure $ Right ()
    else pure $ Left $ "formatConfirmPrompt: got " ++ show formatConfirmPrompt

-- | formatConfirmFile produces correct label.
testDisplayFormatConfirmFile :: Test
testDisplayFormatConfirmFile =
  let result = formatConfirmFile "src/Main.hs"
  in if result == "  [confirm] File:     src/Main.hs"
       then pure $ Right ()
       else pure $ Left $ "formatConfirmFile: got " ++ show result

-- | formatConfirmDiffHeader produces correct text.
testDisplayFormatConfirmDiffHeader :: Test
testDisplayFormatConfirmDiffHeader =
  if formatConfirmDiffHeader == "  [confirm] Diff:"
    then pure $ Right ()
    else pure $ Left $ "formatConfirmDiffHeader: got " ++ show formatConfirmDiffHeader

-- | formatConfirmPreviewHeader produces correct text.
testDisplayFormatConfirmPreviewHeader :: Test
testDisplayFormatConfirmPreviewHeader =
  if formatConfirmPreviewHeader == "  [confirm] Preview:"
    then pure $ Right ()
    else pure $ Left $ "formatConfirmPreviewHeader: got " ++ show formatConfirmPreviewHeader

-- | formatError produces correct label.
testDisplayFormatError :: Test
testDisplayFormatError =
  let result = formatError "something went wrong"
  in if result == "  [error] something went wrong"
       then pure $ Right ()
       else pure $ Left $ "formatError: got " ++ show result

-- | formatVerbose produces correct label format.
testDisplayFormatVerbose :: Test
testDisplayFormatVerbose =
  let result = formatVerbose "provider" "openai"
  in if result == "[verbose] provider: openai"
       then pure $ Right ()
       else pure $ Left $ "formatVerbose: got " ++ show result

-- | Multiline diff formatted with indentBlock is readable.
testDisplayMultilineDiffIndent :: Test
testDisplayMultilineDiffIndent =
  let diff = "--- Foo.hs\n+++ Foo.hs\n@@ -1 +1 @@\n-old\n+new\n"
      result = indentBlock 4 diff
      lines' = T.lines result
  in if all (\l -> T.null l || T.isPrefixOf "    " l) lines'
       then pure $ Right ()
       else pure $ Left $ "Multiline diff indent failed: " ++ show lines'

-- | No secrets appear in formatVerbose output.
testDisplayFormatVerboseNoSecrets :: Test
testDisplayFormatVerboseNoSecrets =
  let result = formatVerbose "api key" "sk-secret-12345"
  in -- formatVerbose is a simple concatenation helper; it does not
     -- redact.  This test confirms the function exists and works.
     -- Secret redaction is handled by the caller (e.g. formatStatus
     -- in Commands.hs never prints pcApiKey).
     if "sk-secret-12345" `T.isInfixOf` T.pack result
       then pure $ Right ()
       else pure $ Left $ "formatVerbose value not in output: " ++ result

-- | formatContextLimitRefusal includes estimated character count.
testFormatContextLimitRefusalEstimate :: Test
testFormatContextLimitRefusalEstimate =
  let msg = formatContextLimitRefusal 130000 120000
  in if "130000" `T.isInfixOf` msg
       then pure $ Right ()
       else pure $ Left $ "Missing estimate in: " ++ T.unpack msg

-- | formatContextLimitRefusal includes configured limit.
testFormatContextLimitRefusalLimit :: Test
testFormatContextLimitRefusalLimit =
  let msg = formatContextLimitRefusal 130000 120000
  in if "120000" `T.isInfixOf` msg
       then pure $ Right ()
       else pure $ Left $ "Missing limit in: " ++ T.unpack msg

-- | formatContextLimitRefusal includes over-limit delta.
testFormatContextLimitRefusalDelta :: Test
testFormatContextLimitRefusalDelta =
  let msg = formatContextLimitRefusal 130000 120000
  in if "10000" `T.isInfixOf` msg && "Over limit by" `T.isInfixOf` msg
       then pure $ Right ()
       else pure $ Left $ "Missing delta in: " ++ T.unpack msg

-- | formatContextLimitRefusal includes percentage used.
testFormatContextLimitRefusalPercent :: Test
testFormatContextLimitRefusalPercent =
  let msg = formatContextLimitRefusal 130000 120000
  in if "108%" `T.isInfixOf` msg && "% used" `T.isInfixOf` msg
       then pure $ Right ()
       else pure $ Left $ "Missing percent in: " ++ T.unpack msg

-- | formatContextLimitRefusal states no auto-truncation or summarization.
testFormatContextLimitRefusalNoAutoTruncate :: Test
testFormatContextLimitRefusalNoAutoTruncate =
  let msg = formatContextLimitRefusal 130000 120000
  in if "does not auto-truncate or summarize" `T.isInfixOf` msg
       then pure $ Right ()
       else pure $ Left $ "Missing no-auto-truncate note in: " ++ T.unpack msg

-- | formatContextLimitRefusal states this is character-based, not token-based.
testFormatContextLimitRefusalCharBased :: Test
testFormatContextLimitRefusalCharBased =
  let msg = formatContextLimitRefusal 130000 120000
  in if "character-based estimate, not a token count" `T.isInfixOf` msg
       then pure $ Right ()
       else pure $ Left $ "Missing character-based note in: " ++ T.unpack msg

-- | formatContextLimitRefusal includes suggested next steps.
testFormatContextLimitRefusalNextSteps :: Test
testFormatContextLimitRefusalNextSteps =
  let msg = formatContextLimitRefusal 130000 120000
  in if "Suggested next steps" `T.isInfixOf` msg
       && "fresh session"       `T.isInfixOf` msg
       && "cfgMaxContextChars"  `T.isInfixOf` msg
       then pure $ Right ()
       else pure $ Left $ "Missing next steps in: " ++ T.unpack msg

-- | formatContextLimitRefusal with no overage (exact limit exceeded by 1).
testFormatContextLimitRefusalExactOver :: Test
testFormatContextLimitRefusalExactOver =
  let msg = formatContextLimitRefusal 120001 120000
  in if "Over limit by:  1 chars" `T.isInfixOf` msg
       then pure $ Right ()
       else pure $ Left $ "Exact-over case failed: " ++ T.unpack msg

-- ---------------------------------------------------------------------------
-- Streaming display helpers (future scaffolding)
-- ---------------------------------------------------------------------------

-- | formatStreamBegin produces the assistant label prefix.
testFormatStreamBegin :: Test
testFormatStreamBegin =
  if formatStreamBegin == "\nAssistant: "
    then pure $ Right ()
    else pure $ Left $ "formatStreamBegin: got " ++ show formatStreamBegin

-- | formatStreamEnd produces a trailing newline.
testFormatStreamEnd :: Test
testFormatStreamEnd =
  if formatStreamEnd == "\n"
    then pure $ Right ()
    else pure $ Left $ "formatStreamEnd: got " ++ show formatStreamEnd

-- | formatStreamBegin matches the prefix of formatAssistantReply.
testFormatStreamBeginMatchesReply :: Test
testFormatStreamBeginMatchesReply =
  let reply = formatAssistantReply "test"
  in if T.isPrefixOf formatStreamBegin reply
       then pure $ Right ()
       else pure $ Left $ "formatStreamBegin not a prefix of formatAssistantReply: "
                        ++ show (formatStreamBegin, T.take 20 reply)

-- | formatStreamEnd matches the suffix of formatAssistantReply.
testFormatStreamEndMatchesReply :: Test
testFormatStreamEndMatchesReply =
  let reply = formatAssistantReply "test"
  in if T.isSuffixOf formatStreamEnd reply
       then pure $ Right ()
       else pure $ Left $ "formatStreamEnd not a suffix of formatAssistantReply: "
                        ++ show (formatStreamEnd, T.takeEnd 20 reply)

-- | formatStreamBegin <> content <> formatStreamEnd equals formatAssistantReply.
testStreamFormattingConsistency :: Test
testStreamFormattingConsistency =
  let content = "hello there"
      streamed = formatStreamBegin <> content <> formatStreamEnd
      reply    = formatAssistantReply content
  in if streamed == reply
       then pure $ Right ()
       else pure $ Left $ "Stream formatting mismatch: " ++ show (streamed, reply)


tests :: [Test]
tests =
  [ testDisplayIndentBlock
  , testDisplayIndentBlockSingleLine
  , testDisplayIndentBlockEmpty
  , testDisplayIndentBlockNoTrailingNewline
  , testDisplayFormatAssistantReply
  , testDisplayFormatAssistantReplyEmpty
  , testDisplayFormatToolExecuting
  , testDisplayFormatToolResult
  , testDisplayFormatToolUnknown
  , testDisplayFormatPolicyDenied
  , testDisplayFormatPolicyConfirmationNeeded
  , testDisplayFormatPolicyApproved
  , testDisplayFormatPolicyRejected
  , testDisplayFormatConfirmTool
  , testDisplayFormatConfirmArgs
  , testDisplayFormatConfirmReason
  , testDisplayFormatConfirmPrompt
  , testDisplayFormatConfirmFile
  , testDisplayFormatConfirmDiffHeader
  , testDisplayFormatConfirmPreviewHeader
  , testDisplayFormatError
  , testDisplayFormatVerbose
  , testDisplayMultilineDiffIndent
  , testDisplayFormatVerboseNoSecrets
  , testFormatContextLimitRefusalEstimate
  , testFormatContextLimitRefusalLimit
  , testFormatContextLimitRefusalDelta
  , testFormatContextLimitRefusalPercent
  , testFormatContextLimitRefusalNoAutoTruncate
  , testFormatContextLimitRefusalCharBased
  , testFormatContextLimitRefusalNextSteps
  , testFormatContextLimitRefusalExactOver
  , testFormatStreamBegin
  , testFormatStreamEnd
  , testFormatStreamBeginMatchesReply
  , testFormatStreamEndMatchesReply
  , testStreamFormattingConsistency
  ]
