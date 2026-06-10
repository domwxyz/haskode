{-# LANGUAGE OverloadedStrings #-}

-- | Pure helper tests for the minimal Brick TUI.
module Haskode.Test.Tui (tests) where

import Haskode.Agent (autoApprove, initState)
import Haskode.Commands (CommandSpec (..), commandRegistry)
import Haskode.Config (defaultConfig)
import Haskode.Display (DisplayEvent (..))
import Haskode.Policy (defaultPolicy)
import Haskode.Provider (stubProvider)
import Haskode.Test.Util (Test)
import Haskode.Tools (defaultRegistry)
import Haskode.Tui
  ( TuiEntry (..)
  , TuiEntryKind (..)
  , TuiConfirmation (..)
  , TuiConfirmationInput (..)
  , TuiUpdate (..)
  , displayEventToTuiUpdate
  , formatTuiConfirmationLines
  , formatTuiHelp
  , formatTuiStatusLine
  , streamChunksToEntry
  , tuiConfirmationInputDecision
  , truncateEntryText
  , truncateArgsText
  , deleteWordBackward
  , deleteToLineStart
  )
import qualified Data.Text as T

testDisplayAssistantEntry :: Test
testDisplayAssistantEntry =
  let update = displayEventToTuiUpdate (DisplayAssistant "hello")
  in if tuiUpdateEntries update == [TuiEntry TuiAssistantEntry "hello"]
        && tuiUpdateStatus update == Just "Assistant replied."
       then pure $ Right ()
       else pure $ Left $ "DisplayAssistant update mismatch: " ++ show update

testDisplayToolActivity :: Test
testDisplayToolActivity =
  let update = displayEventToTuiUpdate (DisplayToolExecuting "read_file")
  in if tuiUpdateEntries update == [TuiEntry TuiToolEntry "Executing: read_file"]
        && tuiUpdateToolActivity update == Just "Running: read_file"
       then pure $ Right ()
       else pure $ Left $ "DisplayToolExecuting update mismatch: " ++ show update

testDisplayToolUnknown :: Test
testDisplayToolUnknown =
  let update = displayEventToTuiUpdate (DisplayToolUnknown "shell")
  in if tuiUpdateEntries update == [TuiEntry TuiSystemEntry "Unknown or disabled tool: shell"]
        && tuiUpdateStatus update == Just "Unknown or disabled tool: shell"
       then pure $ Right ()
       else pure $ Left $ "DisplayToolUnknown update mismatch: " ++ show update

testDisplayPolicyRejected :: Test
testDisplayPolicyRejected =
  let update = displayEventToTuiUpdate DisplayPolicyRejected
  in if tuiUpdateEntries update == [TuiEntry TuiSystemEntry "Policy rejected by user."]
        && tuiUpdateStatus update == Just "Policy rejected."
       then pure $ Right ()
       else pure $ Left $ "DisplayPolicyRejected update mismatch: " ++ show update

testDisplayContextLimit :: Test
testDisplayContextLimit =
  let update = displayEventToTuiUpdate (DisplayContextLimitRefusal 130000 120000)
      text = T.intercalate "\n" (map tuiEntryText (tuiUpdateEntries update))
  in if tuiUpdateStatus update == Just "Context limit exceeded."
        && "character-based estimate" `T.isInfixOf` text
       then pure $ Right ()
       else pure $ Left $ "DisplayContextLimitRefusal update mismatch: " ++ show update

testStreamChunksToEntry :: Test
testStreamChunksToEntry =
  if streamChunksToEntry ["hello", " ", "there"]
      == Just (TuiEntry TuiAssistantEntry "hello there")
    then pure $ Right ()
    else pure $ Left "streamChunksToEntry should assemble assistant text"

testStreamChunksToEntryEmpty :: Test
testStreamChunksToEntryEmpty =
  if streamChunksToEntry [] == Nothing
    then pure $ Right ()
    else pure $ Left "streamChunksToEntry should ignore empty streams"

testFormatTuiHelpUsesRegistry :: Test
testFormatTuiHelpUsesRegistry =
  let tuiCommands = filter cmdAvailableInTui commandRegistry
      missing =
        [ "/" <> cmdName spec
        | spec <- tuiCommands
        , not (T.isInfixOf ("/" <> cmdName spec) (formatTuiHelp commandRegistry))
        ]
  in if null missing
       then pure $ Right ()
       else pure $ Left $ "formatTuiHelp missing commands: " ++ show missing

testFormatTuiStatusLine :: Test
testFormatTuiStatusLine =
  let state = initState defaultConfig stubProvider defaultPolicy defaultRegistry autoApprove False
      out = formatTuiStatusLine state
  in if "Provider: stub" `T.isInfixOf` out
        && "Model: stub" `T.isInfixOf` out
        && "Streaming: no" `T.isInfixOf` out
        && "Resumed: no" `T.isInfixOf` out
       then pure $ Right ()
       else pure $ Left $ "formatTuiStatusLine mismatch: " ++ T.unpack out

testFormatTuiConfirmationLines :: Test
testFormatTuiConfirmationLines =
  let confirmation = TuiConfirmation
        { tuiConfirmationTool = "apply_patch"
        , tuiConfirmationReason = "patch Foo.hs -- no policy rule matched"
        , tuiConfirmationArgs = "{\"path\":\"Foo.hs\"}"
        , tuiConfirmationPreview =
            [ "File: Foo.hs"
            , "Diff:"
            , "--- Foo.hs"
            , "+++ Foo.hs"
            , "-old"
            , "+new"
            ]
        }
      out = T.unlines (formatTuiConfirmationLines confirmation)
  in if "Tool: apply_patch" `T.isInfixOf` out
        && "Reason: patch Foo.hs" `T.isInfixOf` out
        && "Args: {\"path\":\"Foo.hs\"}" `T.isInfixOf` out
        && "Preview:" `T.isInfixOf` out
        && "--- Foo.hs" `T.isInfixOf` out
        && "y = approve" `T.isInfixOf` out
       then pure $ Right ()
       else pure $ Left $ "formatTuiConfirmationLines mismatch: " ++ T.unpack out

testFormatTuiConfirmationWithoutPreview :: Test
testFormatTuiConfirmationWithoutPreview =
  let confirmation = TuiConfirmation
        { tuiConfirmationTool = "shell"
        , tuiConfirmationReason = "no policy rule matched"
        , tuiConfirmationArgs = "{\"command\":\"pwd\"}"
        , tuiConfirmationPreview = []
        }
      out = T.unlines (formatTuiConfirmationLines confirmation)
  in if "Tool: shell" `T.isInfixOf` out
        && not ("Preview:" `T.isInfixOf` out)
       then pure $ Right ()
       else pure $ Left $ "empty preview should be omitted: " ++ T.unpack out

testTuiConfirmationInputDecision :: Test
testTuiConfirmationInputDecision =
  let cases =
        [ (TuiConfirmationChar 'y', Just True)
        , (TuiConfirmationChar 'Y', Just True)
        , (TuiConfirmationChar 'n', Just False)
        , (TuiConfirmationChar 'N', Just False)
        , (TuiConfirmationEnter, Just False)
        , (TuiConfirmationEscape, Just False)
        , (TuiConfirmationCtrlC, Just False)
        , (TuiConfirmationChar 'x', Nothing)
        ]
      failures =
        [ (input, expected, tuiConfirmationInputDecision input)
        | (input, expected) <- cases
        , tuiConfirmationInputDecision input /= expected
        ]
  in if null failures
       then pure $ Right ()
       else pure $ Left $ "tuiConfirmationInputDecision mismatch: " ++ show failures

testTruncateEntryTextNoOp :: Test
testTruncateEntryTextNoOp =
  let text = "short text"
  in if truncateEntryText 100 text == text
       then pure $ Right ()
       else pure $ Left "truncateEntryText should not truncate short text"

testTruncateEntryTextExactLimit :: Test
testTruncateEntryTextExactLimit =
  let text = "abcde"
  in if truncateEntryText 5 text == text
       then pure $ Right ()
       else pure $ Left "truncateEntryText should not truncate text at exact limit"

testTruncateEntryTextTruncates :: Test
testTruncateEntryTextTruncates =
  let text = T.replicate 200 "x"
      result = truncateEntryText 10 text
  in if T.isPrefixOf "xxxxxxxxxx" result
       && "... [200 chars]" `T.isInfixOf` result
       then pure $ Right ()
       else pure $ Left $ "truncateEntryText truncation mismatch: " ++ T.unpack result

testTruncateEntryTextEmpty :: Test
testTruncateEntryTextEmpty =
  if truncateEntryText 100 "" == ""
    then pure $ Right ()
    else pure $ Left "truncateEntryText should handle empty text"

testTruncateArgsTextUsesSameLogic :: Test
testTruncateArgsTextUsesSameLogic =
  let text = T.replicate 30 "a"
      result = truncateArgsText 10 text
  in if T.isPrefixOf "aaaaaaaaaa" result
       && "... [30 chars]" `T.isInfixOf` result
       then pure $ Right ()
       else pure $ Left $ "truncateArgsText mismatch: " ++ T.unpack result

testDeleteWordBackwardSimple :: Test
testDeleteWordBackwardSimple =
  if deleteWordBackward "hello world" == "hello"
    then pure $ Right ()
    else pure $ Left $ "deleteWordBackward: got " ++ T.unpack (deleteWordBackward "hello world")

testDeleteWordBackwardTrailingSpaces :: Test
testDeleteWordBackwardTrailingSpaces =
  if deleteWordBackward "hello   " == ""
    then pure $ Right ()
    else pure $ Left $ "deleteWordBackward trailing spaces: got " ++ T.unpack (deleteWordBackward "hello   ")

testDeleteWordBackwardSingleWord :: Test
testDeleteWordBackwardSingleWord =
  if deleteWordBackward "hello" == ""
    then pure $ Right ()
    else pure $ Left $ "deleteWordBackward single word: got " ++ T.unpack (deleteWordBackward "hello")

testDeleteWordBackwardEmpty :: Test
testDeleteWordBackwardEmpty =
  if deleteWordBackward "" == ""
    then pure $ Right ()
    else pure $ Left "deleteWordBackward empty should return empty"

testDeleteWordBackwardMultipleWords :: Test
testDeleteWordBackwardMultipleWords =
  if deleteWordBackward "one two three" == "one two"
    then pure $ Right ()
    else pure $ Left $ "deleteWordBackward multi: got " ++ T.unpack (deleteWordBackward "one two three")

testDeleteToLineStart :: Test
testDeleteToLineStart =
  if deleteToLineStart "hello world" == ""
    then pure $ Right ()
    else pure $ Left "deleteToLineStart should return empty"

testDeleteToLineStartEmpty :: Test
testDeleteToLineStartEmpty =
  if deleteToLineStart "" == ""
    then pure $ Right ()
    else pure $ Left "deleteToLineStart empty should return empty"

testFormatTuiConfirmationArgsTruncated :: Test
testFormatTuiConfirmationArgsTruncated =
  let longArgs = T.replicate 600 "a"
      confirmation = TuiConfirmation
        { tuiConfirmationTool = "shell"
        , tuiConfirmationReason = "no policy rule matched"
        , tuiConfirmationArgs = longArgs
        , tuiConfirmationPreview = []
        }
      out = T.unlines (formatTuiConfirmationLines confirmation)
  in if "... [600 chars]" `T.isInfixOf` out
       && not (T.isInfixOf (T.replicate 600 "a") out)
       then pure $ Right ()
       else pure $ Left $ "confirmation args should be truncated: " ++ show (T.length out)

tests :: [Test]
tests =
  [ testDisplayAssistantEntry
  , testDisplayToolActivity
  , testDisplayToolUnknown
  , testDisplayPolicyRejected
  , testDisplayContextLimit
  , testStreamChunksToEntry
  , testStreamChunksToEntryEmpty
  , testFormatTuiHelpUsesRegistry
  , testFormatTuiStatusLine
  , testFormatTuiConfirmationLines
  , testFormatTuiConfirmationWithoutPreview
  , testTuiConfirmationInputDecision
  , testTruncateEntryTextNoOp
  , testTruncateEntryTextExactLimit
  , testTruncateEntryTextTruncates
  , testTruncateEntryTextEmpty
  , testTruncateArgsTextUsesSameLogic
  , testDeleteWordBackwardSimple
  , testDeleteWordBackwardTrailingSpaces
  , testDeleteWordBackwardSingleWord
  , testDeleteWordBackwardEmpty
  , testDeleteWordBackwardMultipleWords
  , testDeleteToLineStart
  , testDeleteToLineStartEmpty
  , testFormatTuiConfirmationArgsTruncated
  ]
