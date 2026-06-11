{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Minimal Brick TUI wrapper for Haskode.
--
-- This is intentionally small: one transcript, one input line, and one
-- status/tool area.  The CLI remains the reference path; this module consumes
-- the same final command registry, command actions, and agent display events
-- without parsing terminal rendering.
module Haskode.Tui
  ( runTui
    -- * Pure TUI conversion helpers
  , TuiEntry (..)
  , TuiEntryKind (..)
  , TuiUpdate (..)
  , TuiConfirmation (..)
  , TuiConfirmationInput (..)
  , displayEventToTuiUpdate
  , streamChunksToEntry
  , formatTuiHelp
  , formatTuiStatusLine
  , formatTuiConfirmationLines
  , formatTuiCompactionConfirmationLines
  , tuiConfirmationInputDecision
  , truncateEntryText
  , truncateArgsText
  , deleteWordBackward
  , deleteToLineStart
  ) where

import Data.Aeson             (Value, encode)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding as TE
import Brick
  ( App (..)
  , BrickEvent (..)
  , EventM
  , ViewportType (..)
  , Widget
  , (<=>)
  , attrMap
  , defaultMain
  , halt
  , neverShowCursor
  , padAll
  , str
  , txtWrap
  , vBox
  , vLimit
  , viewport
  , viewportScroll
  , vScrollBy
  )
import qualified Brick.Widgets.Border as B
import Control.Exception        (SomeException, displayException, try)
import Control.Monad           (when)
import Control.Monad.IO.Class  (liftIO)
import Control.Monad.State.Strict (get, put)
import Data.IORef              (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import Data.Text               (Text)
import qualified Data.Text as T
import qualified Graphics.Vty as V

import Haskode.Agent
  ( AgentDisplay (..)
  , AgentState (..)
  , runAgent
  , proposeCompaction
  , applyCompactionDecision
  , recordConversationReset
  )
import Haskode.Commands
  ( CommandAction (..)
  , CommandRegistry
  , CommandSpec (cmdAvailableInTui)
  , formatDoctor
  , formatHelpFor
  , formatNewConfirmation
  , formatStatus
  , formatUnknownCommand
  , parseSlashCommand
  , resolveCommandActionFor
  , resetConversation
  )
import Haskode.Config     (Config (..), ProviderConfig (..))
import Haskode.Core       (ToolCall (..))
import Haskode.Display
  ( DisplayEvent (..)
  , formatContextLimitRefusal
  , formatCompactionAccepted
  , formatCompactionRejected
  )
import Haskode.Provider   (Provider (..))
import Haskode.Session    (flushLogOnException)
import Haskode.Tools
  ( computeBatchApplyPreview
  , computePatchPreview
  , computeWriteFilePreview
  )

-- ---------------------------------------------------------------------------
-- Pure view data
-- ---------------------------------------------------------------------------

data TuiEntryKind
  = TuiUserEntry
  | TuiAssistantEntry
  | TuiSystemEntry
  | TuiToolEntry
  deriving stock (Eq, Show)

data TuiEntry = TuiEntry
  { tuiEntryKind :: !TuiEntryKind
  , tuiEntryText :: !Text
  } deriving stock (Eq, Show)

data TuiUpdate = TuiUpdate
  { tuiUpdateEntries      :: ![TuiEntry]
  , tuiUpdateStatus       :: !(Maybe Text)
  , tuiUpdateToolActivity :: !(Maybe Text)
  } deriving stock (Eq, Show)

data TuiConfirmation = TuiConfirmation
  { tuiConfirmationTool    :: !Text
  , tuiConfirmationReason  :: !Text
  , tuiConfirmationArgs    :: !Text
  , tuiConfirmationPreview :: ![Text]
  } deriving stock (Eq, Show)

data TuiConfirmationInput
  = TuiConfirmationChar !Char
  | TuiConfirmationEnter
  | TuiConfirmationEscape
  | TuiConfirmationCtrlC
  deriving stock (Eq, Show)

emptyTuiUpdate :: TuiUpdate
emptyTuiUpdate = TuiUpdate [] Nothing Nothing

displayEventToTuiUpdate :: DisplayEvent -> TuiUpdate
displayEventToTuiUpdate event =
  case event of
    DisplayAssistant content ->
      (entry TuiAssistantEntry content)
        { tuiUpdateStatus = Just "Assistant replied." }
    DisplayToolExecuting name ->
      (entry TuiToolEntry ("Executing: " <> name))
        { tuiUpdateStatus = Just ("Tool running: " <> name)
        , tuiUpdateToolActivity = Just ("Running: " <> name)
        }
    DisplayToolResult output ->
      (entry TuiToolEntry ("Result: " <> output))
        { tuiUpdateStatus = Just "Tool finished."
        , tuiUpdateToolActivity = Just ("Last result: " <> oneLine output)
        }
    DisplayToolUnknown name ->
      (entry TuiSystemEntry ("Unknown or disabled tool: " <> name))
        { tuiUpdateStatus = Just ("Unknown or disabled tool: " <> name)
        , tuiUpdateToolActivity = Just ("Unknown or disabled: " <> name)
        }
    DisplayPolicyDenied name reason ->
      (entry TuiSystemEntry ("Policy denied: " <> name <> " -- " <> reason))
        { tuiUpdateStatus = Just "Policy denied." }
    DisplayPolicyConfirmationNeeded name ->
      (entry TuiSystemEntry ("Confirmation needed: " <> name))
        { tuiUpdateStatus = Just "Confirmation needed."
        , tuiUpdateToolActivity = Just ("Awaiting confirmation: " <> name)
        }
    DisplayPolicyApproved ->
      (entry TuiSystemEntry "Policy approved by user.")
        { tuiUpdateStatus = Just "Policy approved."
        , tuiUpdateToolActivity = Just "Approved."
        }
    DisplayPolicyRejected ->
      (entry TuiSystemEntry "Policy rejected by user.")
        { tuiUpdateStatus = Just "Policy rejected."
        , tuiUpdateToolActivity = Just "Rejected."
        }
    DisplayError msg ->
      (entry TuiSystemEntry msg)
        { tuiUpdateStatus = Just ("Error: " <> oneLine msg) }
    DisplayContextLimitRefusal estimated maxChars ->
      (entry TuiAssistantEntry (formatContextLimitRefusal estimated maxChars))
        { tuiUpdateStatus = Just "Context limit exceeded." }
  where
    entry kind text = emptyTuiUpdate { tuiUpdateEntries = [TuiEntry kind text] }

streamChunksToEntry :: [Text] -> Maybe TuiEntry
streamChunksToEntry chunks =
  let content = T.concat chunks
  in if T.null content
       then Nothing
       else Just (TuiEntry TuiAssistantEntry content)

formatTuiHelp :: CommandRegistry -> Text
formatTuiHelp commands = formatHelpFor commands cmdAvailableInTui

formatTuiStatusLine :: AgentState -> Text
formatTuiStatusLine st =
  let pc = cfgProvider (asConfig st)
      streaming = case providerStream (asProvider st) of
        Just _  -> "yes"
        Nothing -> "no"
      resumed = if asResumed st then "yes" else "no"
  in "Provider: " <> T.pack (pcProvider pc)
     <> " | Model: " <> T.pack (pcModel pc)
     <> " | Streaming: " <> streaming
     <> " | Resumed: " <> resumed

oneLine :: Text -> Text
oneLine = T.take 160 . T.intercalate " " . T.lines

formatTuiConfirmationLines :: TuiConfirmation -> [Text]
formatTuiConfirmationLines confirmation =
  [ "Tool: " <> tuiConfirmationTool confirmation
  , "Reason: " <> tuiConfirmationReason confirmation
  , "Args: " <> truncateArgsText confirmationArgsLimit (tuiConfirmationArgs confirmation)
  ]
  <> previewLines
  <> [ ""
     , "Approve? y = approve, n/Esc/Enter/Ctrl-C = reject"
     ]
  where
    preview = tuiConfirmationPreview confirmation
    previewLines
      | null preview = []
      | otherwise    = "" : "Preview:" : map ("  " <>) preview

formatTuiCompactionConfirmationLines :: Text -> [Text]
formatTuiCompactionConfirmationLines draft =
  [ "Proposed compact memory:"
  , ""
  ]
  <> T.lines (truncateEntryText transcriptEntryLimit draft)
  <> [ ""
     , "Accept? y = accept, n/Esc/Enter/Ctrl-C = reject"
     ]

tuiConfirmationInputDecision :: TuiConfirmationInput -> Maybe Bool
tuiConfirmationInputDecision input =
  case input of
    TuiConfirmationChar 'y' -> Just True
    TuiConfirmationChar 'Y' -> Just True
    TuiConfirmationChar 'n' -> Just False
    TuiConfirmationChar 'N' -> Just False
    TuiConfirmationEnter    -> Just False
    TuiConfirmationEscape   -> Just False
    TuiConfirmationCtrlC    -> Just False
    TuiConfirmationChar _   -> Nothing

-- ---------------------------------------------------------------------------
-- Brick state and captured agent output
-- ---------------------------------------------------------------------------

data TuiOutput
  = TuiOutputEvent !DisplayEvent
  | TuiOutputStreamBegin
  | TuiOutputStreamChunk !Text
  | TuiOutputStreamEnd

data TuiName = TranscriptViewport
  | ConfirmationViewport
  deriving stock (Eq, Ord, Show)

data TuiConfirmationState = TuiConfirmationState
  { tcsConfirmation :: !TuiConfirmation
  , tcsDecision     :: !(Maybe Bool)
  }

data TuiCompactionConfirmationState = TuiCompactionConfirmationState
  { tccsDraft    :: !Text
  , tccsDecision :: !(Maybe Bool)
  }

data TuiState = TuiState
  { tuiAgentState   :: !AgentState
  , tuiCommandRegistry :: !CommandRegistry
  , tuiOutputRef    :: !(IORef [TuiOutput])
  , tuiTranscript   :: ![TuiEntry]
  , tuiInput        :: !Text
  , tuiStatus       :: !Text
  , tuiToolActivity :: !Text
  , tuiStreamBuffer :: !Text
  }

transcriptLimit :: Int
transcriptLimit = 300

initialTuiState :: IORef [TuiOutput] -> CommandRegistry -> Maybe Text -> AgentState -> TuiState
initialTuiState ref commands initialInput state =
  TuiState
    { tuiAgentState   = state
    , tuiCommandRegistry = commands
    , tuiOutputRef    = ref
    , tuiTranscript   =
        [ TuiEntry TuiSystemEntry "Haskode TUI v0. Type /help for commands." ]
    , tuiInput        = maybe "" id initialInput
    , tuiStatus       = "Ready."
    , tuiToolActivity = "No active tool."
    , tuiStreamBuffer = ""
    }

-- ---------------------------------------------------------------------------
-- Public runner
-- ---------------------------------------------------------------------------

runTui :: CommandRegistry -> Maybe Text -> AgentState -> IO AgentState
runTui commands initialInput state = do
  outputRef <- newIORef []
  previewRef <- newIORef Nothing
  let tuiState = state
        { asDisplay = tuiAgentDisplay outputRef previewRef
        , asApproval = tuiApproval previewRef
        }
  final <- defaultMain tuiApp (initialTuiState outputRef commands initialInput tuiState)
  pure (tuiAgentState final)

tuiAgentDisplay :: IORef [TuiOutput] -> IORef (Maybe [Text]) -> AgentDisplay
tuiAgentDisplay ref previewRef = AgentDisplay
  { agentDisplayEvent       = pushTuiOutput ref . TuiOutputEvent
  , agentDisplayStreamBegin = pushTuiOutput ref TuiOutputStreamBegin
  , agentDisplayStreamChunk = pushTuiOutput ref . TuiOutputStreamChunk
  , agentDisplayStreamEnd   = pushTuiOutput ref TuiOutputStreamEnd
  , agentDisplayPreview     = captureTuiPreview previewRef
  }

tuiApproval :: IORef (Maybe [Text]) -> ToolCall -> Text -> IO Bool
tuiApproval previewRef tc reason = do
  captured <- readIORef previewRef
  writeIORef previewRef Nothing
  preview <- case captured of
    Just lines' -> pure lines'
    Nothing     -> tuiPreviewForToolCall tc
  runTuiConfirmationDialog $
    TuiConfirmation
      { tuiConfirmationTool    = tcName tc
      , tuiConfirmationReason  = reason
      , tuiConfirmationArgs    = encodeValueText (tcArgs tc)
      , tuiConfirmationPreview = preview
      }

captureTuiPreview :: IORef (Maybe [Text]) -> ToolCall -> IO ()
captureTuiPreview ref tc = do
  preview <- tuiPreviewForToolCall tc
  writeIORef ref (Just preview)

tuiPreviewForToolCall :: ToolCall -> IO [Text]
tuiPreviewForToolCall tc =
  case tcName tc of
    "apply_patch" ->
      computePatchPreview (tcArgs tc) >>= \result ->
        pure $ case result of
          Left _err -> []
          Right (path, diff) ->
            ["File: " <> T.pack path, "Diff:"]
            <> T.lines diff
    "write_file" ->
      computeWriteFilePreview (tcArgs tc) >>= \result ->
        pure $ case result of
          Left _err -> []
          Right (path, preview) ->
            ["File: " <> T.pack path, "Preview:"]
            <> T.lines preview
    "apply_patch_batch" ->
      computeBatchApplyPreview (tcArgs tc) >>= \result ->
        pure $ case result of
          Left _err -> []
          Right (summary, parts) ->
            T.lines summary <> concatMap T.lines parts
    _ ->
      pure []

encodeValueText :: Value -> Text
encodeValueText = TE.decodeUtf8 . LBS.toStrict . encode

pushTuiOutput :: IORef [TuiOutput] -> TuiOutput -> IO ()
pushTuiOutput ref output =
  modifyIORef' ref (<> [output])

clearTuiOutput :: IORef [TuiOutput] -> IO ()
clearTuiOutput ref =
  writeIORef ref []

drainTuiOutput :: IORef [TuiOutput] -> IO [TuiOutput]
drainTuiOutput ref = do
  outputs <- readIORef ref
  writeIORef ref []
  pure outputs

-- ---------------------------------------------------------------------------
-- Brick app
-- ---------------------------------------------------------------------------

tuiApp :: App TuiState e TuiName
tuiApp = App
  { appDraw = drawTui
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleTuiEvent
  , appStartEvent = pure ()
  , appAttrMap = const (attrMap V.defAttr [])
  }

tuiConfirmationApp :: App TuiConfirmationState e TuiName
tuiConfirmationApp = App
  { appDraw = drawTuiConfirmation
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleTuiConfirmationEvent
  , appStartEvent = pure ()
  , appAttrMap = const (attrMap V.defAttr [])
  }

tuiCompactionConfirmationApp :: App TuiCompactionConfirmationState e TuiName
tuiCompactionConfirmationApp = App
  { appDraw = drawTuiCompactionConfirmation
  , appChooseCursor = neverShowCursor
  , appHandleEvent = handleTuiCompactionConfirmationEvent
  , appStartEvent = pure ()
  , appAttrMap = const (attrMap V.defAttr [])
  }

runTuiConfirmationDialog :: TuiConfirmation -> IO Bool
runTuiConfirmationDialog confirmation = do
  final <- defaultMain tuiConfirmationApp $
    TuiConfirmationState
      { tcsConfirmation = confirmation
      , tcsDecision = Nothing
      }
  pure (maybe False id (tcsDecision final))

runTuiCompactionConfirmationDialog :: Text -> IO Bool
runTuiCompactionConfirmationDialog draft = do
  final <- defaultMain tuiCompactionConfirmationApp $
    TuiCompactionConfirmationState
      { tccsDraft = draft
      , tccsDecision = Nothing
      }
  pure (maybe False id (tccsDecision final))

drawTui :: TuiState -> [Widget TuiName]
drawTui st =
  [ vBox
      [ str "Haskode TUI v0"
      , B.borderWithLabel (str "Transcript") $
          viewport TranscriptViewport Vertical $
            vBox (renderTranscript (tuiTranscript st))
      , vLimit 4 $
          B.borderWithLabel (str "Status") $
            padAll 1 $
              txtWrap (formatTuiStatusLine (tuiAgentState st))
              <=> txtWrap ("Status: " <> tuiStatus st)
              <=> txtWrap ("Tool: " <> tuiToolActivity st)
      , vLimit 3 $
          B.borderWithLabel (str "Input") $
            padAll 1 $
              txtWrap ("> " <> tuiInput st)
      ]
  ]

drawTuiConfirmation :: TuiConfirmationState -> [Widget TuiName]
drawTuiConfirmation st =
  [ B.borderWithLabel (str "Confirm Tool Action") $
      padAll 1 $
        viewport ConfirmationViewport Vertical $
          vBox (map txtWrap (formatTuiConfirmationLines (tcsConfirmation st)))
  ]

drawTuiCompactionConfirmation :: TuiCompactionConfirmationState -> [Widget TuiName]
drawTuiCompactionConfirmation st =
  [ B.borderWithLabel (str "Confirm Compaction") $
      padAll 1 $
        viewport ConfirmationViewport Vertical $
          vBox (map txtWrap (formatTuiCompactionConfirmationLines (tccsDraft st)))
  ]

handleTuiEvent :: BrickEvent TuiName e -> EventM TuiName TuiState ()
handleTuiEvent event =
  case event of
    VtyEvent (V.EvKey V.KEsc []) ->
      halt
    VtyEvent (V.EvKey (V.KChar 'c') [V.MCtrl]) ->
      halt
    VtyEvent (V.EvKey V.KEnter []) -> do
      st <- get
      (st', shouldExit) <- liftIO (submitInput st)
      put st'
      when shouldExit halt
    VtyEvent (V.EvKey V.KBS []) ->
      updateInput dropLastChar
    VtyEvent (V.EvKey V.KPageUp []) ->
      vScrollBy (viewportScroll TranscriptViewport) (-10)
    VtyEvent (V.EvKey V.KPageDown []) ->
      vScrollBy (viewportScroll TranscriptViewport) 10
    VtyEvent (V.EvKey (V.KChar 'w') [V.MCtrl]) ->
      updateInput deleteWordBackward
    VtyEvent (V.EvKey (V.KChar 'u') [V.MCtrl]) ->
      updateInput deleteToLineStart
    VtyEvent (V.EvKey (V.KChar c) []) ->
      updateInput (<> T.singleton c)
    _ ->
      pure ()

handleTuiConfirmationEvent :: BrickEvent TuiName e -> EventM TuiName TuiConfirmationState ()
handleTuiConfirmationEvent event =
  case event of
    VtyEvent vtyEvent ->
      case vtyEventToConfirmationInput vtyEvent >>= tuiConfirmationInputDecision of
        Nothing -> pure ()
        Just decision -> do
          st <- get
          put st { tcsDecision = Just decision }
          halt
    _ ->
      pure ()

handleTuiCompactionConfirmationEvent :: BrickEvent TuiName e -> EventM TuiName TuiCompactionConfirmationState ()
handleTuiCompactionConfirmationEvent event =
  case event of
    VtyEvent vtyEvent ->
      case vtyEventToConfirmationInput vtyEvent >>= tuiConfirmationInputDecision of
        Nothing -> pure ()
        Just decision -> do
          st <- get
          put st { tccsDecision = Just decision }
          halt
    _ ->
      pure ()

vtyEventToConfirmationInput :: V.Event -> Maybe TuiConfirmationInput
vtyEventToConfirmationInput event =
  case event of
    V.EvKey (V.KChar c) [] ->
      Just (TuiConfirmationChar c)
    V.EvKey (V.KChar 'c') [V.MCtrl] ->
      Just TuiConfirmationCtrlC
    V.EvKey V.KEnter [] ->
      Just TuiConfirmationEnter
    V.EvKey V.KEsc [] ->
      Just TuiConfirmationEscape
    _ ->
      Nothing

updateInput :: (Text -> Text) -> EventM TuiName TuiState ()
updateInput f = do
  st <- get
  put st { tuiInput = f (tuiInput st) }

dropLastChar :: Text -> Text
dropLastChar t =
  case T.unsnoc t of
    Nothing      -> ""
    Just (xs, _) -> xs

-- | Truncate transcript entry text for display.
--
--   When the text exceeds the limit, keeps the first portion and
--   appends a clear marker showing the original length.  This prevents
--   very large tool results or assistant replies from dominating the
--   transcript view.
--
--   >>> truncateEntryText 100 "short text"
--   "short text"
--   >>> truncateEntryText 10 (T.replicate 20 "x")
--   "xxxxxxxxxx... [20 chars]"
truncateEntryText :: Int -> Text -> Text
truncateEntryText limit t
  | T.length t <= limit = t
  | otherwise = T.take limit t <> "... [" <> T.pack (show (T.length t)) <> " chars]"

-- | Truncate confirmation argument text for display.
--
--   Similar to 'truncateEntryText' but uses a shorter default limit
--   appropriate for the confirmation panel's args line.
--
--   >>> truncateArgsText 200 shortArgs
--   shortArgs
--   >>> truncateArgsText 200 longArgs
--   "<first 200 chars>... [N chars]"
truncateArgsText :: Int -> Text -> Text
truncateArgsText = truncateEntryText

-- | Delete the last word from text.
--
--   Strips trailing whitespace, then removes the last non-whitespace
--   word and any preceding whitespace.  Used by Ctrl-W to delete the
--   previous word in the input line.
--
--   >>> deleteWordBackward "hello world"
--   "hello"
--   >>> deleteWordBackward "hello   "
--   "hello"
--   >>> deleteWordBackward ""
--   ""
deleteWordBackward :: Text -> Text
deleteWordBackward t =
  let stripped = T.stripEnd t
  in case T.breakOnEnd " " stripped of
       ("", _)    -> ""
       (_, "")    -> ""
       (pre, _)   -> T.stripEnd pre

-- | Clear the entire input line.
--
--   Used by Ctrl-U to quickly clear the input.  Always returns
--   empty text regardless of input.
--
--   >>> deleteToLineStart "hello world"
--   ""
deleteToLineStart :: Text -> Text
deleteToLineStart _ = ""

-- | Transcript entry limit for assistant and system text.
--   Tool results use a shorter limit since they tend to be verbose.
transcriptEntryLimit :: Int
transcriptEntryLimit = 2000

-- | Transcript entry limit for tool results.
transcriptToolEntryLimit :: Int
transcriptToolEntryLimit = 500

-- | Confirmation args display limit.
confirmationArgsLimit :: Int
confirmationArgsLimit = 500

-- | Render a transcript entry with appropriate truncation.
renderEntry :: TuiEntry -> Widget TuiName
renderEntry entry =
  let limit = case tuiEntryKind entry of
        TuiToolEntry -> transcriptToolEntryLimit
        _            -> transcriptEntryLimit
      text = truncateEntryText limit (tuiEntryText entry)
  in txtWrap (prefix <> text)
  where
    prefix = case tuiEntryKind entry of
      TuiUserEntry      -> "You: "
      TuiAssistantEntry -> "Assistant: "
      TuiSystemEntry    -> "[system] "
      TuiToolEntry      -> "[tool] "

-- | Render a list of transcript entries with a blank separator before
--   every entry after the first.
--
--   This improves readability by giving every transcript item a small
--   visual break, regardless of entry kind.
renderTranscript :: [TuiEntry] -> [Widget TuiName]
renderTranscript [] = []
renderTranscript (first:rest) =
  renderEntry first : concatMap renderSeparatorAndEntry rest
  where
    renderSeparatorAndEntry entry =
      [str " ", renderEntry entry]

-- ---------------------------------------------------------------------------
-- Input and command handling
-- ---------------------------------------------------------------------------

submitInput :: TuiState -> IO (TuiState, Bool)
submitInput st =
  let input = tuiInput st
      trimmed = T.strip input
  in if T.null trimmed
       then pure (st { tuiInput = "" }, False)
       else case parseSlashCommand input of
         Just cmd -> submitCommand cmd st { tuiInput = "" }
         Nothing  -> submitPrompt input st { tuiInput = "" }

submitCommand :: Text -> TuiState -> IO (TuiState, Bool)
submitCommand cmd st =
  case resolveCommandActionFor (tuiCommandRegistry st) cmdAvailableInTui cmd of
    Right action ->
      runTuiCommand action st
    Left unknown ->
      pure
        ( (appendEntry (TuiEntry TuiSystemEntry (formatUnknownCommand unknown)) st)
            { tuiStatus = "Unknown command." }
        , False
        )

runTuiCommand :: CommandAction -> TuiState -> IO (TuiState, Bool)
runTuiCommand action st =
  case action of
    CmdHelp ->
      pure
        ( (appendEntry (TuiEntry TuiSystemEntry (formatTuiHelp (tuiCommandRegistry st))) st)
            { tuiStatus = "Help displayed." }
        , False
        )
    CmdStatus ->
      pure
        ( (appendEntry (TuiEntry TuiSystemEntry (formatStatus (tuiAgentState st))) st)
            { tuiStatus = "Status displayed." }
        , False
        )
    CmdDoctor -> do
      output <- formatDoctor (tuiAgentState st)
      pure
        ( (appendEntry (TuiEntry TuiSystemEntry output) st)
            { tuiStatus = "Doctor checks displayed." }
        , False
        )
    CmdNew -> do
      let resetState = resetConversation (tuiAgentState st)
      recorded <- recordConversationReset resetState
      pure
        ( (appendEntry (TuiEntry TuiSystemEntry formatNewConfirmation) st)
            { tuiAgentState = recorded
            , tuiStatus = formatNewConfirmation
            , tuiToolActivity = "No active tool."
            , tuiStreamBuffer = ""
            }
        , False
        )
    CmdCompact -> do
      result <- proposeCompaction (tuiAgentState st)
      case result of
        Left err ->
          pure
            ( (appendEntry (TuiEntry TuiSystemEntry err) st)
                { tuiStatus = "Compaction skipped." }
            , False
            )
        Right draft -> do
          approved <- runTuiCompactionConfirmationDialog draft
          if approved
            then do
              compacted <- applyCompactionDecision True draft (tuiAgentState st)
              pure
                ( (appendEntry (TuiEntry TuiSystemEntry formatCompactionAccepted) st)
                    { tuiAgentState = compacted
                    , tuiStatus = formatCompactionAccepted
                    , tuiToolActivity = "No active tool."
                    , tuiStreamBuffer = ""
                    }
                , False
                )
            else
              pure
                ( (appendEntry (TuiEntry TuiSystemEntry formatCompactionRejected) st)
                    { tuiStatus = formatCompactionRejected }
                , False
                )
    CmdExit ->
      pure
        ( (appendEntry (TuiEntry TuiSystemEntry "Goodbye.") st)
            { tuiStatus = "Exiting." }
        , True
        )
    CmdExtensionText output ->
      pure
        ( (appendEntry (TuiEntry TuiSystemEntry output) st)
            { tuiStatus = "Command displayed." }
        , False
        )

submitPrompt :: Text -> TuiState -> IO (TuiState, Bool)
submitPrompt input st = do
  let ref = tuiOutputRef st
      agentState = tuiAgentState st
      dir = cfgWorkingDir (asConfig agentState)
      maxBytes = cfgMaxSessionLogBytes (asConfig agentState)
      startState =
        (appendEntry (TuiEntry TuiUserEntry input) st)
          { tuiStatus = "Running agent turn..."
          , tuiToolActivity = "No active tool."
          , tuiStreamBuffer = ""
          }
  clearTuiOutput ref
  result <-
    try
      ( flushLogOnException dir maxBytes (asSession agentState) $
          runAgent agentState input
      ) :: IO (Either SomeException AgentState)
  outputs <- drainTuiOutput ref
  let rendered = applyTuiOutputs startState outputs
  case result of
    Right agentState' ->
      pure
        ( rendered
            { tuiAgentState = agentState'
            , tuiStatus = "Ready."
            }
        , False
        )
    Left err ->
      pure
        ( (appendEntry
            (TuiEntry TuiSystemEntry ("Error: " <> T.pack (displayException err)))
            rendered)
            { tuiStatus = "Agent turn failed." }
        , False
        )

applyTuiOutputs :: TuiState -> [TuiOutput] -> TuiState
applyTuiOutputs = foldl applyTuiOutput

applyTuiOutput :: TuiState -> TuiOutput -> TuiState
applyTuiOutput st output =
  case output of
    TuiOutputEvent event ->
      applyTuiUpdate (displayEventToTuiUpdate event) st
    TuiOutputStreamBegin ->
      st
        { tuiStreamBuffer = ""
        , tuiStatus = "Streaming assistant reply..."
        }
    TuiOutputStreamChunk chunk ->
      st { tuiStreamBuffer = tuiStreamBuffer st <> chunk }
    TuiOutputStreamEnd ->
      case streamChunksToEntry [tuiStreamBuffer st] of
        Nothing ->
          st { tuiStreamBuffer = "", tuiStatus = "Ready." }
        Just entry ->
          (appendEntry entry st)
            { tuiStreamBuffer = ""
            , tuiStatus = "Assistant replied."
            }

applyTuiUpdate :: TuiUpdate -> TuiState -> TuiState
applyTuiUpdate update st =
  (appendEntries (tuiUpdateEntries update) st)
    { tuiStatus = maybe (tuiStatus st) id (tuiUpdateStatus update)
    , tuiToolActivity = maybe (tuiToolActivity st) id (tuiUpdateToolActivity update)
    }

appendEntry :: TuiEntry -> TuiState -> TuiState
appendEntry entry =
  appendEntries [entry]

appendEntries :: [TuiEntry] -> TuiState -> TuiState
appendEntries newEntries st =
  st { tuiTranscript = takeLast transcriptLimit (tuiTranscript st <> newEntries) }

takeLast :: Int -> [a] -> [a]
takeLast n xs =
  drop (max 0 (length xs - n)) xs
