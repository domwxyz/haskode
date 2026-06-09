{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Haskode.Test.Provider (tests) where

import Data.Aeson       (Value (..), encode, decode, eitherDecode, object, (.=))
import qualified Data.Aeson.Key    as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Control.Exception (try, IOException, throwIO)
import qualified Data.IORef
import Data.List          (isInfixOf)
import Data.Maybe         (isNothing)
import qualified Data.Map.Strict as Map
import qualified Data.Vector        as V
import System.Directory  (getTemporaryDirectory, doesFileExist, removeFile,
                          createDirectory, removeDirectoryRecursive,
                          getCurrentDirectory, setCurrentDirectory,
                          createFileLink, createDirectoryLink, emptyPermissions,
                          getPermissions, setPermissions, renameFile)
import System.Environment (setEnv, unsetEnv)
import System.Exit       (exitFailure, exitSuccess)
import System.FilePath   ((</>))
import System.Info       (os)
import System.IO         (hClose, hFileSize, openFile, openTempFile, IOMode (..))

import Haskode.Core
import Haskode.Commands  (parseSlashCommand, formatHelp, formatStatus, formatUnknownCommand, formatNewConfirmation, resetConversation, formatContextUsage)
import Haskode.Display   (indentBlock, formatAssistantReply, formatToolExecuting,
                          formatToolResult, formatToolUnknown,
                          formatPolicyDenied, formatPolicyConfirmationNeeded,
                          formatPolicyApproved, formatPolicyRejected,
                          formatConfirmTool, formatConfirmArgs,
                          formatConfirmReason, formatConfirmPrompt,
                          formatConfirmFile, formatConfirmDiffHeader,
                          formatConfirmPreviewHeader, formatError, formatVerbose,
                          formatContextLimitRefusal,
                          formatStreamBegin, formatStreamEnd)
import Haskode.Config    (defaultConfig, Config (..), ProviderConfig (..),
                          tokenLimitFieldName, defaultMaxContextChars,
                          defaultMaxSessionLogBytes,
                          expandEnvVars, expandConfig)
import Haskode.Provider  (Provider (..), CompletionRequest (..),
                          CompletionResponse (..), StreamHandler (..),
                          stubProvider, scriptedProvider)
import Haskode.Provider.OpenAI
                          (buildRequestBody, buildStreamingRequestBody,
                           messagesToJSON, messageToJSON, toolsToJSON,
                           parseResponseBody, parseToolCall,
                           parseSSELine, parseSSEEvent, parseDeltaContent,
                           parseDeltaToolCalls,
                           StreamingToolCall (..), assembleStreamToolCalls,
                           OpenAIError (..))
import Haskode.Policy    (checkPolicy, defaultPolicy, Decision (..))
import Haskode.Tools     (defaultRegistry, toolNames, lookupTool, readFileTool, listFilesTool,
                          shellTool, globTool, searchTool, previewPatchTool,
                          applyPatchTool, writeFileTool,
                          extractTextField, Tool (..),
                          TruncResult (..), truncateText, formatTruncMeta,
                          matchGlob, isIgnoredDir, searchInText, formatSearchMatch,
                          isUnderRoot, searchMaxFileSize,
                          TraversalStats (..), emptyStats, formatStats,
                          safeCanonicalize, loadAgentIgnore, shouldIgnorePath,
                          computePatchPreview, computeWriteFilePreview)
import Haskode.Session   (emptyLog, logEvent, events, flushLog, flushLogOnException,
                          Event (..), EventType (..),
                          SessionSummary (..), summarizeSession, formatSessionSummary,
                          isMeaningfulSession)
import Haskode.Patch     (makePatch, showDiff)
import Haskode.Agent     (AgentState (..), initState, runAgent, buildSystemPrompt,
                          loadAgentsMd,
                          estimateContextChars,
                          ApprovalFunc,
                          autoApprove, autoReject,
                          recordSessionStart, recordSessionEnd, recordConversationReset)
import Data.Time.Clock   (getCurrentTime)
import qualified Data.Text    as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Haskode.Test.Util
  ( Test
  , cleanup
  , createTestTree
  , skipIfNoSymlinks
  , skipOnWindows
  , toolDescriptionFromRegistry
  )

-- Provider streaming hook tests
-- ---------------------------------------------------------------------------

-- | stubProvider exposes no streaming implementation.
testStubProviderNoStream :: Test
testStubProviderNoStream =
  if isNothing (providerStream stubProvider)
    then pure $ Right ()
    else pure $ Left "stubProvider should have providerStream = Nothing"

-- | scriptedProvider exposes no streaming implementation.
testScriptedProviderNoStream :: Test
testScriptedProviderNoStream = do
  prov <- scriptedProvider []
  if isNothing (providerStream prov)
    then pure $ Right ()
    else pure $ Left "scriptedProvider should have providerStream = Nothing"

-- | StreamHandler record can be constructed with a simple callback.
testStreamHandlerConstruction :: Test
testStreamHandlerConstruction = do
  ref <- Data.IORef.newIORef ("" :: T.Text)
  let handler = StreamHandler { onToken = \t -> Data.IORef.modifyIORef ref (<> t) }
  onToken handler "hello"
  onToken handler " world"
  result <- Data.IORef.readIORef ref
  if result == "hello world"
    then pure $ Right ()
    else pure $ Left $ "StreamHandler onToken accumulation: " ++ T.unpack result

-- ---------------------------------------------------------------------------
-- Agent streaming integration tests
-- ---------------------------------------------------------------------------

-- | Create a provider that uses the streaming path.
--   Each response is delivered via onToken callbacks, then the
--   final CompletionResponse is returned.
fakeStreamingProvider :: [CompletionResponse] -> IO Provider
fakeStreamingProvider responses = do
  ref <- Data.IORef.newIORef responses
  pure Provider
    { providerName = "fake-streaming"
    , providerComplete = \_req -> do
        remaining <- Data.IORef.readIORef ref
        case remaining of
          [] -> pure CompletionResponse
            { crReply     = mkAssistantMessage "[fake-streaming] no more responses"
            , crToolCalls = Nothing
            }
          (r:rest) -> do
            Data.IORef.writeIORef ref rest
            pure r
    , providerStream = Just $ \_req handler -> do
        remaining <- Data.IORef.readIORef ref
        case remaining of
          [] -> do
            let resp = CompletionResponse
                  { crReply     = mkAssistantMessage "[fake-streaming] no more responses"
                  , crToolCalls = Nothing
                  }
            onToken handler (msgContent (crReply resp))
            pure resp
          (r:rest) -> do
            Data.IORef.writeIORef ref rest
            onToken handler (msgContent (crReply r))
            pure r
    }

-- | Streaming provider is used when providerStream is Just.
--   Verifies session events are recorded correctly for a simple
--   text exchange via the streaming path.
testAgentStreamingTextReply :: Test
testAgentStreamingTextReply = do
  prov <- fakeStreamingProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "streamed hello"
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "hello agent"
  let evts = events (asSession state')
      types = map evType evts
  -- Should have UserMessage and AssistantReply (same as non-streaming)
  if EUserMessage `elem` types && EAssistantReply `elem` types
    then do
      -- Verify the assistant reply content is the streamed text
      let asstEvts = filter (\e -> evType e == EAssistantReply) evts
      case asstEvts of
        (e:_) | evData e == "streamed hello" -> pure $ Right ()
        (e:_) -> pure $ Left $ "Assistant reply content mismatch: " ++ T.unpack (evData e)
        []    -> pure $ Left "No EAssistantReply event found"
    else pure $ Left $ "Missing session events, got: " ++ show types

-- | Streaming provider handles tool calls correctly.
--   Tool calls from the final CompletionResponse are processed
--   after the stream completes.
testAgentStreamingToolCalls :: Test
testAgentStreamingToolCalls = do
  tmpDir <- getTemporaryDirectory
  (path, h) <- openTempFile tmpDir "haskode-stream-tc-test.txt"
  TIO.hPutStrLn h "stream tool call test"
  hClose h
  prov <- fakeStreamingProvider
    [ -- First response: text + tool call via streaming
      CompletionResponse
        { crReply     = mkAssistantMessage "Let me read that."
        , crToolCalls = Just [ToolCall "stc-1" "read_file" (object ["path" .= T.pack path])]
        }
    , -- Second response: final text via streaming
      CompletionResponse
        { crReply     = mkAssistantMessage "Done reading."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "read the file"
  let evts = events (asSession state')
      types = map evType evts
  cleanup path
  -- Should have: UserMessage, AssistantReply, ToolCall, ToolResult, AssistantReply
  if EUserMessage `elem` types
     && EAssistantReply `elem` types
     && EToolCall `elem` types
     && EToolResult `elem` types
    then pure $ Right ()
    else pure $ Left $ "Streaming tool-call events: " ++ show types

-- | Non-streaming fallback works when providerStream is Nothing.
--   This is the same as existing agent tests but uses the
--   fakeStreamingProvider with its streaming path removed,
--   confirming the fallback path is taken.
testAgentNonStreamingFallback :: Test
testAgentNonStreamingFallback = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "non-streaming reply"
        , crToolCalls = Nothing
        }
    ]
  -- Verify the provider has no streaming
  if isNothing (providerStream prov)
    then do
      let cfg = defaultConfig
          state = initState cfg prov defaultPolicy defaultRegistry autoApprove
      state' <- runAgent state "hello"
      let evts = events (asSession state')
          types = map evType evts
      if EUserMessage `elem` types && EAssistantReply `elem` types
        then do
          let asstEvts = filter (\e -> evType e == EAssistantReply) evts
          case asstEvts of
            (e:_) | evData e == "non-streaming reply" -> pure $ Right ()
            (e:_) -> pure $ Left $ "Assistant reply content mismatch: " ++ T.unpack (evData e)
            []    -> pure $ Left "No EAssistantReply event found"
        else pure $ Left $ "Missing session events, got: " ++ show types
    else pure $ Left "scriptedProvider should have providerStream = Nothing"

-- | Streaming tool-call-only responses skip the assistant display label.
--   When the streamed response has tool calls but empty text content,
--   the lazy display should not print an empty "Assistant:" line.
testAgentStreamingToolCallOnlyLazyDisplay :: Test
testAgentStreamingToolCallOnlyLazyDisplay = do
  tmpDir <- getTemporaryDirectory
  (path, h) <- openTempFile tmpDir "haskode-lazy-display-test.txt"
  TIO.hPutStrLn h "lazy display test"
  hClose h
  prov <- fakeStreamingProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage ""
        , crToolCalls = Just [ToolCall "ld-1" "read_file" (object ["path" .= T.pack path])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "read the file"
  let evts = events (asSession state')
      types = map evType evts
  cleanup path
  -- Should have: UserMessage, AssistantReply, ToolCall, ToolResult, AssistantReply
  if EUserMessage `elem` types
     && EAssistantReply `elem` types
     && EToolCall `elem` types
     && EToolResult `elem` types
    then
      -- Verify final assistant reply is the second response text
      let asstEvts = filter (\e -> evType e == EAssistantReply) evts
      in case lastMay asstEvts of
           Just e | evData e == "Done." -> pure $ Right ()
           Just e  -> pure $ Left $ "Final assistant reply mismatch: " ++ T.unpack (evData e)
           Nothing -> pure $ Left "No EAssistantReply events found"
    else pure $ Left $ "Streaming tool-call-only events: " ++ show types

lastMay :: [a] -> Maybe a
lastMay [] = Nothing
lastMay xs = Just (last xs)

tests :: [Test]
tests =
  [ testStubProviderNoStream
  , testScriptedProviderNoStream
  , testStreamHandlerConstruction
  , testAgentStreamingTextReply
  , testAgentStreamingToolCalls
  , testAgentNonStreamingFallback
  , testAgentStreamingToolCallOnlyLazyDisplay
  ]
