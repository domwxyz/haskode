{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Haskode.Test.Policy (tests) where

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

-- Approval tests
-- ---------------------------------------------------------------------------

-- | An AskUser tool call executes when the approval function approves.
--   We use 'write_file' (which falls through to AskUser in defaultPolicy)
--   and inject autoApprove.  Since write_file is not in the registry,
--   the tool execution path returns "unknown tool" — but the important
--   thing is that it reaches the execution path (not denial).
testApprovalApproved :: Test
testApprovalApproved = do
  tmpDir <- getTemporaryDirectory
  (path, h) <- openTempFile tmpDir "haskode-approval-test.txt"
  TIO.hPutStrLn h "approval test content"
  hClose h
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Let me write that file."
        , crToolCalls = Just [ToolCall "tc-w1" "write_file"
                               (object ["path" .= T.pack path, "content" .= ("new" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "write the file"
  let evts = events (asSession state')
      types = map evType evts
  -- write_file is not in the registry, so we get EToolResult with "unknown tool"
  -- but the key thing: it was NOT denied — it reached the execution path.
  if EPolicyDecision `elem` types && EToolResult `elem` types
    then
      -- Check that no "denied" event appears
      let deniedEvts = filter (\e -> evType e == EToolResult && T.isInfixOf "denied" (evData e)) evts
      in if null deniedEvts
           then pure $ Right ()
           else pure $ Left $ "Expected no denial events, got: " ++ show deniedEvts
    else pure $ Left $ "Expected EPolicyDecision and EToolResult, got: " ++ show types

-- | An AskUser tool call is rejected when the approval function rejects.
testApprovalRejected :: Test
testApprovalRejected = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Let me write that file."
        , crToolCalls = Just [ToolCall "tc-w2" "write_file"
                               (object ["path" .= ("test.txt" :: T.Text), "content" .= ("new" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "OK, I won't."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoReject
  state' <- runAgent state "write the file"
  let evts = events (asSession state')
      types = map evType evts
  -- Should have EPolicyDecision and EToolResult with "denied by user"
  let toolResults = filter (\e -> evType e == EToolResult) evts
      deniedResults = filter (T.isInfixOf "denied by user" . evData) toolResults
  if EPolicyDecision `elem` types && not (null deniedResults)
    then pure $ Right ()
    else pure $ Left $ "Expected denial by user, got events: " ++ show types

-- | Session events include approval info for approved calls.
testApprovalSessionEvents :: Test
testApprovalSessionEvents = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Checking."
        , crToolCalls = Just [ToolCall "tc-w3" "write_file"
                               (object ["path" .= ("x.txt" :: T.Text), "content" .= ("y" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "check"
  let evts = events (asSession state')
      policyEvts = filter (\e -> evType e == EPolicyDecision) evts
      approvedEvts = filter (T.isInfixOf "approved" . evData) policyEvts
  if not (null approvedEvts)
    then pure $ Right ()
    else pure $ Left $ "Expected 'approved' in policy events, got: " ++ show policyEvts

-- | Session events include rejection info for rejected calls.
testRejectionSessionEvents :: Test
testRejectionSessionEvents = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Checking."
        , crToolCalls = Just [ToolCall "tc-w4" "write_file"
                               (object ["path" .= ("x.txt" :: T.Text), "content" .= ("y" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "OK."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoReject
  state' <- runAgent state "check"
  let evts = events (asSession state')
      toolResultEvts = filter (\e -> evType e == EToolResult) evts
      deniedByUser = filter (T.isInfixOf "denied by user" . evData) toolResultEvts
  if not (null deniedByUser)
    then pure $ Right ()
    else pure $ Left $ "Expected 'denied by user' in tool result events, got: " ++ show toolResultEvts

-- | Dangerous commands are still hard-denied by policy without prompting.
--   Even with autoApprove, a dangerous shell command should be blocked
--   by the Deny rule — the approval function is never consulted.
testDangerousDeniedWithoutPrompting :: Test
testDangerousDeniedWithoutPrompting = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Let me run that."
        , crToolCalls = Just [ToolCall "tc-d1" "shell"
                               (object ["command" .= ("rm -rf /" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "OK."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "delete everything"
  let evts = events (asSession state')
      policyEvts = filter (\e -> evType e == EPolicyDecision) evts
      -- The Deny rule fires, so the policy decision text contains "Deny"
      deniedByPolicy = filter (T.isInfixOf "Deny" . evData) policyEvts
  if not (null deniedByPolicy)
    then pure $ Right ()
    else pure $ Left $ "Expected policy denial for dangerous command, got: " ++ show policyEvts

-- ---------------------------------------------------------------------------

tests :: [Test]
tests =
  [ testApprovalApproved
  , testApprovalRejected
  , testApprovalSessionEvents
  , testRejectionSessionEvents
  , testDangerousDeniedWithoutPrompting
  ]
