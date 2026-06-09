{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Haskode.Test.Patch (tests) where

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

-- Patch confirmation display tests
-- ---------------------------------------------------------------------------

-- | computePatchPreview returns the path and diff for a valid file.
testComputePatchPreviewNormal :: Test
testComputePatchPreviewNormal = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-preview-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  let testFile = root </> "Foo.hs"
  TIO.writeFile testFile "module Foo where\nfoo = 1\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("Foo.hs" :: T.Text)
        , "replacement" .= ("module Foo where\nfoo = 2\n" :: T.Text)
        ]
  result <- computePatchPreview args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Left err -> pure $ Left $ "Expected Right, got Left: " ++ T.unpack err
    Right (path, diff)
      | path /= "Foo.hs" ->
          pure $ Left $ "Expected path Foo.hs, got: " ++ path
      | not (T.isInfixOf "-module Foo where" diff) ->
          pure $ Left $ "Diff missing old marker: " ++ T.unpack (T.take 200 diff)
      | not (T.isInfixOf "+module Foo where" diff) ->
          pure $ Left $ "Diff missing new marker: " ++ T.unpack (T.take 200 diff)
      | otherwise -> pure $ Right ()

-- | computePatchPreview returns an error for a missing path field.
testComputePatchPreviewMissingPath :: Test
testComputePatchPreviewMissingPath = do
  let args = object [ "replacement" .= ("content" :: T.Text) ]
  result <- computePatchPreview args
  case result of
    Left _err -> pure $ Right ()
    Right _ -> pure $ Left "Expected Left for missing path, got Right"

-- | computePatchPreview returns an error for a path outside the working dir.
testComputePatchPreviewOutsideRoot :: Test
testComputePatchPreviewOutsideRoot = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-preview-outside"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  -- Use an absolute path that cannot resolve under the root.
  let args = object
        [ "path"        .= ("C:\\haskode_nonexistent_root_test\\file.txt" :: T.Text)
        , "replacement" .= ("new" :: T.Text)
        ]
  result <- computePatchPreview args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Left _err -> pure $ Right ()
    Right _ -> pure $ Left "Expected Left for outside-root path, got Right"

-- | computePatchPreview returns an error for a nonexistent file.
testComputePatchPreviewMissingFile :: Test
testComputePatchPreviewMissingFile = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-preview-missing"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("nonexistent.hs" :: T.Text)
        , "replacement" .= ("new" :: T.Text)
        ]
  result <- computePatchPreview args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Left err
      | T.isInfixOf "could not resolve" err -> pure $ Right ()
      | otherwise -> pure $ Left $ "Unexpected error: " ++ T.unpack err
    Right _ -> pure $ Left "Expected Left for missing file, got Right"

-- | When apply_patch is approved, the approval function receives a
--   reason that includes the target file path.  We capture the reason
--   text via a custom approval function.
testApplyPatchApprovalShowsPath :: Test
testApplyPatchApprovalShowsPath = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-approval-path-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  let testFile = root </> "Target.hs"
  TIO.writeFile testFile "module Target where\ntarget = 0\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  -- Custom approval function that captures the reason text
  capturedReason <- Data.IORef.newIORef ("" :: T.Text)
  let captureApprove :: ApprovalFunc
      captureApprove _tc reason = do
        Data.IORef.writeIORef capturedReason reason
        pure True
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Applying patch."
        , crToolCalls = Just [ToolCall "tc-ap1" "apply_patch"
                               (object [ "path"        .= ("Target.hs" :: T.Text)
                                       , "replacement" .= ("module Target where\ntarget = 1\n" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry captureApprove
  state' <- runAgent state "apply the patch"
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  reason <- Data.IORef.readIORef capturedReason
  -- The reason should include the path "Target.hs"
  let pathInReason = T.isInfixOf "Target.hs" reason
  -- The session should still have the standard policy decision and approval
  let evts = events (asSession state')
      policyEvts = filter (\e -> evType e == EPolicyDecision) evts
      approvedEvts = filter (T.isInfixOf "approved" . evData) policyEvts
  if pathInReason && not (null approvedEvts)
    then pure $ Right ()
    else pure $ Left $ "pathInReason=" ++ show pathInReason
                     ++ " approved=" ++ show (not (null approvedEvts))
                     ++ " reason=" ++ T.unpack reason

-- | When apply_patch is rejected, the file is unchanged and the
--   session records the rejection cleanly.
testApplyPatchRejectionShowsPath :: Test
testApplyPatchRejectionShowsPath = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-reject-path-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  let testFile = root </> "Target.hs"
      original = "module Target where\ntarget = 0\n"
  TIO.writeFile testFile original
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Applying patch."
        , crToolCalls = Just [ToolCall "tc-ap2" "apply_patch"
                               (object [ "path"        .= ("Target.hs" :: T.Text)
                                       , "replacement" .= ("module Target where\ntarget = 99\n" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "OK, I won't."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoReject
  state' <- runAgent state "apply the patch"
  setCurrentDirectory origDir
  -- File should be unchanged
  after <- TIO.readFile testFile
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let fileUnchanged = after == original
  -- Session should show the policy decision and the rejection
  let evts = events (asSession state')
      policyEvts = filter (\e -> evType e == EPolicyDecision) evts
      askEvts = filter (T.isInfixOf "AskUser" . evData) policyEvts
      deniedEvts = filter (\e -> evType e == EToolResult
                                 && T.isInfixOf "denied by user" (evData e)) evts
  if fileUnchanged && not (null askEvts) && not (null deniedEvts)
    then pure $ Right ()
    else pure $ Left $ "unchanged=" ++ show fileUnchanged
                     ++ " askEvts=" ++ show (not (null askEvts))
                     ++ " denied=" ++ show (not (null deniedEvts))

-- ---------------------------------------------------------------------------
-- Audit-log tests for apply_patch session events
-- ---------------------------------------------------------------------------

-- | Approved apply_patch logs the target path in the approval event.
testApplyPatchAuditApprovalWithPath :: Test
testApplyPatchAuditApprovalWithPath = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-audit-approve"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  let testFile = root </> "Foo.hs"
  TIO.writeFile testFile "module Foo where\nfoo = 0\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Applying."
        , crToolCalls = Just [ToolCall "tc-audit1" "apply_patch"
                               (object [ "path"        .= ("Foo.hs" :: T.Text)
                                       , "replacement" .= ("module Foo where\nfoo = 1\n" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "apply patch"
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let evts = events (asSession state')
      -- Find the approval policy decision event
      policyEvts = filter (\e -> evType e == EPolicyDecision) evts
      approvalEvts = filter (T.isInfixOf "approved" . evData) policyEvts
  case approvalEvts of
    (a:_) ->
      if T.isInfixOf "Foo.hs" (evData a)
        then pure $ Right ()
        else pure $ Left $ "Approval event missing path: " ++ T.unpack (evData a)
    _ -> pure $ Left "No approval event found"

-- | Rejected apply_patch logs the target path in the denial event.
testApplyPatchAuditRejectionWithPath :: Test
testApplyPatchAuditRejectionWithPath = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-audit-reject"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  let testFile = root </> "Bar.hs"
  TIO.writeFile testFile "module Bar where\nbar = 0\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Applying."
        , crToolCalls = Just [ToolCall "tc-audit2" "apply_patch"
                               (object [ "path"        .= ("Bar.hs" :: T.Text)
                                       , "replacement" .= ("module Bar where\nbar = 99\n" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "OK."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoReject
  state' <- runAgent state "apply patch"
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let evts = events (asSession state')
      -- Find the denial tool-result event
      trEvts = filter (\e -> evType e == EToolResult) evts
      denialEvts = filter (T.isInfixOf "denied by user" . evData) trEvts
  case denialEvts of
    (d:_) ->
      if T.isInfixOf "Bar.hs" (evData d)
        then pure $ Right ()
        else pure $ Left $ "Denial event missing path: " ++ T.unpack (evData d)
    _ -> pure $ Left "No denial event found"

-- | Applied patch session event includes a bounded diff (not the
--   full unbounded output).  The conversation keeps the full output,
--   but the session event is truncated.
testApplyPatchAuditResultBounded :: Test
testApplyPatchAuditResultBounded = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-audit-bounded"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  let testFile = root </> "Big.hs"
      -- Create a file with enough content to produce a large diff
      bigContent = T.unlines $ replicate 200 ("old line " <> T.pack (show (1 :: Int)))
      bigReplacement = T.unlines $ replicate 200 ("new line " <> T.pack (show (2 :: Int)))
  TIO.writeFile testFile bigContent
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Applying."
        , crToolCalls = Just [ToolCall "tc-audit3" "apply_patch"
                               (object [ "path"        .= ("Big.hs" :: T.Text)
                                       , "replacement" .= bigReplacement])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "apply big patch"
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let evts = events (asSession state')
      -- Find the tool result event for tc-audit3
      trEvts = filter (\e -> evType e == EToolResult) evts
      patchResults = filter (T.isInfixOf "tc-audit3" . evData) trEvts
      -- The conversation should have the full output
      conv = asConversation state'
      convResults = filter (\m -> msgCallId m == Just "tc-audit3") conv
  case (patchResults, convResults) of
    ([pEvt], [cMsg]) -> do
      let sessionLen = T.length (evData pEvt)
          convLen    = T.length (msgContent cMsg)
          sessionTruncated = T.isInfixOf "[truncated:" (evData pEvt)
      -- Session event should be bounded (<= 1024 + overhead for call ID prefix)
      -- Conversation should have the full output
      if convLen > 1024 && sessionLen < convLen && sessionTruncated
        then pure $ Right ()
        else pure $ Left $ "sessionLen=" ++ show sessionLen
                         ++ " convLen=" ++ show convLen
                         ++ " truncated=" ++ show sessionTruncated
    _ -> pure $ Left $ "Expected 1 session event and 1 conversation msg, got "
                     ++ show (length patchResults) ++ " / "
                     ++ show (length convResults)

-- | Read-only tools (read_file) have unchanged session logging behavior —
--   the session event data matches the tool output exactly.
testReadOnlyAuditUnchanged :: Test
testReadOnlyAuditUnchanged = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-audit-ro-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "audit-ro.txt") "read-only audit test"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Reading."
        , crToolCalls = Just [ToolCall "tc-ro1" "read_file"
                                 (object ["path" .= ("audit-ro.txt" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "read file"
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let evts = events (asSession state')
      trEvts = filter (\e -> evType e == EToolResult) evts
      conv = asConversation state'
      convResults = filter (\m -> msgCallId m == Just "tc-ro1") conv
  case (trEvts, convResults) of
    ([rEvt], [cMsg]) -> do
      -- Session event should contain the full file content (not truncated)
      let sessionData = evData rEvt
          convData    = msgContent cMsg
      if T.isInfixOf "read-only audit test" sessionData
         && sessionData == ("tc-ro1 " <> convData)
        then pure $ Right ()
        else pure $ Left $ "Session data mismatch: "
                         ++ T.unpack (T.take 200 sessionData)
    _ -> pure $ Left $ "Expected 1 session event and 1 conversation msg, got "
                     ++ show (length trEvts) ++ " / "
                     ++ show (length convResults)

-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- preview_patch tool tests
-- ---------------------------------------------------------------------------

-- | previewPatchTool produces a normal unified diff for an in-root file.
testPreviewPatchNormalDiff :: Test
testPreviewPatchNormalDiff = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("Main.hs" :: T.Text)
        , "replacement" .= ("module Main where\nmain = putStrLn \"hello\"\n" :: T.Text)
        ]
  result <- toolExecute previewPatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- The diff should contain the old and new content markers
  if T.isInfixOf "--- Main.hs" out
     && T.isInfixOf "-module Main where" out
     && T.isInfixOf "+module Main where" out
     && T.isInfixOf "Diff preview" out
     && T.isInfixOf "no files modified" out
    then pure $ Right ()
    else pure $ Left $ "preview_patch diff: " ++ T.unpack (T.take 300 out)

-- | previewPatchTool does NOT modify the filesystem.
testPreviewPatchNoModification :: Test
testPreviewPatchNoModification = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  original <- TIO.readFile "Main.hs"
  let args = object
        [ "path"        .= ("Main.hs" :: T.Text)
        , "replacement" .= ("completely different content\n" :: T.Text)
        ]
  _ <- toolExecute previewPatchTool args
  after <- TIO.readFile "Main.hs"
  setCurrentDirectory origDir
  cleanupAction
  if original == after
    then pure $ Right ()
    else pure $ Left "preview_patch modified the file!"

-- | previewPatchTool rejects paths outside the working directory.
testPreviewPatchOutsideRoot :: Test
testPreviewPatchOutsideRoot = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("C:\\Windows\\System32\\drivers\\etc\\hosts" :: T.Text)
        , "replacement" .= ("new content" :: T.Text)
        ]
  result <- toolExecute previewPatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "preview_patch outside root: " ++ T.unpack out

-- | previewPatchTool handles broken symlinks gracefully.
testPreviewPatchBrokenSymlink :: Test
testPreviewPatchBrokenSymlink = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-preview-broken"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createFileLink (root </> "nonexistent") (root </> "broken.hs")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("broken.hs" :: T.Text)
        , "replacement" .= ("new content" :: T.Text)
        ]
  result <- toolExecute previewPatchTool args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "could not resolve" out
    then pure $ Right ()
    else pure $ Left $ "preview_patch broken symlink: " ++ T.unpack out

-- | previewPatchTool returns an error for a missing file.
testPreviewPatchMissingFile :: Test
testPreviewPatchMissingFile = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("nonexistent.hs" :: T.Text)
        , "replacement" .= ("new content" :: T.Text)
        ]
  result <- toolExecute previewPatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "could not resolve" out
    then pure $ Right ()
    else pure $ Left $ "preview_patch missing file: " ++ T.unpack out

-- | previewPatchTool refuses diffs that exceed the size limit.
testPreviewPatchTooLarge :: Test
testPreviewPatchTooLarge = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  -- Generate a replacement string large enough to exceed the diff limit.
  -- The file Main.hs is ~40 chars; the replacement is ~10000 chars.
  -- The diff will be ~10000+ chars, exceeding the 8192 limit.
  let bigReplacement = T.replicate 10000 "x"
      args = object
        [ "path"        .= ("Main.hs" :: T.Text)
        , "replacement" .= bigReplacement
        ]
  result <- toolExecute previewPatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "too large" out
    then pure $ Right ()
    else pure $ Left $ "preview_patch too large: " ++ T.unpack (T.take 200 out)

-- | previewPatchTool is in the default registry.
testPreviewPatchInRegistry :: Test
testPreviewPatchInRegistry =
  if "preview_patch" `elem` toolNames defaultRegistry
    then pure $ Right ()
    else pure $ Left $ "preview_patch not in registry: " ++ show (toolNames defaultRegistry)

-- | previewPatchTool is allowed by default policy (no approval needed).
testPreviewPatchPolicyAllow :: Test
testPreviewPatchPolicyAllow =
  let tc = ToolCall "tc-pp" "preview_patch" (object ["path" .= ("x.hs" :: T.Text), "replacement" .= ("y" :: T.Text)])
  in case checkPolicy defaultPolicy tc of
       Allow -> pure $ Right ()
       other -> pure $ Left $ "Expected Allow for preview_patch, got: " ++ show other

-- ---------------------------------------------------------------------------
-- apply_patch tool tests
-- ---------------------------------------------------------------------------

-- | applyPatchTool successfully applies a patch to an in-root file.
testApplyPatchSuccess :: Test
testApplyPatchSuccess = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("Main.hs" :: T.Text)
        , "replacement" .= ("module Main where\nmain = putStrLn \"patched\"\n" :: T.Text)
        ]
  result <- toolExecute applyPatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "Patch applied" out && T.isInfixOf "--- Main.hs" out
    then pure $ Right ()
    else pure $ Left $ "apply_patch result: " ++ T.unpack (T.take 300 out)

-- | applyPatchTool actually changes the file content on disk.
testApplyPatchFileChanged :: Test
testApplyPatchFileChanged = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let newContent = "module Main where\nmain = putStrLn \"changed\"\n"
      args = object
        [ "path"        .= ("Main.hs" :: T.Text)
        , "replacement" .= newContent
        ]
  _ <- toolExecute applyPatchTool args
  after <- TIO.readFile "Main.hs"
  setCurrentDirectory origDir
  cleanupAction
  if after == newContent
    then pure $ Right ()
    else pure $ Left $ "apply_patch did not change file: " ++ T.unpack (T.take 200 after)

-- | applyPatchTool rejects paths outside the working directory.
testApplyPatchOutsideRoot :: Test
testApplyPatchOutsideRoot = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("C:\\Windows\\System32\\drivers\\etc\\hosts" :: T.Text)
        , "replacement" .= ("new content" :: T.Text)
        ]
  result <- toolExecute applyPatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "apply_patch outside root: " ++ T.unpack out

-- | applyPatchTool returns an error for a missing file.
testApplyPatchMissingFile :: Test
testApplyPatchMissingFile = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("nonexistent.hs" :: T.Text)
        , "replacement" .= ("new content" :: T.Text)
        ]
  result <- toolExecute applyPatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "could not resolve" out
    then pure $ Right ()
    else pure $ Left $ "apply_patch missing file: " ++ T.unpack out

-- | applyPatchTool handles broken symlinks gracefully.
testApplyPatchBrokenSymlink :: Test
testApplyPatchBrokenSymlink = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-apply-broken"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createFileLink (root </> "nonexistent") (root </> "broken.hs")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("broken.hs" :: T.Text)
        , "replacement" .= ("new content" :: T.Text)
        ]
  result <- toolExecute applyPatchTool args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "could not resolve" out
    then pure $ Right ()
    else pure $ Left $ "apply_patch broken symlink: " ++ T.unpack out

-- | applyPatchTool includes the diff in its result.
testApplyPatchDiffInResult :: Test
testApplyPatchDiffInResult = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"        .= ("Main.hs" :: T.Text)
        , "replacement" .= ("module New where\n" :: T.Text)
        ]
  result <- toolExecute applyPatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- The diff should contain old and new markers
  if T.isInfixOf "-module Main where" out && T.isInfixOf "+module New where" out
    then pure $ Right ()
    else pure $ Left $ "apply_patch diff: " ++ T.unpack (T.take 300 out)

-- | applyPatchTool is in the default registry.
testApplyPatchInRegistry :: Test
testApplyPatchInRegistry =
  if "apply_patch" `elem` toolNames defaultRegistry
    then pure $ Right ()
    else pure $ Left $ "apply_patch not in registry: " ++ show (toolNames defaultRegistry)

-- | applyPatchTool is NOT allowed by default policy (requires confirmation).
testApplyPatchPolicyAskUser :: Test
testApplyPatchPolicyAskUser =
  let tc = ToolCall "tc-ap" "apply_patch" (object ["path" .= ("x.hs" :: T.Text), "replacement" .= ("y" :: T.Text)])
  in case checkPolicy defaultPolicy tc of
       AskUser _ -> pure $ Right ()
       other     -> pure $ Left $ "Expected AskUser for apply_patch, got: " ++ show other

-- ---------------------------------------------------------------------------
-- write_file tool tests
-- ---------------------------------------------------------------------------

-- | write_file successfully creates a new in-root file after approval.
testWriteFileSuccess :: Test
testWriteFileSuccess = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-writefile-success"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"    .= ("new_file.hs" :: T.Text)
        , "content" .= ("module New where\nnew = 1\n" :: T.Text)
        ]
  result <- toolExecute writeFileTool args
  setCurrentDirectory origDir
  -- Read back the file to verify content
  exists <- doesFileExist (root </> "new_file.hs")
  content <- if exists then TIO.readFile (root </> "new_file.hs") else pure ""
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "File created" out
     && T.isInfixOf "new_file.hs" out
     && T.isInfixOf "+module New where" out
     && exists
     && T.isInfixOf "module New where" content
    then pure $ Right ()
    else pure $ Left $ "write_file success: out=" ++ T.unpack (T.take 200 out)
                     ++ " exists=" ++ show exists

-- | write_file refuses to overwrite an existing file.
testWriteFileRejectsOverwrite :: Test
testWriteFileRejectsOverwrite = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-writefile-overwrite"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "existing.hs") "module Existing where\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"    .= ("existing.hs" :: T.Text)
        , "content" .= ("module Overwritten where\n" :: T.Text)
        ]
  result <- toolExecute writeFileTool args
  setCurrentDirectory origDir
  -- File should be unchanged
  after <- TIO.readFile (root </> "existing.hs")
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "error" out
     && T.isInfixOf "already exists" out
     && T.isInfixOf "Existing" after  -- unchanged
    then pure $ Right ()
    else pure $ Left $ "write_file overwrite: " ++ T.unpack (T.take 200 out)

-- | write_file rejects paths outside the working directory.
testWriteFileOutsideRoot :: Test
testWriteFileOutsideRoot = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-writefile-outside"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"    .= ("C:\\Windows\\System32\\haskode-outside-test.txt" :: T.Text)
        , "content" .= ("outside content" :: T.Text)
        ]
  result <- toolExecute writeFileTool args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "write_file outside root: " ++ T.unpack (T.take 200 out)

-- | write_file rejects symlinks that resolve outside the working directory.
testWriteFileRejectsOutsideRootSymlink :: Test
testWriteFileRejectsOutsideRootSymlink = do
  tmpDir <- getTemporaryDirectory
  let root   = tmpDir </> "haskode-writefile-sym-root"
      target = tmpDir </> "haskode-writefile-sym-target"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  createDirectory root
  createDirectory target
  createDirectoryLink target (root </> "outside-dir")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"    .= ("outside-dir/newfile.txt" :: T.Text)
        , "content" .= ("symlink escape" :: T.Text)
        ]
  result <- toolExecute writeFileTool args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "working directory" out
    then pure $ Right ()
    else pure $ Left $ "write_file outside symlink: " ++ T.unpack (T.take 200 out)

-- | write_file rejects missing parent directories.
testWriteFileMissingParent :: Test
testWriteFileMissingParent = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-writefile-missing-parent"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"    .= ("nonexistent_dir/newfile.txt" :: T.Text)
        , "content" .= ("content" :: T.Text)
        ]
  result <- toolExecute writeFileTool args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "error" out
     && (T.isInfixOf "parent" out || T.isInfixOf "does not exist" out)
    then pure $ Right ()
    else pure $ Left $ "write_file missing parent: " ++ T.unpack (T.take 200 out)

-- | write_file rejects directory targets.
testWriteFileRejectsDirectory :: Test
testWriteFileRejectsDirectory = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-writefile-dir"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createDirectory (root </> "subdir")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"    .= ("subdir" :: T.Text)
        , "content" .= ("content" :: T.Text)
        ]
  result <- toolExecute writeFileTool args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "directory" out
    then pure $ Right ()
    else pure $ Left $ "write_file directory: " ++ T.unpack (T.take 200 out)

-- | write_file is in the default registry.
testWriteFileInRegistry :: Test
testWriteFileInRegistry =
  if "write_file" `elem` toolNames defaultRegistry
    then pure $ Right ()
    else pure $ Left $ "write_file not in registry: " ++ show (toolNames defaultRegistry)

-- | write_file is NOT allowed by default policy (requires confirmation).
testWriteFilePolicyAskUser :: Test
testWriteFilePolicyAskUser =
  let tc = ToolCall "tc-wf" "write_file" (object ["path" .= ("x.hs" :: T.Text), "content" .= ("y" :: T.Text)])
  in case checkPolicy defaultPolicy tc of
       AskUser _ -> pure $ Right ()
       other     -> pure $ Left $ "Expected AskUser for write_file, got: " ++ show other

-- | computeWriteFilePreview returns the path and preview for a valid new file.
testComputeWriteFilePreviewNormal :: Test
testComputeWriteFilePreviewNormal = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-wf-preview-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"    .= ("New.hs" :: T.Text)
        , "content" .= ("module New where\nnew = 1\n" :: T.Text)
        ]
  result <- computeWriteFilePreview args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Left err -> pure $ Left $ "Expected Right, got Left: " ++ T.unpack err
    Right (path, preview)
      | path /= "New.hs" ->
          pure $ Left $ "Expected path New.hs, got: " ++ path
      | not (T.isInfixOf "(new file)" preview) ->
          pure $ Left $ "Preview missing new file marker: " ++ T.unpack (T.take 200 preview)
      | not (T.isInfixOf "+module New where" preview) ->
          pure $ Left $ "Preview missing content: " ++ T.unpack (T.take 200 preview)
      | otherwise -> pure $ Right ()

-- | computeWriteFilePreview returns an error for an existing file.
testComputeWriteFilePreviewExisting :: Test
testComputeWriteFilePreviewExisting = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-wf-preview-existing"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "Existing.hs") "module Existing where\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"    .= ("Existing.hs" :: T.Text)
        , "content" .= ("new content" :: T.Text)
        ]
  result <- computeWriteFilePreview args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Left err
      | T.isInfixOf "already exists" err -> pure $ Right ()
      | otherwise -> pure $ Left $ "Unexpected error: " ++ T.unpack err
    Right _ -> pure $ Left "Expected Left for existing file, got Right"

-- | computeWriteFilePreview returns an error for a path outside the working dir.
testComputeWriteFilePreviewOutsideRoot :: Test
testComputeWriteFilePreviewOutsideRoot = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-wf-preview-outside"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"    .= ("C:\\Windows\\System32\\haskode-outside-preview.txt" :: T.Text)
        , "content" .= ("content" :: T.Text)
        ]
  result <- computeWriteFilePreview args
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Left _ -> pure $ Right ()
    Right _ -> pure $ Left "Expected Left for outside-root path, got Right"

-- | When write_file is approved, the approval function receives a
--   reason that includes the target file path.
testWriteFileApprovalShowsPath :: Test
testWriteFileApprovalShowsPath = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-wf-approval-path"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  capturedReason <- Data.IORef.newIORef ("" :: T.Text)
  let captureApprove :: ApprovalFunc
      captureApprove _tc reason = do
        Data.IORef.writeIORef capturedReason reason
        pure True
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Creating file."
        , crToolCalls = Just [ToolCall "tc-wf1" "write_file"
                               (object [ "path"    .= ("New.hs" :: T.Text)
                                       , "content" .= ("module New where\n" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry captureApprove
  state' <- runAgent state "create new file"
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  reason <- Data.IORef.readIORef capturedReason
  let pathInReason = T.isInfixOf "New.hs" reason
  let evts = events (asSession state')
      policyEvts = filter (\e -> evType e == EPolicyDecision) evts
      approvedEvts = filter (T.isInfixOf "approved" . evData) policyEvts
  if pathInReason && not (null approvedEvts)
    then pure $ Right ()
    else pure $ Left $ "pathInReason=" ++ show pathInReason
                     ++ " approved=" ++ show (not (null approvedEvts))
                     ++ " reason=" ++ T.unpack reason

-- | When write_file is rejected, the file is not created and the
--   session records the rejection cleanly.
testWriteFileRejectionShowsPath :: Test
testWriteFileRejectionShowsPath = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-wf-reject-path"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Creating file."
        , crToolCalls = Just [ToolCall "tc-wf2" "write_file"
                               (object [ "path"    .= ("New.hs" :: T.Text)
                                       , "content" .= ("module New where\n" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "OK, I won't."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoReject
  state' <- runAgent state "create new file"
  setCurrentDirectory origDir
  -- File should NOT exist
  exists <- doesFileExist (root </> "New.hs")
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let evts = events (asSession state')
      policyEvts = filter (\e -> evType e == EPolicyDecision) evts
      askEvts = filter (T.isInfixOf "AskUser" . evData) policyEvts
      deniedEvts = filter (\e -> evType e == EToolResult
                                 && T.isInfixOf "denied by user" (evData e)) evts
  if not exists && not (null askEvts) && not (null deniedEvts)
    then pure $ Right ()
    else pure $ Left $ "exists=" ++ show exists
                     ++ " askEvts=" ++ show (not (null askEvts))
                     ++ " denied=" ++ show (not (null deniedEvts))

-- | Approved write_file logs the target path in the approval event.
testWriteFileAuditApprovalWithPath :: Test
testWriteFileAuditApprovalWithPath = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-wf-audit-approve"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Creating."
        , crToolCalls = Just [ToolCall "tc-wfa1" "write_file"
                               (object [ "path"    .= ("Audit.hs" :: T.Text)
                                       , "content" .= ("module Audit where\n" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "create file"
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let evts = events (asSession state')
      policyEvts = filter (\e -> evType e == EPolicyDecision) evts
      approvalEvts = filter (T.isInfixOf "approved" . evData) policyEvts
  case approvalEvts of
    (a:_) ->
      if T.isInfixOf "Audit.hs" (evData a)
        then pure $ Right ()
        else pure $ Left $ "Approval event missing path: " ++ T.unpack (evData a)
    _ -> pure $ Left "No approval event found"

-- | Rejected write_file logs the target path in the denial event.
testWriteFileAuditRejectionWithPath :: Test
testWriteFileAuditRejectionWithPath = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-wf-audit-reject"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Creating."
        , crToolCalls = Just [ToolCall "tc-wfa2" "write_file"
                               (object [ "path"    .= ("Reject.hs" :: T.Text)
                                       , "content" .= ("module Reject where\n" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "OK."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoReject
  state' <- runAgent state "create file"
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let evts = events (asSession state')
      trEvts = filter (\e -> evType e == EToolResult) evts
      denialEvts = filter (T.isInfixOf "denied by user" . evData) trEvts
  case denialEvts of
    (d:_) ->
      if T.isInfixOf "Reject.hs" (evData d)
        then pure $ Right ()
        else pure $ Left $ "Denial event missing path: " ++ T.unpack (evData d)
    _ -> pure $ Left "No denial event found"

-- | write_file description mentions confirmation and cannot overwrite.
testWriteFileDescriptionPhrases :: Test
testWriteFileDescriptionPhrases =
  case toolDescriptionFromRegistry "write_file" of
    Nothing -> pure $ Left "write_file not in registry"
    Just desc
      | not (T.isInfixOf "confirmation" desc || T.isInfixOf "confirm" desc)
        -> pure $ Left $ "write_file description missing confirmation: " ++ T.unpack desc
      | not (T.isInfixOf "existing" desc || T.isInfixOf "overwrite" desc || T.isInfixOf "Cannot overwrite" desc)
        -> pure $ Left $ "write_file description missing overwrite protection: " ++ T.unpack desc
      | otherwise -> pure $ Right ()

-- ---------------------------------------------------------------------------
-- Patch workflow smoke tests (agent-loop integration)
-- ---------------------------------------------------------------------------

-- | End-to-end smoke test: the model proposes preview_patch, sees the
--   diff (no file change), then proposes apply_patch, and the file
--   changes only after the policy/approval path allows execution.
--   Uses scriptedProvider + autoApprove to drive the agent loop
--   deterministically.
testPatchWorkflowSmoke :: Test
testPatchWorkflowSmoke = do
  -- Create a temp file with known content.
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-patch-smoke"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  let testFile = root </> "hello.hs"
  TIO.writeFile testFile "module Hello where\ngreet = \"old\"\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let newContent = "module Hello where\ngreet = \"new\"\n"
  prov <- scriptedProvider
    [ -- Turn 1: assistant calls preview_patch
      CompletionResponse
        { crReply     = mkAssistantMessage "Let me preview the change."
        , crToolCalls = Just
            [ ToolCall "tc-pv" "preview_patch"
                (object ["path" .= ("hello.hs" :: T.Text), "replacement" .= newContent])
            ]
        }
    , -- Turn 2: assistant calls apply_patch (preview looked good)
      CompletionResponse
        { crReply     = mkAssistantMessage "The diff looks correct, applying."
        , crToolCalls = Just
            [ ToolCall "tc-ap" "apply_patch"
                (object ["path" .= ("hello.hs" :: T.Text), "replacement" .= newContent])
            ]
        }
    , -- Turn 3: final text reply
      CompletionResponse
        { crReply     = mkAssistantMessage "Patch applied successfully."
        , crToolCalls = Nothing
        }
    ]
  let cfg   = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove
  state' <- runAgent state "fix the greeting"
  setCurrentDirectory origDir
  -- Verify conversation structure.
  let conv = asConversation state'
      evts = events (asSession state')
      types = map evType evts
  -- The file should now have the new content.
  after <- TIO.readFile testFile
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  -- Check session event types.
  let hasUserMsg     = EUserMessage `elem` types
      hasAsstReply   = EAssistantReply `elem` types
      hasToolCall    = EToolCall `elem` types
      hasToolResult  = EToolResult `elem` types
      hasPolicyDec   = EPolicyDecision `elem` types
  -- Check that the tool results contain expected content.
  let toolResults  = filter (\m -> msgCallId m /= Nothing) conv
      previewResult = filter (T.isInfixOf "Diff preview" . msgContent) toolResults
      applyResult   = filter (T.isInfixOf "Patch applied" . msgContent) toolResults
  -- Check that apply_patch was approved (AskUser path).
  -- The approval event for apply_patch includes the target path:
  -- "apply_patch: approved — <path>"
  let approvedEvts = filter (T.isInfixOf "approved" . evData)
                             (filter (\e -> evType e == EPolicyDecision) evts)
  -- Verify file content changed.
  let fileChanged = after == newContent
  if hasUserMsg && hasAsstReply && hasToolCall && hasToolResult
     && hasPolicyDec && not (null previewResult) && not (null applyResult)
     && not (null approvedEvts) && fileChanged
    then pure $ Right ()
    else pure $ Left $
         "events=" ++ show types
      ++ " previewResult=" ++ show (length previewResult)
      ++ " applyResult=" ++ show (length applyResult)
      ++ " approved=" ++ show (not (null approvedEvts))
      ++ " fileChanged=" ++ show fileChanged

-- | Rejection smoke test: the model proposes apply_patch, but the
--   approval function rejects it.  No file is modified and the session
--   log records the policy decision and denial clearly.
testPatchWorkflowRejectSmoke :: Test
testPatchWorkflowRejectSmoke = do
  -- Create a temp file with known content.
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-patch-reject"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  let testFile = root </> "hello.hs"
      original = "module Hello where\ngreet = \"old\"\n"
  TIO.writeFile testFile original
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ -- Turn 1: assistant calls apply_patch
      CompletionResponse
        { crReply     = mkAssistantMessage "Applying the patch now."
        , crToolCalls = Just
            [ ToolCall "tc-ap" "apply_patch"
                (object [ "path"        .= ("hello.hs" :: T.Text)
                        , "replacement" .= ("module Hello where\ngreet = \"rejected\"\n" :: T.Text)
                        ])
            ]
        }
    , -- Turn 2: final text reply acknowledging rejection
      CompletionResponse
        { crReply     = mkAssistantMessage "OK, I won't apply the patch."
        , crToolCalls = Nothing
        }
    ]
  let cfg   = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoReject
  state' <- runAgent state "fix the greeting"
  setCurrentDirectory origDir
  -- Verify file is unchanged.
  after <- TIO.readFile testFile
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let fileUnchanged = after == original
  -- Verify session log records the denial.
  let evts = events (asSession state')
      types = map evType evts
      hasPolicyDec  = EPolicyDecision `elem` types
      hasToolResult = EToolResult `elem` types
      -- The denial message should appear in tool result events.
      deniedEvts = filter (\e -> evType e == EToolResult
                                 && T.isInfixOf "denied by user" (evData e)) evts
      -- The conversation should contain a denial message for the tool call.
      conv = asConversation state'
      denialMsgs = filter (T.isInfixOf "denied by user" . msgContent) conv
  if hasPolicyDec && hasToolResult && not (null deniedEvts)
     && not (null denialMsgs) && fileUnchanged
    then pure $ Right ()
    else pure $ Left $
         "events=" ++ show types
      ++ " deniedEvts=" ++ show (length deniedEvts)
      ++ " denialMsgs=" ++ show (length denialMsgs)
      ++ " fileUnchanged=" ++ show fileUnchanged


tests :: [Test]
tests =
  [ testComputePatchPreviewNormal
  , testComputePatchPreviewMissingPath
  , skipOnWindows testComputePatchPreviewOutsideRoot
  , testComputePatchPreviewMissingFile
  , testApplyPatchApprovalShowsPath
  , testApplyPatchRejectionShowsPath
  , testApplyPatchAuditApprovalWithPath
  , testApplyPatchAuditRejectionWithPath
  , testApplyPatchAuditResultBounded
  , testReadOnlyAuditUnchanged
  , testPreviewPatchNormalDiff
  , testPreviewPatchNoModification
  , testPreviewPatchOutsideRoot
  , skipIfNoSymlinks testPreviewPatchBrokenSymlink
  , testPreviewPatchMissingFile
  , testPreviewPatchTooLarge
  , testPreviewPatchInRegistry
  , testPreviewPatchPolicyAllow
  , testApplyPatchSuccess
  , testApplyPatchFileChanged
  , testApplyPatchOutsideRoot
  , testApplyPatchMissingFile
  , skipIfNoSymlinks testApplyPatchBrokenSymlink
  , testApplyPatchDiffInResult
  , testApplyPatchInRegistry
  , testApplyPatchPolicyAskUser
  , testWriteFileSuccess
  , testWriteFileRejectsOverwrite
  , testWriteFileOutsideRoot
  , skipIfNoSymlinks testWriteFileRejectsOutsideRootSymlink
  , testWriteFileMissingParent
  , testWriteFileRejectsDirectory
  , testWriteFileInRegistry
  , testWriteFilePolicyAskUser
  , testComputeWriteFilePreviewNormal
  , testComputeWriteFilePreviewExisting
  , skipOnWindows testComputeWriteFilePreviewOutsideRoot
  , testWriteFileApprovalShowsPath
  , testWriteFileRejectionShowsPath
  , testWriteFileAuditApprovalWithPath
  , testWriteFileAuditRejectionWithPath
  , testWriteFileDescriptionPhrases
  , testPatchWorkflowSmoke
  , testPatchWorkflowRejectSmoke
  ]
