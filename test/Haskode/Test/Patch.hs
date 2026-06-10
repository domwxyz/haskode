{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

-- | Patch, preview, write_file, and patch workflow tests.
module Haskode.Test.Patch (tests) where

import Control.Exception ( IOException, try )
import Data.Aeson ( Value(..), object, KeyValue((.=)) )
import qualified Data.Vector as V (fromList)
import Haskode.Agent
    ( AgentState(asConversation, asSession),
      autoApprove,
      autoReject,
      initState,
      runAgent,
      ApprovalFunc )
import Haskode.Config ( defaultConfig )
import Haskode.Core
    ( ToolResult(trOutput),
      mkAssistantMessage,
      Message(msgContent, msgCallId),
      ToolCall(ToolCall) )
import Haskode.Policy ( checkPolicy, defaultPolicy, Decision(..) )
import Haskode.Provider
    ( scriptedProvider,
      CompletionResponse(crToolCalls, CompletionResponse, crReply) )
import Haskode.Session
    ( events,
      Event(evData, evType),
      EventType(EToolResult, EUserMessage, EAssistantReply, EToolCall,
                EPolicyDecision) )
import Haskode.Test.Util
    ( createTestTree,
      runInTestDir,
      skipIfNoSymlinks,
      skipOnWindows,
      toolDescriptionFromRegistry,
      withTestDir,
      Test )
import Haskode.Patch
    ( formatBatchHeader, showDiff, makePatch, batchOpPreview,
      countAddedLines, countRemovedLines, BatchOp(..),
      ValidatedBatchOp(..), applyValidatedBatchOps,
      formatBatchApplySummary, formatNewFilePreview,
      formatDiffCountSummary, colorizeUnifiedDiff )
import Haskode.Tools
    ( defaultRegistry,
      applyPatchTool,
      applyPatchBatchTool,
      computePatchPreview,
      computeWriteFilePreview,
      previewPatchTool,
      previewPatchBatchTool,
      toolNames,
      writeFileTool,
      Tool(toolExecute) )
import System.Directory
    ( createDirectory,
      createDirectoryLink,
      createFileLink,
      doesFileExist,
      getCurrentDirectory,
      getTemporaryDirectory,
      removeDirectoryRecursive,
      setCurrentDirectory )
import System.FilePath ( (</>) )
import qualified Data.IORef ( newIORef, readIORef, writeIORef )
import qualified Data.Text as T
    ( Text, isInfixOf, length, replicate, take, unlines, pack, unpack )
import qualified Data.Text.IO as TIO ( readFile, writeFile )
-- ---------------------------------------------------------------------------

-- | computePatchPreview returns the path and diff for a valid file.
testComputePatchPreviewNormal :: Test
testComputePatchPreviewNormal = runInTestDir "haskode-preview-test" $ \_root -> do
  TIO.writeFile "Foo.hs" "module Foo where\nfoo = 1\n"
  let args = object
        [ "path"        .= ("Foo.hs" :: T.Text)
        , "replacement" .= ("module Foo where\nfoo = 2\n" :: T.Text)
        ]
  result <- computePatchPreview args
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
testComputePatchPreviewOutsideRoot = runInTestDir "haskode-preview-outside" $ \_root -> do
  -- Use an absolute path that cannot resolve under the root.
  let args = object
        [ "path"        .= ("C:\\haskode_nonexistent_root_test\\file.txt" :: T.Text)
        , "replacement" .= ("new" :: T.Text)
        ]
  result <- computePatchPreview args
  case result of
    Left _err -> pure $ Right ()
    Right _ -> pure $ Left "Expected Left for outside-root path, got Right"

-- | computePatchPreview returns an error for a nonexistent file.
testComputePatchPreviewMissingFile :: Test
testComputePatchPreviewMissingFile = runInTestDir "haskode-preview-missing" $ \_root -> do
  let args = object
        [ "path"        .= ("nonexistent.hs" :: T.Text)
        , "replacement" .= ("new" :: T.Text)
        ]
  result <- computePatchPreview args
  case result of
    Left err
      | T.isInfixOf "could not resolve" err -> pure $ Right ()
      | otherwise -> pure $ Left $ "Unexpected error: " ++ T.unpack err
    Right _ -> pure $ Left "Expected Left for missing file, got Right"

-- | When apply_patch is approved, the approval function receives a
--   reason that includes the target file path.  We capture the reason
--   text via a custom approval function.
testApplyPatchApprovalShowsPath :: Test
testApplyPatchApprovalShowsPath = withTestDir "haskode-approval-path-test" $ \root -> do
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
      state = initState cfg prov defaultPolicy defaultRegistry captureApprove False
  state' <- runAgent state "apply the patch"
  setCurrentDirectory origDir
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
testApplyPatchRejectionShowsPath = withTestDir "haskode-reject-path-test" $ \root -> do
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
      state = initState cfg prov defaultPolicy defaultRegistry autoReject False
  state' <- runAgent state "apply the patch"
  setCurrentDirectory origDir
  -- File should be unchanged
  after <- TIO.readFile testFile
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
testApplyPatchAuditApprovalWithPath = withTestDir "haskode-audit-approve" $ \root -> do
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
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove False
  state' <- runAgent state "apply patch"
  setCurrentDirectory origDir
  let evts = events (asSession state')
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
testApplyPatchAuditRejectionWithPath = withTestDir "haskode-audit-reject" $ \root -> do
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
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoReject False
  state' <- runAgent state "apply patch"
  setCurrentDirectory origDir
  let evts = events (asSession state')
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
testApplyPatchAuditResultBounded = withTestDir "haskode-audit-bounded" $ \root -> do
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
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove False
  state' <- runAgent state "apply big patch"
  setCurrentDirectory origDir
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
testReadOnlyAuditUnchanged = withTestDir "haskode-audit-ro-test" $ \root -> do
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
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove False
  state' <- runAgent state "read file"
  setCurrentDirectory origDir
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
testWriteFileSuccess = withTestDir "haskode-writefile-success" $ \root -> do
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
testWriteFileRejectsOverwrite = withTestDir "haskode-writefile-overwrite" $ \root -> do
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
  let out = trOutput result
  if T.isInfixOf "error" out
     && T.isInfixOf "already exists" out
     && T.isInfixOf "Existing" after  -- unchanged
    then pure $ Right ()
    else pure $ Left $ "write_file overwrite: " ++ T.unpack (T.take 200 out)

-- | write_file rejects paths outside the working directory.
testWriteFileOutsideRoot :: Test
testWriteFileOutsideRoot = runInTestDir "haskode-writefile-outside" $ \_root -> do
  let args = object
        [ "path"    .= ("C:\\Windows\\System32\\haskode-outside-test.txt" :: T.Text)
        , "content" .= ("outside content" :: T.Text)
        ]
  result <- toolExecute writeFileTool args
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
testWriteFileMissingParent = runInTestDir "haskode-writefile-missing-parent" $ \_root -> do
  let args = object
        [ "path"    .= ("nonexistent_dir/newfile.txt" :: T.Text)
        , "content" .= ("content" :: T.Text)
        ]
  result <- toolExecute writeFileTool args
  let out = trOutput result
  if T.isInfixOf "error" out
     && (T.isInfixOf "parent" out || T.isInfixOf "does not exist" out)
    then pure $ Right ()
    else pure $ Left $ "write_file missing parent: " ++ T.unpack (T.take 200 out)

-- | write_file rejects directory targets.
testWriteFileRejectsDirectory :: Test
testWriteFileRejectsDirectory = withTestDir "haskode-writefile-dir" $ \root -> do
  createDirectory (root </> "subdir")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"    .= ("subdir" :: T.Text)
        , "content" .= ("content" :: T.Text)
        ]
  result <- toolExecute writeFileTool args
  setCurrentDirectory origDir
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
testComputeWriteFilePreviewNormal = runInTestDir "haskode-wf-preview-test" $ \_root -> do
  let args = object
        [ "path"    .= ("New.hs" :: T.Text)
        , "content" .= ("module New where\nnew = 1\n" :: T.Text)
        ]
  result <- computeWriteFilePreview args
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
testComputeWriteFilePreviewExisting = withTestDir "haskode-wf-preview-existing" $ \root -> do
  writeFile (root </> "Existing.hs") "module Existing where\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "path"    .= ("Existing.hs" :: T.Text)
        , "content" .= ("new content" :: T.Text)
        ]
  result <- computeWriteFilePreview args
  setCurrentDirectory origDir
  case result of
    Left err
      | T.isInfixOf "already exists" err -> pure $ Right ()
      | otherwise -> pure $ Left $ "Unexpected error: " ++ T.unpack err
    Right _ -> pure $ Left "Expected Left for existing file, got Right"

-- | computeWriteFilePreview returns an error for a path outside the working dir.
testComputeWriteFilePreviewOutsideRoot :: Test
testComputeWriteFilePreviewOutsideRoot = runInTestDir "haskode-wf-preview-outside" $ \_root -> do
  let args = object
        [ "path"    .= ("C:\\Windows\\System32\\haskode-outside-preview.txt" :: T.Text)
        , "content" .= ("content" :: T.Text)
        ]
  result <- computeWriteFilePreview args
  case result of
    Left _ -> pure $ Right ()
    Right _ -> pure $ Left "Expected Left for outside-root path, got Right"

-- | When write_file is approved, the approval function receives a
--   reason that includes the target file path.
testWriteFileApprovalShowsPath :: Test
testWriteFileApprovalShowsPath = withTestDir "haskode-wf-approval-path" $ \root -> do
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
  let state = initState defaultConfig prov defaultPolicy defaultRegistry captureApprove False
  state' <- runAgent state "create new file"
  setCurrentDirectory origDir
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
testWriteFileRejectionShowsPath = withTestDir "haskode-wf-reject-path" $ \root -> do
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
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoReject False
  state' <- runAgent state "create new file"
  setCurrentDirectory origDir
  -- File should NOT exist
  exists <- doesFileExist (root </> "New.hs")
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
testWriteFileAuditApprovalWithPath = withTestDir "haskode-wf-audit-approve" $ \root -> do
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
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove False
  state' <- runAgent state "create file"
  setCurrentDirectory origDir
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
testWriteFileAuditRejectionWithPath = withTestDir "haskode-wf-audit-reject" $ \root -> do
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
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoReject False
  state' <- runAgent state "create file"
  setCurrentDirectory origDir
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
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove False
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
      state = initState cfg prov defaultPolicy defaultRegistry autoReject False
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


-- ---------------------------------------------------------------------------
-- preview_patch_batch tool tests
-- ---------------------------------------------------------------------------

-- | Batch with two existing-file replacements succeeds and contains both diffs.
testBatchTwoReplacements :: Test
testBatchTwoReplacements = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "op" .= ("replace" :: T.Text)
                     , "path" .= ("Main.hs" :: T.Text)
                     , "replacement" .= ("module New where\n" :: T.Text)
                     ]
            , object [ "op" .= ("replace" :: T.Text)
                     , "path" .= ("src/Lib.hs" :: T.Text)
                     , "replacement" .= ("module Lib2 where\n" :: T.Text)
                     ]
            ])
        ]
  result <- toolExecute previewPatchBatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "2 operations" out
     && T.isInfixOf "Operation 1" out
     && T.isInfixOf "Operation 2" out
     && T.isInfixOf "--- Main.hs" out
     && T.isInfixOf "--- src/Lib.hs" out
     && T.isInfixOf "Batch preview" out
    then pure $ Right ()
    else pure $ Left $ "batch two replacements: " ++ T.unpack (T.take 400 out)

-- | Batch with a create preview succeeds.
testBatchCreatePreview :: Test
testBatchCreatePreview = runInTestDir "haskode-batch-create" $ \_root -> do
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "op" .= ("create" :: T.Text)
                     , "path" .= ("New.hs" :: T.Text)
                     , "content" .= ("module New where\nnew = 1\n" :: T.Text)
                     ]
            ])
        ]
  result <- toolExecute previewPatchBatchTool args
  let out = trOutput result
  if T.isInfixOf "1 operations" out
     && T.isInfixOf "Operation 1" out
     && T.isInfixOf "(new file)" out
     && T.isInfixOf "+module New where" out
     && T.isInfixOf "Batch preview" out
    then pure $ Right ()
    else pure $ Left $ "batch create: " ++ T.unpack (T.take 400 out)

-- | Mixed replace/create batch preview succeeds.
testBatchMixedPreview :: Test
testBatchMixedPreview = runInTestDir "haskode-batch-mixed" $ \root -> do
  writeFile (root </> "Existing.hs") "module Existing where\nexisting = 0\n"
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "op" .= ("replace" :: T.Text)
                     , "path" .= ("Existing.hs" :: T.Text)
                     , "replacement" .= ("module Existing where\nexisting = 1\n" :: T.Text)
                     ]
            , object [ "op" .= ("create" :: T.Text)
                     , "path" .= ("NewFile.hs" :: T.Text)
                     , "content" .= ("module NewFile where\n" :: T.Text)
                     ]
            ])
        ]
  result <- toolExecute previewPatchBatchTool args
  let out = trOutput result
  if T.isInfixOf "2 operations" out
     && T.isInfixOf "(replace)" out
     && T.isInfixOf "(create)" out
     && T.isInfixOf "Existing.hs" out
     && T.isInfixOf "NewFile.hs" out
     && T.isInfixOf "Batch preview" out
    then pure $ Right ()
    else pure $ Left $ "batch mixed: " ++ T.unpack (T.take 400 out)

-- | Empty operations list is rejected.
testBatchEmptyOps :: Test
testBatchEmptyOps = do
  let args = object
        [ "operations" .= Array (V.fromList [])
        ]
  result <- toolExecute previewPatchBatchTool args
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "empty" out
    then pure $ Right ()
    else pure $ Left $ "batch empty ops: " ++ T.unpack out

-- | Unknown op value is rejected.
testBatchUnknownOp :: Test
testBatchUnknownOp = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "op" .= ("delete" :: T.Text)
                     , "path" .= ("Main.hs" :: T.Text)
                     ]
            ])
        ]
  result <- toolExecute previewPatchBatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "unknown op" out
    then pure $ Right ()
    else pure $ Left $ "batch unknown op: " ++ T.unpack (T.take 200 out)

-- | Replace rejects missing file.
testBatchReplaceMissingFile :: Test
testBatchReplaceMissingFile = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "op" .= ("replace" :: T.Text)
                     , "path" .= ("nonexistent.hs" :: T.Text)
                     , "replacement" .= ("new content" :: T.Text)
                     ]
            ])
        ]
  result <- toolExecute previewPatchBatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "could not resolve" out
    then pure $ Right ()
    else pure $ Left $ "batch replace missing: " ++ T.unpack (T.take 200 out)

-- | Create rejects existing file.
testBatchCreateExistingFile :: Test
testBatchCreateExistingFile = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "op" .= ("create" :: T.Text)
                     , "path" .= ("Main.hs" :: T.Text)
                     , "content" .= ("content" :: T.Text)
                     ]
            ])
        ]
  result <- toolExecute previewPatchBatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "already exists" out
    then pure $ Right ()
    else pure $ Left $ "batch create existing: " ++ T.unpack (T.take 200 out)

-- | Create rejects missing parent directory.
testBatchCreateMissingParent :: Test
testBatchCreateMissingParent = runInTestDir "haskode-batch-missing-parent" $ \_root -> do
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "op" .= ("create" :: T.Text)
                     , "path" .= ("nonexistent_dir/New.hs" :: T.Text)
                     , "content" .= ("content" :: T.Text)
                     ]
            ])
        ]
  result <- toolExecute previewPatchBatchTool args
  let out = trOutput result
  if T.isInfixOf "error" out
     && (T.isInfixOf "parent" out || T.isInfixOf "does not exist" out)
    then pure $ Right ()
    else pure $ Left $ "batch create missing parent: " ++ T.unpack (T.take 200 out)

-- | Outside-root path is rejected and the whole batch fails.
testBatchOutsideRoot :: Test
testBatchOutsideRoot = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "op" .= ("replace" :: T.Text)
                     , "path" .= ("C:\\Windows\\System32\\drivers\\etc\\hosts" :: T.Text)
                     , "replacement" .= ("new content" :: T.Text)
                     ]
            ])
        ]
  result <- toolExecute previewPatchBatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "batch outside root: " ++ T.unpack (T.take 200 out)

-- | preview_patch_batch is read-only: files are unchanged after preview.
testBatchReadOnly :: Test
testBatchReadOnly = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-batch-readonly"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  let fileA = root </> "A.hs"
      fileB = root </> "B.hs"
      contentA = "module A where\na = 1\n"
      contentB = "module B where\nb = 1\n"
  TIO.writeFile fileA contentA
  TIO.writeFile fileB contentB
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "op" .= ("replace" :: T.Text)
                     , "path" .= ("A.hs" :: T.Text)
                     , "replacement" .= ("module A where\na = 99\n" :: T.Text)
                     ]
            , object [ "op" .= ("replace" :: T.Text)
                     , "path" .= ("B.hs" :: T.Text)
                     , "replacement" .= ("module B where\nb = 99\n" :: T.Text)
                     ]
            ])
        ]
  _ <- toolExecute previewPatchBatchTool args
  afterA <- TIO.readFile fileA
  afterB <- TIO.readFile fileB
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  if afterA == contentA && afterB == contentB
    then pure $ Right ()
    else pure $ Left "batch preview modified files!"

-- | preview_patch_batch validation failure is also read-only.
testBatchPreviewValidationFailureWritesNothing :: Test
testBatchPreviewValidationFailureWritesNothing = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-batch-preview-validation-failure"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  let fileA = root </> "A.hs"
      originalA = "module A where\na = 1\n"
  TIO.writeFile fileA originalA
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "op" .= ("replace" :: T.Text)
                     , "path" .= ("A.hs" :: T.Text)
                     , "replacement" .= ("module A where\na = 99\n" :: T.Text)
                     ]
            , object [ "op" .= ("replace" :: T.Text)
                     , "path" .= ("Missing.hs" :: T.Text)
                     , "replacement" .= ("module Missing where\n" :: T.Text)
                     ]
            ])
        ]
  result <- toolExecute previewPatchBatchTool args
  afterA <- TIO.readFile fileA
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "error" out && afterA == originalA
    then pure $ Right ()
    else pure $ Left $ "preview validation failure wrote file or missed error: "
                     ++ T.unpack (T.take 200 out)

-- | preview_patch_batch is in the default registry.
testBatchInRegistry :: Test
testBatchInRegistry =
  if "preview_patch_batch" `elem` toolNames defaultRegistry
    then pure $ Right ()
    else pure $ Left $ "preview_patch_batch not in registry: "
                     ++ show (toolNames defaultRegistry)

-- | preview_patch_batch is allowed by default policy (read-only).
testBatchPolicyAllow :: Test
testBatchPolicyAllow =
  let tc = ToolCall "tc-batch" "preview_patch_batch"
             (object ["operations" .= Array (V.fromList [])])
  in case checkPolicy defaultPolicy tc of
       Allow -> pure $ Right ()
       other -> pure $ Left $ "Expected Allow for preview_patch_batch, got: "
                             ++ show other

-- | Missing operations field is rejected.
testBatchMissingOpsField :: Test
testBatchMissingOpsField = do
  let args = object []
  result <- toolExecute previewPatchBatchTool args
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "operations" out
    then pure $ Right ()
    else pure $ Left $ "batch missing ops: " ++ T.unpack out

-- | Malformed operation (missing path) is rejected.
testBatchMissingPath :: Test
testBatchMissingPath = do
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "op" .= ("replace" :: T.Text) ]
            ])
        ]
  result <- toolExecute previewPatchBatchTool args
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "path" out
    then pure $ Right ()
    else pure $ Left $ "batch missing path: " ++ T.unpack out

-- | Malformed operation (missing op) is rejected.
testBatchMissingOp :: Test
testBatchMissingOp = do
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "path" .= ("Main.hs" :: T.Text) ]
            ])
        ]
  result <- toolExecute previewPatchBatchTool args
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "op" out
    then pure $ Right ()
    else pure $ Left $ "batch missing op: " ++ T.unpack out

-- | Replace rejects directory targets.
testBatchReplaceDirectory :: Test
testBatchReplaceDirectory = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "op" .= ("replace" :: T.Text)
                     , "path" .= ("src" :: T.Text)
                     , "replacement" .= ("new content" :: T.Text)
                     ]
            ])
        ]
  result <- toolExecute previewPatchBatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "directory" out
    then pure $ Right ()
    else pure $ Left $ "batch replace directory: " ++ T.unpack (T.take 200 out)


-- ---------------------------------------------------------------------------
-- apply_patch_batch tool tests
-- ---------------------------------------------------------------------------

-- | apply_patch_batch successfully applies multiple replacements.
testBatchApplyTwoReplacements :: Test
testBatchApplyTwoReplacements = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-batch-apply-replace"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  let fileA = root </> "A.hs"
      fileB = root </> "B.hs"
  TIO.writeFile fileA "module A where\na = 1\n"
  TIO.writeFile fileB "module B where\nb = 1\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "op" .= ("replace" :: T.Text)
                     , "path" .= ("A.hs" :: T.Text)
                     , "replacement" .= ("module A where\na = 99\n" :: T.Text)
                     ]
            , object [ "op" .= ("replace" :: T.Text)
                     , "path" .= ("B.hs" :: T.Text)
                     , "replacement" .= ("module B where\nb = 99\n" :: T.Text)
                     ]
            ])
        ]
  result <- toolExecute applyPatchBatchTool args
  setCurrentDirectory origDir
  afterA <- TIO.readFile fileA
  afterB <- TIO.readFile fileB
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "Batch applied" out
     && T.isInfixOf "2 of 2" out
     && T.isInfixOf "Operation 1" out
     && T.isInfixOf "Operation 2" out
     && afterA == "module A where\na = 99\n"
     && afterB == "module B where\nb = 99\n"
    then pure $ Right ()
    else pure $ Left $ "batch apply two replacements: " ++ T.unpack (T.take 400 out)

-- | apply_patch_batch successfully creates a new file.
testBatchApplyCreate :: Test
testBatchApplyCreate = runInTestDir "haskode-batch-apply-create" $ \_root -> do
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "op" .= ("create" :: T.Text)
                     , "path" .= ("New.hs" :: T.Text)
                     , "content" .= ("module New where\nnew = 1\n" :: T.Text)
                     ]
            ])
        ]
  result <- toolExecute applyPatchBatchTool args
  exists <- doesFileExist "New.hs"
  content <- if exists then TIO.readFile "New.hs" else pure ""
  let out = trOutput result
  if T.isInfixOf "Batch applied" out
     && T.isInfixOf "1 of 1" out
     && exists
     && T.isInfixOf "module New where" content
    then pure $ Right ()
    else pure $ Left $ "batch apply create: " ++ T.unpack (T.take 400 out)

-- | apply_patch_batch with mixed replace/create succeeds.
testBatchApplyMixed :: Test
testBatchApplyMixed = withTestDir "haskode-batch-apply-mixed" $ \root -> do
  TIO.writeFile (root </> "Existing.hs") "module Existing where\nexisting = 0\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "op" .= ("replace" :: T.Text)
                     , "path" .= ("Existing.hs" :: T.Text)
                     , "replacement" .= ("module Existing where\nexisting = 1\n" :: T.Text)
                     ]
            , object [ "op" .= ("create" :: T.Text)
                     , "path" .= ("NewFile.hs" :: T.Text)
                     , "content" .= ("module NewFile where\n" :: T.Text)
                     ]
            ])
        ]
  result <- toolExecute applyPatchBatchTool args
  setCurrentDirectory origDir
  afterExisting <- TIO.readFile (root </> "Existing.hs")
  newExists <- doesFileExist (root </> "NewFile.hs")
  newContent <- if newExists then TIO.readFile (root </> "NewFile.hs") else pure ""
  let out = trOutput result
  if T.isInfixOf "Batch applied" out
     && T.isInfixOf "2 of 2" out
     && afterExisting == "module Existing where\nexisting = 1\n"
     && newExists
     && T.isInfixOf "module NewFile where" newContent
    then pure $ Right ()
    else pure $ Left $ "batch apply mixed: " ++ T.unpack (T.take 400 out)

-- | apply_patch_batch with confirmation denial writes nothing.
testBatchApplyDenialWritesNothing :: Test
testBatchApplyDenialWritesNothing = withTestDir "haskode-batch-apply-deny" $ \root -> do
  let original = "module A where\na = 1\n"
  TIO.writeFile (root </> "A.hs") original
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Applying batch."
        , crToolCalls = Just [ToolCall "tc-batch1" "apply_patch_batch"
                               (object [ "operations" .= Array (V.fromList
                                 [ object [ "op" .= ("replace" :: T.Text)
                                          , "path" .= ("A.hs" :: T.Text)
                                          , "replacement" .= ("module A where\na = 99\n" :: T.Text)
                                          ]
                                 ])])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "OK, I won't."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoReject False
  state' <- runAgent state "apply batch"
  setCurrentDirectory origDir
  after <- TIO.readFile (root </> "A.hs")
  let fileUnchanged = after == original
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

-- | apply_patch_batch rejects empty operations.
testBatchApplyEmptyOps :: Test
testBatchApplyEmptyOps = do
  let args = object
        [ "operations" .= Array (V.fromList [])
        ]
  result <- toolExecute applyPatchBatchTool args
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "empty" out
    then pure $ Right ()
    else pure $ Left $ "batch apply empty: " ++ T.unpack out

-- | apply_patch_batch rejects unknown op.
testBatchApplyUnknownOp :: Test
testBatchApplyUnknownOp = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "op" .= ("delete" :: T.Text)
                     , "path" .= ("Main.hs" :: T.Text)
                     ]
            ])
        ]
  result <- toolExecute applyPatchBatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "unknown op" out
    then pure $ Right ()
    else pure $ Left $ "batch apply unknown op: " ++ T.unpack (T.take 200 out)

-- | apply_patch_batch rejects missing replace target.
testBatchApplyReplaceMissing :: Test
testBatchApplyReplaceMissing = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "op" .= ("replace" :: T.Text)
                     , "path" .= ("nonexistent.hs" :: T.Text)
                     , "replacement" .= ("new content" :: T.Text)
                     ]
            ])
        ]
  result <- toolExecute applyPatchBatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "could not resolve" out
    then pure $ Right ()
    else pure $ Left $ "batch apply replace missing: " ++ T.unpack (T.take 200 out)

-- | apply_patch_batch rejects existing create target.
testBatchApplyCreateExisting :: Test
testBatchApplyCreateExisting = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "op" .= ("create" :: T.Text)
                     , "path" .= ("Main.hs" :: T.Text)
                     , "content" .= ("content" :: T.Text)
                     ]
            ])
        ]
  result <- toolExecute applyPatchBatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "already exists" out
    then pure $ Right ()
    else pure $ Left $ "batch apply create existing: " ++ T.unpack (T.take 200 out)

-- | apply_patch_batch rejects missing create parent.
testBatchApplyCreateMissingParent :: Test
testBatchApplyCreateMissingParent = runInTestDir "haskode-batch-apply-missing-parent" $ \_root -> do
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "op" .= ("create" :: T.Text)
                     , "path" .= ("nonexistent_dir/New.hs" :: T.Text)
                     , "content" .= ("content" :: T.Text)
                     ]
            ])
        ]
  result <- toolExecute applyPatchBatchTool args
  let out = trOutput result
  if T.isInfixOf "error" out
     && (T.isInfixOf "parent" out || T.isInfixOf "does not exist" out)
    then pure $ Right ()
    else pure $ Left $ "batch apply create missing parent: " ++ T.unpack (T.take 200 out)

-- | apply_patch_batch rejects outside-root paths.
testBatchApplyOutsideRoot :: Test
testBatchApplyOutsideRoot = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "op" .= ("replace" :: T.Text)
                     , "path" .= ("C:\\Windows\\System32\\drivers\\etc\\hosts" :: T.Text)
                     , "replacement" .= ("new content" :: T.Text)
                     ]
            ])
        ]
  result <- toolExecute applyPatchBatchTool args
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "batch apply outside root: " ++ T.unpack (T.take 200 out)

-- | apply_patch_batch validation is all-or-nothing: if any op fails,
--   no files are written.
testBatchApplyAllOrNothing :: Test
testBatchApplyAllOrNothing = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-batch-apply-all-or-nothing"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  let originalA = "module A where\na = 1\n"
      originalB = "module B where\nb = 1\n"
  TIO.writeFile (root </> "A.hs") originalA
  TIO.writeFile (root </> "B.hs") originalB
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  -- First op succeeds, second fails (nonexistent file)
  let args = object
        [ "operations" .= Array (V.fromList
            [ object [ "op" .= ("replace" :: T.Text)
                     , "path" .= ("A.hs" :: T.Text)
                     , "replacement" .= ("module A where\na = 99\n" :: T.Text)
                     ]
            , object [ "op" .= ("replace" :: T.Text)
                     , "path" .= ("nonexistent.hs" :: T.Text)
                     , "replacement" .= ("new" :: T.Text)
                     ]
            ])
        ]
  result <- toolExecute applyPatchBatchTool args
  setCurrentDirectory origDir
  afterA <- TIO.readFile (root </> "A.hs")
  afterB <- TIO.readFile (root </> "B.hs")
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Validation fails on second op, so NO files should be written
  if T.isInfixOf "error" out
     && afterA == originalA
     && afterB == originalB
    then pure $ Right ()
    else pure $ Left $ "batch all-or-nothing: A changed="
                     ++ show (afterA /= originalA)
                     ++ " B changed=" ++ show (afterB /= originalB)
                     ++ " out=" ++ T.unpack (T.take 200 out)

-- | A mid-batch write failure reports partial success and does not
--   roll back already-written files.
testBatchApplyPartialWriteFailureNoRollback :: Test
testBatchApplyPartialWriteFailureNoRollback = withTestDir "haskode-batch-apply-partial-write" $ \root -> do
  let fileA = root </> "A.hs"
      originalA = "module A where\na = 1\n"
      replacementA = "module A where\na = 2\n"
      missingParentFile = root </> "missing-parent" </> "B.hs"
  TIO.writeFile fileA originalA
  result <- applyValidatedBatchOps
    [ ValidatedBatchOp (ReplaceOp fileA replacementA) ""
    , ValidatedBatchOp (CreateOp missingParentFile "module B where\n") ""
    ]
  afterA <- TIO.readFile fileA
  if T.isInfixOf "Partial batch: 1 of 2" result
     && not (T.isInfixOf "rollback" result)
     && afterA == replacementA
    then pure $ Right ()
    else pure $ Left $ "partial write failure: afterA changed="
                     ++ show (afterA == replacementA)
                     ++ " result=" ++ T.unpack (T.take 300 result)

-- | apply_patch_batch is in the default registry.
testBatchApplyInRegistry :: Test
testBatchApplyInRegistry =
  if "apply_patch_batch" `elem` toolNames defaultRegistry
    then pure $ Right ()
    else pure $ Left $ "apply_patch_batch not in registry: "
                     ++ show (toolNames defaultRegistry)

-- | apply_patch_batch is NOT allowed by default policy (requires confirmation).
testBatchApplyPolicyAskUser :: Test
testBatchApplyPolicyAskUser =
  let tc = ToolCall "tc-batch-ap" "apply_patch_batch"
             (object ["operations" .= Array (V.fromList [])])
  in case checkPolicy defaultPolicy tc of
       AskUser _ -> pure $ Right ()
       other     -> pure $ Left $ "Expected AskUser for apply_patch_batch, got: "
                                 ++ show other

-- | Approved apply_patch_batch logs approval in the session.
testBatchApplyAuditApproval :: Test
testBatchApplyAuditApproval = withTestDir "haskode-batch-apply-audit-approve" $ \root -> do
  TIO.writeFile (root </> "A.hs") "module A where\na = 1\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Applying."
        , crToolCalls = Just [ToolCall "tc-ba1" "apply_patch_batch"
                               (object [ "operations" .= Array (V.fromList
                                 [ object [ "op" .= ("replace" :: T.Text)
                                          , "path" .= ("A.hs" :: T.Text)
                                          , "replacement" .= ("module A where\na = 2\n" :: T.Text)
                                          ]
                                 ])])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove False
  state' <- runAgent state "apply batch"
  setCurrentDirectory origDir
  let evts = events (asSession state')
      policyEvts = filter (\e -> evType e == EPolicyDecision) evts
      approvalEvts = filter (T.isInfixOf "approved" . evData) policyEvts
  case approvalEvts of
    (_:_) -> pure $ Right ()
    _     -> pure $ Left "No approval event found for apply_patch_batch"

-- | Rejected apply_patch_batch logs rejection in the session.
testBatchApplyAuditRejection :: Test
testBatchApplyAuditRejection = withTestDir "haskode-batch-apply-audit-reject" $ \root -> do
  TIO.writeFile (root </> "A.hs") "module A where\na = 1\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Applying."
        , crToolCalls = Just [ToolCall "tc-ba2" "apply_patch_batch"
                               (object [ "operations" .= Array (V.fromList
                                 [ object [ "op" .= ("replace" :: T.Text)
                                          , "path" .= ("A.hs" :: T.Text)
                                          , "replacement" .= ("module A where\na = 99\n" :: T.Text)
                                          ]
                                 ])])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "OK."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoReject False
  state' <- runAgent state "apply batch"
  setCurrentDirectory origDir
  let evts = events (asSession state')
      trEvts = filter (\e -> evType e == EToolResult) evts
      denialEvts = filter (T.isInfixOf "denied by user" . evData) trEvts
  case denialEvts of
    (_:_) -> pure $ Right ()
    _     -> pure $ Left "No denial event found for apply_patch_batch"

-- ---------------------------------------------------------------------------
-- Format tests
-- ---------------------------------------------------------------------------

-- | formatBatchHeader produces a concise header with operation count.
testFormatBatchHeader :: Test
testFormatBatchHeader =
  let out = formatBatchHeader 3
  in if T.isInfixOf "3 operations" out && T.isInfixOf "Batch preview" out
        && T.isInfixOf "──────" out
     then pure $ Right ()
     else pure $ Left $ "formatBatchHeader: " ++ T.unpack out

-- ---------------------------------------------------------------------------
-- Diff display formatting tests
-- ---------------------------------------------------------------------------

-- | showDiff includes a hunk header with added/removed line counts.
testShowDiffHunkHeader :: Test
testShowDiffHunkHeader =
  let patch = makePatch "Foo.hs" "module Foo where\nfoo = 1\n" "module Foo where\nfoo = 2\nbar = 3\n"
      diff  = showDiff patch
  in if T.isInfixOf "@@ -2 +3 @@" diff
       then pure $ Right ()
       else pure $ Left $ "showDiff hunk header: " ++ T.unpack (T.take 200 diff)

-- | showDiff line counts are correct for a removal-heavy diff.
testShowDiffLineCountsRemoved :: Test
testShowDiffLineCountsRemoved =
  let patch = makePatch "Bar.hs" "a\nb\nc\nd\n" "a\nc\n"
      diff  = showDiff patch
  in if T.isInfixOf "@@ -4 +2 @@" diff
       then pure $ Right ()
       else pure $ Left $ "showDiff removal counts: " ++ T.unpack (T.take 200 diff)

-- | countAddedLines correctly counts + lines in a diff.
testCountAddedLines :: Test
testCountAddedLines =
  let diff = "--- Foo.hs\n+++ Foo.hs\n@@ -1 +2 @@\n-old1\n-old2\n+new1\n+new2\n+new3\n"
  in if countAddedLines diff == 3
       then pure $ Right ()
       else pure $ Left $ "countAddedLines: expected 3, got " ++ show (countAddedLines diff)

-- | countRemovedLines correctly counts - lines in a diff.
testCountRemovedLines :: Test
testCountRemovedLines =
  let diff = "--- Foo.hs\n+++ Foo.hs\n@@ -2 +1 @@\n-old1\n-old2\n+new1\n"
  in if countRemovedLines diff == 2
       then pure $ Right ()
       else pure $ Left $ "countRemovedLines: expected 2, got " ++ show (countRemovedLines diff)

-- | batchOpPreview includes operation metadata and separator.
testBatchOpPreviewMetadata :: Test
testBatchOpPreviewMetadata =
  let diff = "--- Foo.hs\n+++ Foo.hs\n@@ -1 +1 @@\n-old\n+new\n"
      preview = batchOpPreview 1 (ReplaceOp "Foo.hs" "new") diff
  in if T.isInfixOf "(1 added, 1 removed)" preview
       && T.isInfixOf "─────────────" preview
       && T.isInfixOf "Operation 1" preview
       && T.isInfixOf "Foo.hs" preview
       then pure $ Right ()
       else pure $ Left $ "batchOpPreview metadata: " ++ T.unpack (T.take 300 preview)

-- | batchOpPreview for create operation shows added count.
testBatchOpPreviewCreateMetadata :: Test
testBatchOpPreviewCreateMetadata =
  let preview = "--- (new file)\n+++ New.hs\n@@ -0,0 +1,2 @@\n+line1\n+line2\n"
      result  = batchOpPreview 2 (CreateOp "New.hs" "line1\nline2\n") preview
  in if T.isInfixOf "(2 added)" result
       && T.isInfixOf "Operation 2" result
       && T.isInfixOf "New.hs" result
       && T.isInfixOf "─────────────" result
       then pure $ Right ()
       else pure $ Left $ "batchOpPreview create: " ++ T.unpack (T.take 300 result)

-- ---------------------------------------------------------------------------
-- Pure batch formatting helper tests
-- ---------------------------------------------------------------------------

-- | formatBatchApplySummary shows the operation count and numbered list.
testFormatBatchApplySummary :: Test
testFormatBatchApplySummary =
  let ops = [ ReplaceOp "src/Foo.hs" "new content"
            , ReplaceOp "src/Bar.hs" "other"
            , CreateOp  "src/Baz.hs" "baz content"
            ]
      out = formatBatchApplySummary ops
  in if T.isInfixOf "3 operations" out
       && T.isInfixOf "1. replace | src/Foo.hs" out
       && T.isInfixOf "2. replace | src/Bar.hs" out
       && T.isInfixOf "3. create | src/Baz.hs" out
       then pure $ Right ()
       else pure $ Left $ "formatBatchApplySummary: " ++ T.unpack out

-- | formatBatchApplySummary with a single replace operation.
testFormatBatchApplySummarySingleReplace :: Test
testFormatBatchApplySummarySingleReplace =
  let ops = [ReplaceOp "Main.hs" "content"]
      out = formatBatchApplySummary ops
  in if T.isInfixOf "1 operations" out
       && T.isInfixOf "1. replace | Main.hs" out
       then pure $ Right ()
       else pure $ Left $ "formatBatchApplySummary single: " ++ T.unpack out

-- | formatBatchApplySummary with a single create operation.
testFormatBatchApplySummarySingleCreate :: Test
testFormatBatchApplySummarySingleCreate =
  let ops = [CreateOp "New.hs" "content"]
      out = formatBatchApplySummary ops
  in if T.isInfixOf "1 operations" out
       && T.isInfixOf "1. create | New.hs" out
       then pure $ Right ()
       else pure $ Left $ "formatBatchApplySummary create: " ++ T.unpack out

-- | formatBatchApplySummary with empty list produces valid output.
testFormatBatchApplySummaryEmpty :: Test
testFormatBatchApplySummaryEmpty =
  let out = formatBatchApplySummary []
  in if T.isInfixOf "0 operations" out
       then pure $ Right ()
       else pure $ Left $ "formatBatchApplySummary empty: " ++ T.unpack out

-- | formatDiffCountSummary shows added and removed counts.
testFormatDiffCountSummaryBoth :: Test
testFormatDiffCountSummaryBoth =
  let diff = "--- Foo.hs\n+++ Foo.hs\n-old\n+new1\n+new2\n"
      out = formatDiffCountSummary diff
  in if T.isInfixOf "2 added" out && T.isInfixOf "1 removed" out
       then pure $ Right ()
       else pure $ Left $ "formatDiffCountSummary both: " ++ T.unpack out

-- | formatDiffCountSummary with only additions omits removed.
testFormatDiffCountSummaryAddedOnly :: Test
testFormatDiffCountSummaryAddedOnly =
  let diff = "--- (new file)\n+++ New.hs\n+line1\n+line2\n+line3\n"
      out = formatDiffCountSummary diff
  in if T.isInfixOf "3 added" out && not (T.isInfixOf "removed" out)
       then pure $ Right ()
       else pure $ Left $ "formatDiffCountSummary added only: " ++ T.unpack out

-- | formatNewFilePreview produces a new-file diff with added lines.
testFormatNewFilePreviewPure :: Test
testFormatNewFilePreviewPure =
  let out = formatNewFilePreview "New.hs" "module New where\nimport Data.List\n"
  in if T.isInfixOf "(new file)" out
       && T.isInfixOf "+++ New.hs" out
       && T.isInfixOf "+module New where" out
       && T.isInfixOf "+import Data.List" out
       && T.isInfixOf "@@ -0,0 +1,2 @@" out
       then pure $ Right ()
       else pure $ Left $ "formatNewFilePreview: " ++ T.unpack (T.take 300 out)

-- ---------------------------------------------------------------------------
-- Colored diff rendering tests
-- ---------------------------------------------------------------------------

-- | colorizeUnifiedDiff adds ANSI codes to added lines.
testColorizeAddedLine :: Test
testColorizeAddedLine =
  let colored = colorizeUnifiedDiff "+new line\n"
  in if T.isInfixOf "\ESC[32m" colored && T.isInfixOf "+new line" colored
        && T.isInfixOf "\ESC[0m" colored
       then pure $ Right ()
       else pure $ Left $ "colorize added: " ++ T.unpack colored

-- | colorizeUnifiedDiff adds ANSI codes to removed lines.
testColorizeRemovedLine :: Test
testColorizeRemovedLine =
  let colored = colorizeUnifiedDiff "-old line\n"
  in if T.isInfixOf "\ESC[31m" colored && T.isInfixOf "-old line" colored
        && T.isInfixOf "\ESC[0m" colored
       then pure $ Right ()
       else pure $ Left $ "colorize removed: " ++ T.unpack colored

-- | colorizeUnifiedDiff adds ANSI codes to hunk headers.
testColorizeHunkHeader :: Test
testColorizeHunkHeader =
  let colored = colorizeUnifiedDiff "@@ -1 +1 @@\n"
  in if T.isInfixOf "\ESC[1;36m" colored && T.isInfixOf "@@ -1 +1 @@" colored
        && T.isInfixOf "\ESC[0m" colored
       then pure $ Right ()
       else pure $ Left $ "colorize hunk: " ++ T.unpack colored

-- | colorizeUnifiedDiff colors --- file header as bold red, not plain red.
testColorizeFileHeaderDash :: Test
testColorizeFileHeaderDash =
  let colored = colorizeUnifiedDiff "--- Foo.hs\n"
  in if T.isInfixOf "\ESC[1;31m" colored && T.isInfixOf "--- Foo.hs" colored
        && T.isInfixOf "\ESC[0m" colored
       then pure $ Right ()
       else pure $ Left $ "colorize --- header: " ++ T.unpack colored

-- | colorizeUnifiedDiff colors +++ file header as bold green, not plain green.
testColorizeFileHeaderPlus :: Test
testColorizeFileHeaderPlus =
  let colored = colorizeUnifiedDiff "+++ Foo.hs\n"
  in if T.isInfixOf "\ESC[1;32m" colored && T.isInfixOf "+++ Foo.hs" colored
        && T.isInfixOf "\ESC[0m" colored
       then pure $ Right ()
       else pure $ Left $ "colorize +++ header: " ++ T.unpack colored

-- | colorizeUnifiedDiff leaves context lines uncolored.
testColorizeContextLine :: Test
testColorizeContextLine =
  let colored = colorizeUnifiedDiff "  context line\n"
  in if colored == "  context line\n"
       then pure $ Right ()
       else pure $ Left $ "colorize context: " ++ T.unpack colored

-- | colorizeUnifiedDiff does not confuse --- file header with a removed line.
testColorizeDashDashDashNotRemoved :: Test
testColorizeDashDashDashNotRemoved =
  let colored = colorizeUnifiedDiff "--- Foo.hs\n"
  in if T.isInfixOf "\ESC[1;31m" colored
       then pure $ Right ()
       else pure $ Left $ "colorize --- treated as removed: " ++ T.unpack colored

-- | colorizeUnifiedDiff does not confuse +++ file header with an added line.
testColorizePlusPlusPlusNotAdded :: Test
testColorizePlusPlusPlusNotAdded =
  let colored = colorizeUnifiedDiff "+++ Foo.hs\n"
  in if T.isInfixOf "\ESC[1;32m" colored
       then pure $ Right ()
       else pure $ Left $ "colorize +++ treated as added: " ++ T.unpack colored

-- | colorizeUnifiedDiff on a full diff adds codes to all expected lines.
testColorizeFullDiff :: Test
testColorizeFullDiff =
  let diff = showDiff (makePatch "Foo.hs" "a\nb\n" "a\nc\n")
      colored = colorizeUnifiedDiff diff
  in if T.isInfixOf "\ESC[1;31m" colored   -- --- header
        && T.isInfixOf "\ESC[1;32m" colored  -- +++ header
        && T.isInfixOf "\ESC[1;36m" colored  -- @@ hunk
        && T.isInfixOf "\ESC[31m" colored    -- -b
        && T.isInfixOf "\ESC[32m" colored    -- +c
       then pure $ Right ()
       else pure $ Left $ "colorize full diff: " ++ T.unpack (T.take 300 colored)

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
  , testBatchTwoReplacements
  , testBatchCreatePreview
  , testBatchMixedPreview
  , testBatchEmptyOps
  , testBatchUnknownOp
  , testBatchReplaceMissingFile
  , testBatchCreateExistingFile
  , testBatchCreateMissingParent
  , skipOnWindows testBatchOutsideRoot
  , testBatchReadOnly
  , testBatchPreviewValidationFailureWritesNothing
  , testBatchInRegistry
  , testBatchPolicyAllow
  , testBatchMissingOpsField
  , testBatchMissingPath
  , testBatchMissingOp
  , testBatchReplaceDirectory
  , testBatchApplyTwoReplacements
  , testBatchApplyCreate
  , testBatchApplyMixed
  , testBatchApplyDenialWritesNothing
  , testBatchApplyEmptyOps
  , testBatchApplyUnknownOp
  , testBatchApplyReplaceMissing
  , testBatchApplyCreateExisting
  , testBatchApplyCreateMissingParent
  , skipOnWindows testBatchApplyOutsideRoot
  , testBatchApplyAllOrNothing
  , testBatchApplyPartialWriteFailureNoRollback
  , testBatchApplyInRegistry
  , testBatchApplyPolicyAskUser
  , testBatchApplyAuditApproval
  , testBatchApplyAuditRejection
  , testFormatBatchHeader
  , testShowDiffHunkHeader
  , testShowDiffLineCountsRemoved
  , testCountAddedLines
  , testCountRemovedLines
  , testBatchOpPreviewMetadata
  , testBatchOpPreviewCreateMetadata
  , testFormatBatchApplySummary
  , testFormatBatchApplySummarySingleReplace
  , testFormatBatchApplySummarySingleCreate
  , testFormatBatchApplySummaryEmpty
  , testFormatDiffCountSummaryBoth
  , testFormatDiffCountSummaryAddedOnly
  , testFormatNewFilePreviewPure
  , testColorizeAddedLine
  , testColorizeRemovedLine
  , testColorizeHunkHeader
  , testColorizeFileHeaderDash
  , testColorizeFileHeaderPlus
  , testColorizeContextLine
  , testColorizeDashDashDashNotRemoved
  , testColorizePlusPlusPlusNotAdded
  , testColorizeFullDiff
  ]
