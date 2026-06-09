{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Patch manager.
--
-- When the LLM proposes file changes, we represent them as 'Patch'
-- values.  A patch captures:
--
--   * The file path
--   * The old content (for diffing and rollback)
--   * The new content
--
-- The patch manager can:
--   * Show a unified diff to the user
--   * Apply a patch (write the new content)
--   * Roll back a patch (restore the old content)
--
-- This is intentionally simple.  A future version may use a real
-- diff/patch library (e.g. @diff@ or @patience@).

module Haskode.Patch
  ( -- * Patch type
    Patch (..)
    -- * Operations
  , makePatch
  , applyPatch
  , rollbackPatch
    -- * Diff helpers
  , showDiff
  , countDiffLines
  , countAddedLines
  , countRemovedLines
  , formatDiffCountSummary
    -- * Batch operation type
  , BatchOp (..)
  , ValidatedBatchOp (..)
  , parseBatchOps
  , validateBatchOps
    -- * Diff limits
  , previewDiffLimit
  , batchPreviewLimit
    -- * Batch preview helpers
  , batchOpPreview
  , formatBatchHeader
  , formatNewFilePreview
    -- * Batch apply helpers
  , applyBatchOp
  , applyValidatedBatchOps
  , formatBatchApplySummary
    -- * Path safety helpers
  , safeCanonicalize
  , isUnderRoot
  , takeParentDir
    -- * JSON helpers
  , extractTextField
  ) where

import Control.Exception (IOException, try)
import Data.Aeson       (Value (..))
import qualified Data.Aeson.Key    as Key
import qualified Data.Aeson.KeyMap as KM
import Data.List        (isPrefixOf)
import Data.Text        (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (canonicalizePath, doesDirectoryExist, doesPathExist)
import System.IO        (hPutStrLn, stderr)

-- ---------------------------------------------------------------------------
-- Patch
-- ---------------------------------------------------------------------------

-- | A proposed or applied change to a single file.
data Patch = Patch
  { patchPath     :: !FilePath  -- ^ File being modified
  , patchOld      :: !Text      -- ^ Content before the change
  , patchNew      :: !Text      -- ^ Content after the change
  , patchApplied  :: !Bool      -- ^ Whether the patch has been applied
  } deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Operations
-- ---------------------------------------------------------------------------

-- | Create a patch from old and new content.
makePatch :: FilePath -> Text -> Text -> Patch
makePatch path old new = Patch
  { patchPath    = path
  , patchOld     = old
  , patchNew     = new
  , patchApplied = False
  }

-- | Apply a patch by writing the new content to disk.
--   Returns the patch with 'patchApplied' set to True.
applyPatch :: Patch -> IO Patch
applyPatch p = do
  writeFile (patchPath p) (T.unpack (patchNew p))
  hPutStrLn stderr $ "Applied patch: " ++ patchPath p
  pure p { patchApplied = True }

-- | Roll back a patch by restoring the old content.
rollbackPatch :: Patch -> IO Patch
rollbackPatch p = do
  writeFile (patchPath p) (T.unpack (patchOld p))
  hPutStrLn stderr $ "Rolled back patch: " ++ patchPath p
  pure p { patchApplied = False }

-- | Count the number of newline-terminated lines in a text value.
--   A trailing newline is not required; the last line is counted
--   even without one.
countDiffLines :: Text -> Int
countDiffLines t
  | T.null t  = 0
  | otherwise = T.count "\n" t + if T.isSuffixOf "\n" t then 0 else 1

-- | Show a simple unified-style diff (line-based) with a hunk header
--   summarising added and removed line counts.
--   This is a placeholder; a real implementation would use a proper
--   diff algorithm.
showDiff :: Patch -> Text
showDiff p = T.unlines $
  [ "--- " <> T.pack (patchPath p)
  , "+++ " <> T.pack (patchPath p)
  , "@@ -" <> T.pack (show removedCount) <> " +" <> T.pack (show addedCount) <> " @@"
  ] ++ map ("-" <>) oldLines
    ++ map ("+" <>) newLines
  where
    oldLines     = T.lines (patchOld p)
    newLines     = T.lines (patchNew p)
    removedCount = countDiffLines (patchOld p)
    addedCount   = countDiffLines (patchNew p)

-- ---------------------------------------------------------------------------
-- Path safety helpers (internal)
-- ---------------------------------------------------------------------------

-- | Safely canonicalize a path.  Returns 'Nothing' on failure
-- (broken symlink, permission error, missing path, etc.).
--   After canonicalization, verifies the resolved path actually exists
--   so that broken symlinks (whose targets are missing) are caught.
safeCanonicalize :: FilePath -> IO (Maybe FilePath)
safeCanonicalize path = do
  result <- try (canonicalizePath path) :: IO (Either IOException FilePath)
  case result of
    Left _  -> pure Nothing
    Right p -> do
      exists <- doesPathExist p
      if exists then pure (Just p) else pure Nothing

-- | Check whether @path@ is the same as, or a subdirectory of, @root@.
--   Both paths must already be canonicalized.
--   Handles both @/@ and @\\@ separators for Windows compatibility.
isUnderRoot :: FilePath -> FilePath -> Bool
isUnderRoot root path =
  root == path
  || (root ++ "/")  `isPrefixOf` path
  || (root ++ "\\") `isPrefixOf` path

-- | Take the parent directory of a path.
--   Returns "." if the path has no directory component.
takeParentDir :: FilePath -> FilePath
takeParentDir path = case reverse (splitPath path) of
  (_:rest) -> if null rest then "." else joinPath (reverse rest)
  []       -> "."
  where
    splitPath [] = [""]
    splitPath p  = case break (== '/') p of
      (chunk, [])   -> [chunk]
      (chunk, _:rest) -> chunk : splitPath rest
    joinPath []     = "."
    joinPath chunks = foldl1 (\a b -> a ++ "/" ++ b) chunks

-- ---------------------------------------------------------------------------
-- Batch operations
-- ---------------------------------------------------------------------------

-- | A single parsed batch operation.
data BatchOp
  = ReplaceOp !FilePath !Text   -- ^ path, replacement content
  | CreateOp  !FilePath !Text   -- ^ path, file content
  deriving stock (Show, Eq)

-- | A batch operation after all path, parent, content-read, and
--   preview-size validation has succeeded.
data ValidatedBatchOp = ValidatedBatchOp
  { validatedBatchOp      :: !BatchOp
  , validatedBatchPreview :: !Text
  } deriving stock (Show, Eq)

-- | Extract a text field from a JSON object, returning 'Nothing' if
--   the field is missing or not a string.
extractTextField :: Text -> Value -> Maybe Text
extractTextField key (Object o) =
  case KM.lookup (Key.fromText key) o of
    Just (String s) -> Just s
    _               -> Nothing
extractTextField _ _ = Nothing

-- | Parse a list of JSON values into 'BatchOp' values.
--   Returns @Left (index, error)@ on the first failure.
parseBatchOps :: [Value] -> Either (Int, Text) [BatchOp]
parseBatchOps = go 1
  where
    go _ []     = Right []
    go n (v:vs) = case parseOne n v of
      Left err   -> Left err
      Right op   -> (op :) <$> go (n + 1) vs

    parseOne n v = case extractTextField "op" v of
      Nothing   -> Left (n, "missing 'op' field")
      Just tag  -> case extractTextField "path" v of
        Nothing -> Left (n, "missing 'path' field")
        Just p  -> let path = T.unpack p in case tag of
          "replace" -> Right $ ReplaceOp path
            (maybe "" id (extractTextField "replacement" v))
          "create"  -> Right $ CreateOp path
            (maybe "" id (extractTextField "content" v))
          other     -> Left (n, "unknown op '" <> other <> "'")

-- | Validate every batch operation before any write happens.
--   Replace operations must target existing regular files under the
--   working directory.  Create operations must target nonexistent
--   paths whose parent directory already exists under the working
--   directory.  The returned preview text is the same text used by
--   read-only batch preview and confirmation display.
validateBatchOps :: FilePath -> [BatchOp] -> IO (Either Text [ValidatedBatchOp])
validateBatchOps rootCanon = go 1 []
  where
    go :: Int -> [ValidatedBatchOp] -> [BatchOp] -> IO (Either Text [ValidatedBatchOp])
    go _ acc [] = pure $ Right (reverse acc)
    go n acc (op:rest) = do
      result <- validateOne n op
      case result of
        Left msg        -> pure $ Left msg
        Right validated -> go (n + 1) (validated : acc) rest

    validateOne :: Int -> BatchOp -> IO (Either Text ValidatedBatchOp)
    validateOne n (ReplaceOp path replacement) = do
      pathCanonResult <- safeCanonicalize path
      case pathCanonResult of
        Nothing -> opErr n $ "could not resolve path: " <> T.pack path
        Just pathCanon
          | not (isUnderRoot rootCanon pathCanon) ->
            opErr n $ "path must be under the working directory: " <> T.pack path
          | otherwise -> do
            isDir <- doesDirectoryExist path
            if isDir
              then opErr n $ "path is a directory, not a file: " <> T.pack path
              else do
                readResult <- try (TIO.readFile path) :: IO (Either IOException Text)
                case readResult of
                  Left e -> opErr n $ "error reading " <> T.pack path
                    <> ": " <> T.pack (show e)
                  Right current -> do
                    let op = ReplaceOp path replacement
                        patch = makePatch path current replacement
                        diff = showDiff patch
                    if T.length diff > previewDiffLimit
                      then opErr n $ "diff too large ("
                        <> T.pack (show (T.length diff))
                        <> " chars, limit "
                        <> T.pack (show previewDiffLimit)
                        <> "): " <> T.pack path
                      else pure $ Right (ValidatedBatchOp op diff)

    validateOne n (CreateOp path content) = do
      pathExists <- doesPathExist path
      if pathExists
        then opErr n $ "file already exists, cannot create: " <> T.pack path
        else do
          let parentDir = takeParentDir path
          parentCanonResult <- safeCanonicalize parentDir
          case parentCanonResult of
            Nothing -> opErr n $ "parent directory does not exist: "
              <> T.pack parentDir
            Just parentCanon
              | not (isUnderRoot rootCanon parentCanon) ->
                opErr n $ "path must be under the working directory: "
                  <> T.pack path
              | otherwise -> do
                parentIsDir <- doesDirectoryExist parentDir
                if not parentIsDir
                  then opErr n $ "parent path is not a directory: "
                    <> T.pack parentDir
                  else do
                    let op = CreateOp path content
                        preview = formatNewFilePreview path content
                    if T.length preview > previewDiffLimit
                      then opErr n $ "preview too large for: " <> T.pack path
                      else pure $ Right (ValidatedBatchOp op preview)

    opErr :: Int -> Text -> IO (Either Text ValidatedBatchOp)
    opErr n msg = pure $ Left
      ("error in operation " <> T.pack (show n) <> ": " <> msg)

-- ---------------------------------------------------------------------------
-- Diff limits
-- ---------------------------------------------------------------------------

-- | Maximum character count for a single preview diff.  Diffs larger
--   than this are refused so the output does not flood model context.
previewDiffLimit :: Int
previewDiffLimit = 8192

-- | Maximum combined preview size (in chars) for a batch preview.
--   Individual file diffs are still bounded by 'previewDiffLimit'.
batchPreviewLimit :: Int
batchPreviewLimit = 32768

-- ---------------------------------------------------------------------------
-- Batch preview helpers
-- ---------------------------------------------------------------------------

-- | Count lines in a unified diff that are additions (start with @+@
--   but not @++@).
countAddedLines :: Text -> Int
countAddedLines t = length
  [ () | l <- T.lines t
       , T.isPrefixOf "+" l
       , not (T.isPrefixOf "+++" l)
       , not (T.isPrefixOf "@@" l)
  ]

-- | Count lines in a unified diff that are removals (start with @-@
--   but not @--@).
countRemovedLines :: Text -> Int
countRemovedLines t = length
  [ () | l <- T.lines t
       , T.isPrefixOf "-" l
       , not (T.isPrefixOf "---" l)
       , not (T.isPrefixOf "@@" l)
  ]

-- | Format a concise diff-count summary for tool output.
--   Returns e.g. @\"(3 added, 2 removed)\"@ for a replacement diff
--   or @\"(5 added)\"@ when there are no removals (e.g. new-file previews).
formatDiffCountSummary :: Text -> Text
formatDiffCountSummary diff =
  let added   = countAddedLines diff
      removed = countRemovedLines diff
  in if removed > 0
       then "(" <> T.pack (show added) <> " added, "
            <> T.pack (show removed) <> " removed)"
       else "(" <> T.pack (show added) <> " added)"

-- | Format the per-operation header and diff for a batch preview.
--   Takes the 1-based operation index, the 'BatchOp', and the diff
--   text (as returned by 'batchOpDiff').  Includes an operation
--   metadata line with line-count summaries and a visual separator.
--
--   The separator uses a Unicode box-drawing horizontal line
--   (@─────────────@) for cosmetic framing.  This is intentional;
--   on Windows consoles without UTF-8 support it may render as
--   fallback glyphs but the surrounding text remains readable.
batchOpPreview :: Int -> BatchOp -> Text -> Text
batchOpPreview n (ReplaceOp path _) diff =
  "--- Operation " <> T.pack (show n) <> " (replace): " <> T.pack path
     <> " " <> formatDiffCountSummary diff <> "\n"
     <> diff
     <> "─────────────\n"
batchOpPreview n (CreateOp path _) preview =
  "--- Operation " <> T.pack (show n) <> " (create): " <> T.pack path
     <> " " <> formatDiffCountSummary preview <> "\n"
     <> preview
     <> "─────────────\n"

-- | Format the top-level batch result header.
--   Uses a Unicode box-drawing horizontal line (@─────────────@)
--   for cosmetic framing (same as 'batchOpPreview').
formatBatchHeader :: Int -> Text
formatBatchHeader count =
  "Batch preview (" <> T.pack (show count) <> " operations):\n"
  <> "─────────────"

formatNewFilePreview :: FilePath -> Text -> Text
formatNewFilePreview path content =
  let nAdded = length (T.lines content)
  in "--- (new file)\n+++ " <> T.pack path <> "\n"
     <> "@@ -0,0 +1," <> T.pack (show nAdded) <> " @@\n"
     <> T.unlines (map ("+" <>) (T.lines content))

-- ---------------------------------------------------------------------------
-- Batch apply helpers
-- ---------------------------------------------------------------------------

-- | Apply a single 'BatchOp' to disk.  Does NOT validate; the caller
--   must validate before calling this function.
--   Returns 'Right' on success, 'Left' with an error message on failure.
applyBatchOp :: BatchOp -> IO (Either Text ())
applyBatchOp (ReplaceOp path replacement) = do
  result <- try (writeFile path (T.unpack replacement)) :: IO (Either IOException ())
  case result of
    Left e  -> pure $ Left ("error writing " <> T.pack path <> ": " <> T.pack (show e))
    Right _ -> pure $ Right ()
applyBatchOp (CreateOp path content) = do
  result <- try (writeFile path (T.unpack content)) :: IO (Either IOException ())
  case result of
    Left e  -> pure $ Left ("error writing " <> T.pack path <> ": " <> T.pack (show e))
    Right _ -> pure $ Right ()

-- | Format a concise per-operation summary for batch-apply confirmation
--   preview.  Shows the operation count and a numbered list of op type
--   and path.
formatBatchApplySummary :: [BatchOp] -> Text
formatBatchApplySummary ops =
  "Batch: " <> T.pack (show (length ops)) <> " operations\n"
  <> T.unlines (map formatOne (zip [1..] ops))
  where
    formatOne (n, ReplaceOp path _) =
      T.pack (show (n :: Int)) <> ". replace | " <> T.pack path
    formatOne (n, CreateOp path _) =
      T.pack (show (n :: Int)) <> ". create | " <> T.pack path

-- | Apply validated operations in order.
--   If a write fails mid-batch, stops and reports partial success.
--   Already-written files are not rolled back.
--   Returns a summary text for the tool result.
applyValidatedBatchOps :: [ValidatedBatchOp] -> IO Text
applyValidatedBatchOps ops = go 0 (length ops) ops
  where
    go :: Int -> Int -> [ValidatedBatchOp] -> IO Text
    go applied _total [] =
      pure $ "Batch applied (" <> T.pack (show applied) <> " of "
           <> T.pack (show applied) <> " operations succeeded)"
    go applied total (op:rest) = do
      writeResult <- applyBatchOp (validatedBatchOp op)
      case writeResult of
        Left err -> pure $
          "Partial batch: " <> T.pack (show applied) <> " of "
          <> T.pack (show total) <> " operations applied before error: " <> err
        Right () -> go (applied + 1) total rest
