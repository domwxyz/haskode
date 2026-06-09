{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Concrete file-editing tool implementations.
--
-- This module keeps patch/write/batch implementation details out of
-- "Haskode.Tools". It exports small tool specs instead of the public
-- 'Haskode.Tools.Tool' type so the registry module can remain the
-- public surface without creating an import cycle.

module Haskode.Tools.FileEdit
  ( ToolSpec (..)
  , previewPatchToolSpec
  , previewPatchBatchToolSpec
  , applyPatchToolSpec
  , applyPatchBatchToolSpec
  , writeFileToolSpec
  , computePatchPreview
  , computeWriteFilePreview
  , computeBatchApplyPreview
  ) where

import Control.Exception (IOException, try)
import Data.Aeson        (Value (..), object, (.=))
import qualified Data.Aeson.Key    as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text         (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Vector as V (toList)
import System.Directory  (doesDirectoryExist, doesPathExist)

import Haskode.Core (ToolResult (..))
import Haskode.Patch
    ( Patch
    , applyPatch, makePatch, showDiff
    , ValidatedBatchOp (..), parseBatchOps
    , previewDiffLimit, batchPreviewLimit
    , batchOpPreview, formatBatchHeader
    , formatDiffCountSummary
    , formatNewFilePreview
    , formatBatchApplySummary
    , validateBatchOps, applyValidatedBatchOps
    , safeCanonicalize, isUnderRoot, takeParentDir
    , extractTextField
    )

data ToolSpec = ToolSpec
  { specName        :: !Text
  , specDescription :: !Text
  , specSchema      :: !Value
  , specExecute     :: Value -> IO ToolResult
  }

data ValidatedBatch = ValidatedBatch
  { vbOperations :: ![ValidatedBatchOp]
  , vbParts      :: ![Text]
  , vbCombined   :: !Text
  }

missingOperationsError :: Text
missingOperationsError = "missing or invalid 'operations' field"

formatBatchToolError :: Text -> Text
formatBatchToolError msg
  | msg == missingOperationsError =
    "error: " <> msg <> " (expected array)"
  | T.isPrefixOf "error " msg = msg
  | otherwise = "error: " <> msg

withRootContainment :: FilePath -> (FilePath -> FilePath -> IO ToolResult) -> IO ToolResult
withRootContainment path k = do
  rootCanonResult <- safeCanonicalize "."
  case rootCanonResult of
    Nothing -> pure $ ToolResult ""
      "error: could not resolve working directory"
    Just rootCanon -> do
      pathCanonResult <- safeCanonicalize path
      case pathCanonResult of
        Nothing -> pure $ ToolResult ""
          ("error: could not resolve path: " <> T.pack path)
        Just pathCanon
          | not (isUnderRoot rootCanon pathCanon) ->
            pure $ ToolResult ""
              ("error: path must be under the working directory: "
               <> T.pack path)
          | otherwise -> k rootCanon pathCanon

previewPatchToolSpec :: ToolSpec
previewPatchToolSpec = ToolSpec
  { specName        = "preview_patch"
  , specDescription = "Preview a unified diff for a proposed file replacement without modifying the filesystem. Reads the current file and shows what would change. Use this before apply_patch to review changes. Does NOT write to disk."
  , specSchema      = object
      [ "type"       .= ("object" :: Text)
      , "properties" .= object
          [ "path" .= object
              [ "type"        .= ("string" :: Text)
              , "description" .= ("Path to the existing file" :: Text)
              ]
          , "replacement" .= object
              [ "type"        .= ("string" :: Text)
              , "description" .= ("Proposed new content for the file" :: Text)
              ]
          ]
      , "required"   .= (["path", "replacement"] :: [Text])
      ]
  , specExecute = \args -> do
      let path = case extractTextField "path" args of
            Just p  -> T.unpack p
            Nothing -> ""
          replacement = case extractTextField "replacement" args of
            Just r  -> r
            Nothing -> ""
      if null path
        then pure $ ToolResult "" "error: missing required field 'path'"
        else withRootContainment path $ \_rootCanon _pathCanon -> do
          readResult <- try (TIO.readFile path) :: IO (Either IOException Text)
          case readResult of
            Left e -> pure $ ToolResult ""
              ("error reading " <> T.pack path <> ": " <> T.pack (show e))
            Right current -> do
              let patch = makePatch path current replacement
                  diff  = showDiff patch
              if T.length diff > previewDiffLimit
                then pure $ ToolResult ""
                  ("error: diff too large ("
                   <> T.pack (show (T.length diff))
                   <> " chars, limit "
                   <> T.pack (show previewDiffLimit)
                   <> "). Consider reading the file and making the change manually.")
                else
                  let summary = formatDiffCountSummary diff
                  in pure $ ToolResult ""
                    ("Diff preview " <> summary <> ":\n" <> diff)
  }

previewPatchBatchToolSpec :: ToolSpec
previewPatchBatchToolSpec = ToolSpec
  { specName        = "preview_patch_batch"
  , specDescription = "Preview multiple file changes (replace existing files or create new files) in one read-only call. Validates all operations before computing any preview. If any operation fails, the entire batch is rejected. Does NOT write to disk."
  , specSchema      = batchSchema "Ordered list of file operations to preview"
  , specExecute = \args -> do
      batchResult <- loadValidatedBatch args
      case batchResult of
        Left err -> pure $ ToolResult "" (formatBatchToolError err)
        Right batch -> pure $ ToolResult ""
          (formatBatchHeader (length (vbParts batch)) <> "\n"
           <> vbCombined batch)
  }

applyPatchBatchToolSpec :: ToolSpec
applyPatchBatchToolSpec = ToolSpec
  { specName        = "apply_patch_batch"
  , specDescription = "Apply multiple file changes (replace existing files or create new files) in one confirmed call. Validates all operations before writing. If any validation fails, nothing is written. Requires user confirmation. No rollback on partial write failure."
  , specSchema      = batchSchema "Ordered list of file operations to apply"
  , specExecute = \args -> do
      batchResult <- loadValidatedBatch args
      case batchResult of
        Left err -> pure $ ToolResult "" (formatBatchToolError err)
        Right batch -> do
          result <- applyValidatedBatchOps (vbOperations batch)
          pure $ ToolResult ""
            ("Batch applied:\n" <> vbCombined batch <> "\n" <> result)
  }

applyPatchToolSpec :: ToolSpec
applyPatchToolSpec = ToolSpec
  { specName        = "apply_patch"
  , specDescription = "Apply a patch to exactly one existing file under the working directory. Reads the current file, writes the replacement content, and returns the diff. Requires user confirmation before writing. Cannot create new files or delete files."
  , specSchema      = object
      [ "type"       .= ("object" :: Text)
      , "properties" .= object
          [ "path" .= object
              [ "type"        .= ("string" :: Text)
              , "description" .= ("Path to the existing file to modify" :: Text)
              ]
          , "replacement" .= object
              [ "type"        .= ("string" :: Text)
              , "description" .= ("New content to write to the file" :: Text)
              ]
          ]
      , "required"   .= (["path", "replacement"] :: [Text])
      ]
  , specExecute = \args -> do
      let path = case extractTextField "path" args of
            Just p  -> T.unpack p
            Nothing -> ""
          replacement = case extractTextField "replacement" args of
            Just r  -> r
            Nothing -> ""
      if null path
        then pure $ ToolResult "" "error: missing required field 'path'"
        else withRootContainment path $ \_rootCanon _pathCanon -> do
          readResult <- try (TIO.readFile path) :: IO (Either IOException Text)
          case readResult of
            Left e -> pure $ ToolResult ""
              ("error reading " <> T.pack path <> ": " <> T.pack (show e))
            Right current -> do
              let patch = makePatch path current replacement
                  diff  = showDiff patch
              applyResult <- try (applyPatch patch) :: IO (Either IOException Patch)
              case applyResult of
                Left e -> pure $ ToolResult ""
                  ("error writing " <> T.pack path <> ": " <> T.pack (show e))
                Right _ ->
                  let summary = formatDiffCountSummary diff
                  in pure $ ToolResult ""
                    ("Patch applied " <> summary <> ":\n" <> diff)
  }

writeFileToolSpec :: ToolSpec
writeFileToolSpec = ToolSpec
  { specName        = "write_file"
  , specDescription = "Create a new file under the working directory with the given content. Requires user confirmation before writing. Cannot overwrite existing files or create directories. The parent directory must already exist."
  , specSchema      = object
      [ "type"       .= ("object" :: Text)
      , "properties" .= object
          [ "path" .= object
              [ "type"        .= ("string" :: Text)
              , "description" .= ("Path for the new file to create" :: Text)
              ]
          , "content" .= object
              [ "type"        .= ("string" :: Text)
              , "description" .= ("Content to write to the new file" :: Text)
              ]
          ]
      , "required"   .= (["path", "content"] :: [Text])
      ]
  , specExecute = \args -> do
      let path = case extractTextField "path" args of
            Just p  -> T.unpack p
            Nothing -> ""
          content = case extractTextField "content" args of
            Just c  -> c
            Nothing -> ""
      if null path
        then pure $ ToolResult "" "error: missing required field 'path'"
        else do
          rootCanonResult <- safeCanonicalize "."
          case rootCanonResult of
            Nothing -> pure $ ToolResult ""
              "error: could not resolve working directory"
            Just rootCanon -> do
              pathExists <- doesPathExist path
              if pathExists
                then do
                  isDir <- doesDirectoryExist path
                  if isDir
                    then pure $ ToolResult ""
                      ("error: path is a directory, not a file: " <> T.pack path)
                    else pure $ ToolResult ""
                      ("error: file already exists, will not overwrite: "
                       <> T.pack path)
                else do
                  let parentDir = takeParentDir path
                  parentCanonResult <- safeCanonicalize parentDir
                  case parentCanonResult of
                    Nothing -> pure $ ToolResult ""
                      ("error: parent directory does not exist or is not accessible: "
                       <> T.pack parentDir)
                    Just parentCanon
                      | not (isUnderRoot rootCanon parentCanon) ->
                        pure $ ToolResult ""
                          ("error: path must be under the working directory: "
                           <> T.pack path)
                      | otherwise -> do
                        parentIsDir <- doesDirectoryExist parentDir
                        if not parentIsDir
                          then pure $ ToolResult ""
                            ("error: parent path is not a directory: "
                             <> T.pack parentDir)
                          else do
                            let nAdded = length (T.lines content)
                                preview = formatNewFilePreview path content
                            writeResult <- try (TIO.writeFile path content)
                                           :: IO (Either IOException ())
                            case writeResult of
                              Left e -> pure $ ToolResult ""
                                ("error writing " <> T.pack path <> ": "
                                 <> T.pack (show e))
                              Right _ -> pure $ ToolResult ""
                                ("File created (" <> T.pack (show nAdded) <> " lines):\n" <> preview)
  }

computePatchPreview :: Value -> IO (Either Text (FilePath, Text))
computePatchPreview args = do
  let path = case extractTextField "path" args of
        Just p  -> T.unpack p
        Nothing -> ""
      replacement = case extractTextField "replacement" args of
        Just r  -> r
        Nothing -> ""
  if null path
    then pure $ Left "missing required field 'path'"
    else do
      rootCanonResult <- safeCanonicalize "."
      case rootCanonResult of
        Nothing -> pure $ Left "could not resolve working directory"
        Just rootCanon -> do
          pathCanonResult <- safeCanonicalize path
          case pathCanonResult of
            Nothing -> pure $ Left ("could not resolve path: " <> T.pack path)
            Just pathCanon
              | not (isUnderRoot rootCanon pathCanon) ->
                pure $ Left ("path must be under the working directory: " <> T.pack path)
              | otherwise -> do
                readResult <- try (TIO.readFile path) :: IO (Either IOException Text)
                case readResult of
                  Left e -> pure $ Left ("error reading " <> T.pack path <> ": " <> T.pack (show e))
                  Right current -> do
                    let patch = makePatch path current replacement
                        diff  = showDiff patch
                    pure $ Right (path, diff)

computeWriteFilePreview :: Value -> IO (Either Text (FilePath, Text))
computeWriteFilePreview args = do
  let path = case extractTextField "path" args of
        Just p  -> T.unpack p
        Nothing -> ""
      content = case extractTextField "content" args of
        Just c  -> c
        Nothing -> ""
  if null path
    then pure $ Left "missing required field 'path'"
    else do
      rootCanonResult <- safeCanonicalize "."
      case rootCanonResult of
        Nothing -> pure $ Left "could not resolve working directory"
        Just rootCanon -> do
          pathExists <- doesPathExist path
          if pathExists
            then do
              isDir <- doesDirectoryExist path
              if isDir
                then pure $ Left ("path is a directory, not a file: " <> T.pack path)
                else pure $ Left ("file already exists, will not overwrite: " <> T.pack path)
            else do
              let parentDir = takeParentDir path
              parentCanonResult <- safeCanonicalize parentDir
              case parentCanonResult of
                Nothing -> pure $ Left
                  ("parent directory does not exist or is not accessible: "
                   <> T.pack parentDir)
                Just parentCanon
                  | not (isUnderRoot rootCanon parentCanon) ->
                    pure $ Left ("path must be under the working directory: " <> T.pack path)
                  | otherwise -> do
                    parentIsDir <- doesDirectoryExist parentDir
                    if not parentIsDir
                      then pure $ Left ("parent path is not a directory: " <> T.pack parentDir)
                      else pure $ Right (path, formatNewFilePreview path content)

computeBatchApplyPreview :: Value -> IO (Either Text (Text, [Text]))
computeBatchApplyPreview args = do
  batchResult <- loadValidatedBatch args
  case batchResult of
    Left err -> pure $ Left err
    Right batch ->
      let ops = map validatedBatchOp (vbOperations batch)
          summary = formatBatchApplySummary ops
      in pure $ Right (summary, vbParts batch)

batchSchema :: Text -> Value
batchSchema operationsDescription = object
  [ "type"       .= ("object" :: Text)
  , "properties" .= object
      [ "operations" .= object
          [ "type"        .= ("array" :: Text)
          , "minItems"    .= (1 :: Int)
          , "items"       .= object
              [ "type"       .= ("object" :: Text)
              , "properties" .= object
                  [ "op" .= object
                      [ "type"        .= ("string" :: Text)
                      , "enum"        .= (["replace", "create"] :: [Text])
                      , "description" .= ("replace = modify existing file; create = write new file" :: Text)
                      ]
                  , "path" .= object
                      [ "type"        .= ("string" :: Text)
                      , "description" .= ("File path relative to working directory" :: Text)
                      ]
                  , "replacement" .= object
                      [ "type"        .= ("string" :: Text)
                      , "description" .= ("New file content (for replace operations)" :: Text)
                      ]
                  , "content" .= object
                      [ "type"        .= ("string" :: Text)
                      , "description" .= ("File content (for create operations)" :: Text)
                      ]
                  ]
              , "required" .= (["op", "path"] :: [Text])
              ]
          , "description" .= operationsDescription
          ]
      ]
  , "required" .= (["operations"] :: [Text])
  ]

extractRawOperations :: Value -> Maybe [Value]
extractRawOperations (Object o) =
  case KM.lookup (Key.fromText "operations") o of
    Just (Array v) -> Just (V.toList v)
    _              -> Nothing
extractRawOperations _ = Nothing

loadValidatedBatch :: Value -> IO (Either Text ValidatedBatch)
loadValidatedBatch args =
  case extractRawOperations args of
    Nothing -> pure $ Left missingOperationsError
    Just [] -> pure $ Left "operations list must not be empty"
    Just raw ->
      case parseBatchOps raw of
        Left (n, msg) -> pure $ Left
          ("error in operation " <> T.pack (show n) <> ": " <> msg)
        Right ops -> do
          rootCanonResult <- safeCanonicalize "."
          case rootCanonResult of
            Nothing -> pure $ Left "could not resolve working directory"
            Just rootCanon -> do
              validatedResult <- validateBatchOps rootCanon ops
              case validatedResult of
                Left err -> pure $ Left err
                Right validated -> do
                  let parts = formatValidatedBatchPreviews validated
                      combined = T.intercalate "\n" parts
                  if T.length combined > batchPreviewLimit
                    then pure $ Left
                      ("combined preview too large ("
                       <> T.pack (show (T.length combined))
                       <> " chars, limit "
                       <> T.pack (show batchPreviewLimit)
                       <> "). Split into smaller batches.")
                    else pure $ Right ValidatedBatch
                      { vbOperations = validated
                      , vbParts = parts
                      , vbCombined = combined
                      }

formatValidatedBatchPreviews :: [ValidatedBatchOp] -> [Text]
formatValidatedBatchPreviews validated =
  [ batchOpPreview n op preview
  | (n, ValidatedBatchOp op preview) <- zip [1..] validated
  ]
