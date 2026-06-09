{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Haskode.Test.Tools (tests) where

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

-- Truncation helper tests (pure, no IO)
-- ---------------------------------------------------------------------------

-- | Text shorter than the limit passes through unchanged.
testTruncateTextNoOp :: Test
testTruncateTextNoOp =
  let tr = truncateText 100 "hello"
  in if truncText tr == "hello"
        && truncOriginalLength tr == 5
        && truncReturnedLength tr == 5
        && not (truncDidTruncate tr)
        && truncDropped tr == 0
     then pure $ Right ()
     else pure $ Left $ "truncateText (short): " ++ show tr

-- | Text exceeding the limit is truncated with correct metadata.
testTruncateTextTruncates :: Test
testTruncateTextTruncates =
  let input = T.replicate 100 "x"   -- 100 chars
      tr    = truncateText 10 input
  in if T.length (truncText tr) == 10
        && truncOriginalLength tr == 100
        && truncReturnedLength tr == 10
        && truncDidTruncate tr
        && truncDropped tr == 90
     then pure $ Right ()
     else pure $ Left $ "truncateText (long): " ++ show tr

-- | Text exactly at the limit is not truncated.
testTruncateTextExactLimit :: Test
testTruncateTextExactLimit =
  let input = T.replicate 50 "a"   -- exactly 50 chars
      tr    = truncateText 50 input
  in if truncText tr == input
        && not (truncDidTruncate tr)
        && truncDropped tr == 0
     then pure $ Right ()
     else pure $ Left $ "truncateText (exact): " ++ show tr

-- | formatTruncMeta returns empty string when no truncation.
testFormatTruncMetaNoOp :: Test
testFormatTruncMetaNoOp =
  let tr   = truncateText 100 "short"
      meta = formatTruncMeta tr
  in if meta == ""
     then pure $ Right ()
     else pure $ Left $ "formatTruncMeta (no trunc): expected empty, got: " ++ T.unpack meta

-- | formatTruncMeta returns proper metadata line when truncated.
testFormatTruncMetaTruncated :: Test
testFormatTruncMetaTruncated =
  let input = T.replicate 200 "b"
      tr    = truncateText 50 input
      meta  = formatTruncMeta tr
  in if T.isInfixOf "[truncated:" meta
        && T.isInfixOf "returned 50 of 200 chars" meta
        && T.isInfixOf "150 dropped" meta
     then pure $ Right ()
     else pure $ Left $ "formatTruncMeta (trunc): " ++ T.unpack meta

-- | Empty text is not truncated.
testTruncateTextEmpty :: Test
testTruncateTextEmpty =
  let tr = truncateText 100 ""
  in if truncText tr == ""
        && truncOriginalLength tr == 0
        && not (truncDidTruncate tr)
     then pure $ Right ()
     else pure $ Left $ "truncateText (empty): " ++ show tr

testMatchGlobSimpleStar :: Test
testMatchGlobSimpleStar =
  if matchGlob "*.hs" "Foo.hs" && not (matchGlob "*.hs" "src/Foo.hs")
     && matchGlob "*.txt" "README.txt" && not (matchGlob "*.hs" "Foo.txt")
    then pure $ Right ()
    else pure $ Left "matchGlob simple * failed"

-- | matchGlob with ** recursive wildcard.
testMatchGlobDoubleStar :: Test
testMatchGlobDoubleStar =
  if matchGlob "**/*.hs" "Foo.hs"
     && matchGlob "**/*.hs" "src/Foo.hs"
     && matchGlob "**/*.hs" "src/A/Bar.hs"
     && not (matchGlob "**/*.hs" "Foo.txt")
    then pure $ Right ()
    else pure $ Left "matchGlob ** failed"

-- | matchGlob with directory prefix.
testMatchGlobDirPrefix :: Test
testMatchGlobDirPrefix =
  if matchGlob "src/**/*.hs" "src/Foo.hs"
     && matchGlob "src/**/*.hs" "src/A/Bar.hs"
     && not (matchGlob "src/**/*.hs" "test/Foo.hs")
     && not (matchGlob "src/**/*.hs" "Foo.hs")
    then pure $ Right ()
    else pure $ Left "matchGlob dir prefix failed"

-- | matchGlob with exact filename (no wildcard).
testMatchGlobExact :: Test
testMatchGlobExact =
  if matchGlob "Makefile" "Makefile"
     && not (matchGlob "Makefile" "src/Makefile")
     && not (matchGlob "Makefile" "makefile")
    then pure $ Right ()
    else pure $ Left "matchGlob exact failed"

-- | isIgnoredDir skips known build/cache directories.
testIsIgnoredDir :: Test
testIsIgnoredDir =
  if isIgnoredDir ".git"
     && isIgnoredDir "dist-newstyle"
     && isIgnoredDir ".stack-work"
     && isIgnoredDir "node_modules"
     && not (isIgnoredDir "src")
     && not (isIgnoredDir "test")
    then pure $ Right ()
    else pure $ Left "isIgnoredDir failed"

-- ---------------------------------------------------------------------------
-- Search helper tests (pure)
-- ---------------------------------------------------------------------------

-- | searchInText finds matching lines (case-sensitive default).
testSearchInText :: Test
testSearchInText =
  let body = "hello world\nfoo bar\nhello again\nnothing"
      results = searchInText False "hello" body
  in if map fst results == [1, 3] && length results == 2
       then pure $ Right ()
       else pure $ Left $ "searchInText: " ++ show (map fst results)

-- | searchInText returns nothing for empty query.
testSearchInTextEmptyQuery :: Test
testSearchInTextEmptyQuery =
  if null (searchInText False "" "hello world")
    then pure $ Right ()
    else pure $ Left "searchInText empty query should return no results"

-- | searchInText returns nothing when query not found.
testSearchInTextNoMatch :: Test
testSearchInTextNoMatch =
  if null (searchInText False "xyz" "hello world\nfoo bar")
    then pure $ Right ()
    else pure $ Left "searchInText no-match should return no results"

-- | searchInText with ignoreCase=True finds mixed-case matches.
testSearchInTextIgnoreCase :: Test
testSearchInTextIgnoreCase =
  let body = "Hello World\nfoo bar\nHELLO again\nnothing"
      results = searchInText True "hello" body
  in if map fst results == [1, 3] && length results == 2
       then pure $ Right ()
       else pure $ Left $ "searchInText ignoreCase: " ++ show (map fst results)

-- | searchInText with ignoreCase=True returns nothing when query not found.
testSearchInTextIgnoreCaseNoMatch :: Test
testSearchInTextIgnoreCaseNoMatch =
  if null (searchInText True "xyz" "Hello World\nFoo Bar")
    then pure $ Right ()
    else pure $ Left "searchInText ignoreCase no-match should return no results"

-- | searchInText with ignoreCase=False is still case-sensitive (does not match mixed case).
testSearchInTextCaseSensitiveDefault :: Test
testSearchInTextCaseSensitiveDefault =
  let body = "Hello World\nfoo bar\nHELLO again\nnothing"
      results = searchInText False "hello" body
  in if null results
       then pure $ Right ()
       else pure $ Left $ "searchInText case-sensitive should miss mixed-case: " ++ show (map fst results)

-- | formatSearchMatch produces path:line:snippet format.
testFormatSearchMatch :: Test
testFormatSearchMatch =
  let result = formatSearchMatch ("src/Main.hs", 42, "hello world")
  in if T.isInfixOf "src/Main.hs:42:" result && T.isInfixOf "hello world" result
       then pure $ Right ()
       else pure $ Left $ "formatSearchMatch: " ++ T.unpack result

-- | formatSearchMatch truncates long lines to 120 chars.
testFormatSearchMatchTruncates :: Test
testFormatSearchMatchTruncates =
  let longLine = T.replicate 200 "x"
      result   = formatSearchMatch ("f.hs", 1, longLine)
      snippet  = last (T.splitOn ":" result)
  in if T.length snippet <= 120
       then pure $ Right ()
       else pure $ Left $ "formatSearchMatch did not truncate: " ++ show (T.length snippet)

-- ---------------------------------------------------------------------------
-- Glob tool IO tests
-- ---------------------------------------------------------------------------

-- | Helper: create a temp directory tree for testing.
--   Returns (rootDir, cleanup action).

-- | globTool finds files matching a simple pattern.
testGlobToolSimple :: Test
testGlobToolSimple = do
  (root, cleanupAction) <- createTestTree
  -- Run glob from the test root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("*.hs" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "Main.hs" out && T.isInfixOf "1 files match" out
    then pure $ Right ()
    else pure $ Left $ "glob *.hs: " ++ T.unpack out

-- | globTool with ** wildcard finds files in subdirectories.
testGlobToolRecursive :: Test
testGlobToolRecursive = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*.hs" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Should find Main.hs, src/Lib.hs, src/Util.hs, src/deep/Inner.hs, test/Spec.hs
  if T.isInfixOf "Main.hs" out && T.isInfixOf "Lib.hs" out
     && T.isInfixOf "Inner.hs" out && T.isInfixOf "5 files match" out
    then pure $ Right ()
    else pure $ Left $ "glob **/*.hs: " ++ T.unpack out

-- | globTool skips .git and other ignored directories.
testGlobToolSkipsIgnored :: Test
testGlobToolSkipsIgnored = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Should NOT contain .git/config
  if not (T.isInfixOf ".git" out)
    then pure $ Right ()
    else pure $ Left $ "glob **/* included .git: " ++ T.unpack out

-- | globTool with directory prefix pattern.
testGlobToolDirPrefix :: Test
testGlobToolDirPrefix = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("src/**/*.hs" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Should find src/Lib.hs, src/Util.hs, src/deep/Inner.hs but NOT Main.hs or test/Spec.hs
  if T.isInfixOf "Lib.hs" out && T.isInfixOf "Inner.hs" out
     && not (T.isInfixOf "Main.hs" out) && not (T.isInfixOf "Spec.hs" out)
     && T.isInfixOf "3 files match" out
    then pure $ Right ()
    else pure $ Left $ "glob src/**/*.hs: " ++ T.unpack out

-- | globTool returns "0 files" for a pattern that matches nothing.
testGlobToolNoMatch :: Test
testGlobToolNoMatch = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("*.xyz" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "0 files match" out
    then pure $ Right ()
    else pure $ Left $ "glob *.xyz: " ++ T.unpack out

-- ---------------------------------------------------------------------------
-- Search tool IO tests
-- ---------------------------------------------------------------------------

-- | searchTool finds matches across files.
testSearchToolFindsMatches :: Test
testSearchToolFindsMatches = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object ["query" .= ("module" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- "module" appears in Main.hs, Lib.hs, Util.hs, Inner.hs, Spec.hs
  if T.isInfixOf "matches for" out && T.isInfixOf "Main.hs:" out
     && T.isInfixOf "Lib.hs:" out
    then pure $ Right ()
    else pure $ Left $ "search module: " ++ T.unpack out

-- | searchTool returns "0 matches" when query is not found.
testSearchToolNoMatch :: Test
testSearchToolNoMatch = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object ["query" .= ("zzzznotfound" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "0 matches" out
    then pure $ Right ()
    else pure $ Left $ "search zzzznotfound: " ++ T.unpack out

-- | searchTool respects directory argument (search single file).
testSearchToolSingleFile :: Test
testSearchToolSingleFile = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query"     .= ("putStrLn" :: T.Text)
    , "directory" .= ("Main.hs" :: T.Text)
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "Main.hs:" out && T.isInfixOf "1 matches" out
    then pure $ Right ()
    else pure $ Left $ "search Main.hs: " ++ T.unpack out

-- | searchTool skips binary-extension files.
testSearchToolSkipsBinary :: Test
testSearchToolSkipsBinary = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-search-bin-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "code.hs")  "foobar_unique_12345"
  writeFile (root </> "image.png") "foobar_unique_12345"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object ["query" .= ("foobar_unique_12345" :: T.Text)])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find code.hs but NOT image.png
  if T.isInfixOf "code.hs" out && not (T.isInfixOf "image.png" out)
    then pure $ Right ()
    else pure $ Left $ "search binary skip: " ++ T.unpack out

-- | searchTool skips .git directory.
testSearchToolSkipsIgnoredDirs :: Test
testSearchToolSkipsIgnoredDirs = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object ["query" .= ("core" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- "core" appears in .git/config but should be skipped
  if not (T.isInfixOf ".git" out)
    then pure $ Right ()
    else pure $ Left $ "search included .git: " ++ T.unpack out

-- | globTool is in the default registry.
testGlobInRegistry :: Test
testGlobInRegistry =
  if "glob" `elem` toolNames defaultRegistry
    then pure $ Right ()
    else pure $ Left $ "glob not in registry: " ++ show (toolNames defaultRegistry)

-- | searchTool is in the default registry.
testSearchInRegistry :: Test
testSearchInRegistry =
  if "search" `elem` toolNames defaultRegistry
    then pure $ Right ()
    else pure $ Left $ "search not in registry: " ++ show (toolNames defaultRegistry)

-- ---------------------------------------------------------------------------
-- isUnderRoot tests (pure)
-- ---------------------------------------------------------------------------

-- | isUnderRoot allows exact match.
testIsUnderRootExact :: Test
testIsUnderRootExact =
  if isUnderRoot "/a/b" "/a/b"
    then pure $ Right ()
    else pure $ Left "isUnderRoot exact match failed"

-- | isUnderRoot allows subdirectory.
testIsUnderRootSubdir :: Test
testIsUnderRootSubdir =
  if isUnderRoot "/a/b" "/a/b/c/d"
    then pure $ Right ()
    else pure $ Left "isUnderRoot subdirectory failed"

-- | isUnderRoot rejects sibling (prefix collision).
testIsUnderRootSibling :: Test
testIsUnderRootSibling =
  if not (isUnderRoot "/a/b" "/a/bad")
    then pure $ Right ()
    else pure $ Left "isUnderRoot rejected sibling prefix collision"

-- | isUnderRoot rejects unrelated path.
testIsUnderRootOutside :: Test
testIsUnderRootOutside =
  if not (isUnderRoot "/a/b" "/etc")
    then pure $ Right ()
    else pure $ Left "isUnderRoot rejected unrelated path"

-- ---------------------------------------------------------------------------
-- Search: directory default and path safety
-- ---------------------------------------------------------------------------

-- | searchTool with no directory argument defaults to working directory.
testSearchDefaultDir :: Test
testSearchDefaultDir = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object ["query" .= ("module" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "Main.hs:" out && T.isInfixOf "Lib.hs:" out
    then pure $ Right ()
    else pure $ Left $ "search default dir: " ++ T.unpack out

-- | searchTool default (case-sensitive) does NOT match wrong case.
testSearchCaseSensitiveDefault :: Test
testSearchCaseSensitiveDefault = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-search-cs-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "a.hs") "module Main where\nimport Foo\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object ["query" .= ("MODULE" :: T.Text)])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "0 matches" out
    then pure $ Right ()
    else pure $ Left $ "search case-sensitive default: " ++ T.unpack out

-- | searchTool with ignore_case:true finds mixed-case matches.
testSearchIgnoreCaseTrue :: Test
testSearchIgnoreCaseTrue = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-search-ic-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "a.hs") "module Main where\nimport Foo\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query"       .= ("MODULE" :: T.Text)
    , "ignore_case" .= True
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  if T.isInfixOf "1 matches" out && T.isInfixOf "case-insensitive" out
    then pure $ Right ()
    else pure $ Left $ "search ignore_case true: " ++ T.unpack out

-- | searchTool with ignore_case:true and scoped directory still works.
testSearchIgnoreCaseScoped :: Test
testSearchIgnoreCaseScoped = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query"       .= ("MODULE" :: T.Text)
    , "directory"   .= ("src" :: T.Text)
    , "ignore_case" .= True
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Should find "module" in src/ files but NOT in test/Spec.hs or Main.hs
  if T.isInfixOf "Lib.hs:" out && T.isInfixOf "case-insensitive" out
     && not (T.isInfixOf "Spec.hs:" out) && not (T.isInfixOf "Main.hs:" out)
    then pure $ Right ()
    else pure $ Left $ "search ignore_case scoped: " ++ T.unpack out

-- | searchTool with directory argument scopes to that subtree.
testSearchScopedDir :: Test
testSearchScopedDir = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query"     .= ("module" :: T.Text)
    , "directory" .= ("src" :: T.Text)
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Should find matches in src/ but NOT in test/Spec.hs or Main.hs
  if T.isInfixOf "Lib.hs:" out && not (T.isInfixOf "Spec.hs:" out)
     && not (T.isInfixOf "Main.hs:" out)
    then pure $ Right ()
    else pure $ Left $ "search scoped dir: " ++ T.unpack out

-- | searchTool rejects a directory outside the working root.
testSearchRejectsOutsideRoot :: Test
testSearchRejectsOutsideRoot = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query"     .= ("module" :: T.Text)
    , "directory" .= ("C:\\Windows\\System32" :: T.Text)
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "search outside root: " ++ T.unpack out

-- | searchTool returns error for a non-existent path.
testSearchNotFound :: Test
testSearchNotFound = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query"     .= ("x" :: T.Text)
    , "directory" .= ("nonexistent_dir" :: T.Text)
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out && T.isInfixOf "not found" out
    then pure $ Right ()
    else pure $ Left $ "search not found: " ++ T.unpack out

-- ---------------------------------------------------------------------------
-- Search: large file skipping
-- ---------------------------------------------------------------------------

-- | searchTool skips files larger than searchMaxFileSize.
testSearchSkipsLargeFile :: Test
testSearchSkipsLargeFile = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-search-size-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "small.hs") "unique_token_abc123"
  -- Write a file just over the limit.
  let bigSize = fromIntegral searchMaxFileSize + 1
  writeFile (root </> "big.hs") (replicate bigSize 'x' ++ "unique_token_abc123\n")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query" .= ("unique_token_abc123" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find the match in small.hs but skip big.hs.
  if T.isInfixOf "small.hs:" out && T.isInfixOf "1 matches" out
     && T.isInfixOf "skipped" out && T.isInfixOf "1 large files" out
    then pure $ Right ()
    else pure $ Left $ "search large file: " ++ T.unpack out

-- | searchMaxFileSize is a positive value.
testSearchMaxFileSizePositive :: Test
testSearchMaxFileSizePositive =
  if searchMaxFileSize > 0
    then pure $ Right ()
    else pure $ Left $ "searchMaxFileSize not positive: " ++ show searchMaxFileSize

-- ---------------------------------------------------------------------------
-- TraversalStats / formatStats tests (pure)
-- ---------------------------------------------------------------------------

-- | formatStats returns empty string for zero stats.
testFormatStatsEmpty :: Test
testFormatStatsEmpty =
  if formatStats emptyStats == ""
    then pure $ Right ()
    else pure $ Left "formatStats empty should be empty string"

-- | formatStats formats a mix of skip reasons correctly.
testFormatStatsMixed :: Test
testFormatStatsMixed =
  let stats = TraversalStats { tsSkippedLarge = 2, tsSkippedUnreadable = 1, tsOutsideRoot = 0, tsIgnoredByAgent = 0, tsRevisitedDirs = 0 }
      out   = formatStats stats
  in if T.isInfixOf "2 large files" out && T.isInfixOf "1 unreadable" out
       && not (T.isInfixOf "outside" out)
       then pure $ Right ()
       else pure $ Left $ "formatStats mixed: " ++ T.unpack out

-- | formatStats formats outside-root correctly.
testFormatStatsOutside :: Test
testFormatStatsOutside =
  let stats = emptyStats { tsOutsideRoot = 3 }
      out   = formatStats stats
  in if T.isInfixOf "3 outside project root" out
       then pure $ Right ()
       else pure $ Left $ "formatStats outside: " ++ T.unpack out

-- ---------------------------------------------------------------------------
-- safeCanonicalize tests
-- ---------------------------------------------------------------------------

-- | safeCanonicalize returns Just for an existing path.
testSafeCanonicalizeExisting :: Test
testSafeCanonicalizeExisting = do
  result <- safeCanonicalize "."
  case result of
    Just _  -> pure $ Right ()
    Nothing -> pure $ Left "safeCanonicalize '.' returned Nothing"

-- | safeCanonicalize returns Nothing for a broken symlink.
testSafeCanonicalizeBrokenSymlink :: Test
testSafeCanonicalizeBrokenSymlink = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-canon-broken-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createFileLink (root </> "nonexistent") (root </> "broken")
  result <- safeCanonicalize (root </> "broken")
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Nothing -> pure $ Right ()
    Just _  -> pure $ Left "safeCanonicalize broken symlink should return Nothing"

-- ---------------------------------------------------------------------------
-- Glob: broken symlink and outside-root tests
-- ---------------------------------------------------------------------------

-- | globTool handles broken symlinks gracefully (skips them).
testGlobBrokenSymlink :: Test
testGlobBrokenSymlink = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-glob-broken-sym"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "good.hs") "module Good where"
  createFileLink (root </> "nonexistent") (root </> "broken.hs")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object
    [ "pattern" .= ("*.hs" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find good.hs, skip broken.hs, report unreadable
  if T.isInfixOf "good.hs" out && T.isInfixOf "skipped" out && T.isInfixOf "unreadable" out
    then pure $ Right ()
    else pure $ Left $ "glob broken symlink: " ++ T.unpack out

-- | globTool skips symlinks that resolve outside the project root.
testGlobOutsideRootSymlink :: Test
testGlobOutsideRootSymlink = do
  tmpDir <- getTemporaryDirectory
  let root   = tmpDir </> "haskode-glob-outside-root"
      target = tmpDir </> "haskode-glob-outside-target"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  createDirectory root
  createDirectory target
  writeFile (target </> "secret.hs") "module Secret where"
  writeFile (root </> "safe.hs") "module Safe where"
  createFileLink target (root </> "outside")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object
    [ "pattern" .= ("**/*.hs" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find safe.hs but NOT secret.hs; should report outside root
  if T.isInfixOf "safe.hs" out && not (T.isInfixOf "secret.hs" out)
     && T.isInfixOf "outside" out
    then pure $ Right ()
    else pure $ Left $ "glob outside root: " ++ T.unpack out

-- ---------------------------------------------------------------------------
-- Search: broken symlink and outside-root tests
-- ---------------------------------------------------------------------------

-- | searchTool handles broken symlinks gracefully (skips them).
testSearchBrokenSymlink :: Test
testSearchBrokenSymlink = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-search-broken-sym"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "good.hs") "unique_token_broken_test"
  createFileLink (root </> "nonexistent") (root </> "broken.hs")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query" .= ("unique_token_broken_test" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find good.hs, skip broken.hs, report unreadable
  if T.isInfixOf "good.hs:" out && T.isInfixOf "skipped" out && T.isInfixOf "unreadable" out
    then pure $ Right ()
    else pure $ Left $ "search broken symlink: " ++ T.unpack out

-- | searchTool skips symlinks that resolve outside the project root.
testSearchOutsideRootSymlink :: Test
testSearchOutsideRootSymlink = do
  tmpDir <- getTemporaryDirectory
  let root   = tmpDir </> "haskode-search-outside-root"
      target = tmpDir </> "haskode-search-outside-target"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  createDirectory root
  createDirectory target
  writeFile (target </> "secret.hs") "unique_token_secret_outside"
  writeFile (root </> "safe.hs") "unique_token_secret_outside"
  createFileLink target (root </> "outside")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query" .= ("unique_token_secret_outside" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find safe.hs but NOT secret.hs; should report outside root
  if T.isInfixOf "safe.hs:" out && not (T.isInfixOf "secret.hs" out)
     && T.isInfixOf "outside" out
    then pure $ Right ()
    else pure $ Left $ "search outside root: " ++ T.unpack out

-- | searchTool handles an unreadable directory gracefully.
testSearchUnreadableDir :: Test
testSearchUnreadableDir = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-search-unreadable-dir"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createDirectory (root </> "locked")
  writeFile (root </> "locked" </> "secret.hs") "unique_token_locked"
  writeFile (root </> "open.hs") "unique_token_locked"
  origPerms <- getPermissions (root </> "locked")
  setPermissions (root </> "locked") emptyPermissions
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query" .= ("unique_token_locked" :: T.Text)
    ])
  setCurrentDirectory origDir
  -- Restore permissions so cleanup works
  setPermissions (root </> "locked") origPerms
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find open.hs, skip locked dir, report unreadable
  if T.isInfixOf "open.hs:" out && T.isInfixOf "skipped" out && T.isInfixOf "unreadable" out
    then pure $ Right ()
    else pure $ Left $ "search unreadable dir: " ++ T.unpack out

-- | readFileTool returns an error message for an unreadable file.
testReadFileUnreadable :: Test
testReadFileUnreadable = do
  (root, cleanupAction) <- createTestTree
  writeFile (root </> "locked.txt") "secret content"
  origPerms <- getPermissions (root </> "locked.txt")
  setPermissions (root </> "locked.txt") emptyPermissions
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute readFileTool (object
    [ "path" .= ("locked.txt" :: T.Text)
    ])
  setCurrentDirectory origDir
  setPermissions (root </> "locked.txt") origPerms
  cleanupAction
  let out = trOutput result
  -- Should return an error, not crash
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "read_file unreadable: " ++ T.unpack out

-- | listFilesTool returns an error message for an unreadable directory.
testListFilesUnreadable :: Test
testListFilesUnreadable = do
  (root, cleanupAction) <- createTestTree
  createDirectory (root </> "locked")
  origPerms <- getPermissions (root </> "locked")
  setPermissions (root </> "locked") emptyPermissions
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute listFilesTool (object
    [ "dir" .= ("locked" :: T.Text)
    ])
  setCurrentDirectory origDir
  setPermissions (root </> "locked") origPerms
  cleanupAction
  let out = trOutput result
  -- Should return an error, not crash
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "list_files unreadable: " ++ T.unpack out

-- ---------------------------------------------------------------------------
-- Path-safety hardening tests for read_file and list_files
-- ---------------------------------------------------------------------------

-- | readFileTool rejects paths that escape the working directory via ...
testReadFileRejectsDotDotEscape :: Test
testReadFileRejectsDotDotEscape = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute readFileTool (object
    [ "path" .= ("../../etc/passwd" :: T.Text)
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Either "working directory" or "could not resolve" is acceptable
  -- (Windows may not resolve .. paths the same as Unix).
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "read_file .. escape: " ++ T.unpack out

-- | readFileTool rejects absolute paths outside the working directory.
testReadFileRejectsAbsoluteOutsideRoot :: Test
testReadFileRejectsAbsoluteOutsideRoot = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute readFileTool (object
    [ "path" .= ("C:\\Windows\\System32\\config\\SAM" :: T.Text)
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- On any platform, reading a system file outside the working dir must fail.
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "read_file absolute outside: " ++ T.unpack out

-- | readFileTool allows normal relative paths within the working directory.
testReadFileNormalInRoot :: Test
testReadFileNormalInRoot = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute readFileTool (object
    [ "path" .= ("src/Lib.hs" :: T.Text)
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "module Lib" out
    then pure $ Right ()
    else pure $ Left $ "read_file in-root: " ++ T.unpack out

-- | readFileTool rejects symlinks that resolve outside the working directory.
testReadFileRejectsOutsideRootSymlink :: Test
testReadFileRejectsOutsideRootSymlink = do
  tmpDir <- getTemporaryDirectory
  let root   = tmpDir </> "haskode-rf-safety-root"
      target = tmpDir </> "haskode-rf-safety-target"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  createDirectory root
  createDirectory target
  writeFile (target </> "secret.txt") "secret content"
  writeFile (root </> "safe.txt") "safe content"
  createFileLink (target </> "secret.txt") (root </> "outside-link")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute readFileTool (object
    [ "path" .= ("outside-link" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  let out = trOutput result
  -- Should reject the outside-root symlink, not return secret content
  if T.isInfixOf "error" out && T.isInfixOf "working directory" out
     && not (T.isInfixOf "secret content" out)
    then pure $ Right ()
    else pure $ Left $ "read_file outside symlink: " ++ T.unpack out

-- | readFileTool handles broken symlinks gracefully.
testReadFileHandlesBrokenSymlink :: Test
testReadFileHandlesBrokenSymlink = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-rf-broken-root"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "good.txt") "good content"
  createFileLink (root </> "nonexistent") (root </> "broken-link")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute readFileTool (object
    [ "path" .= ("broken-link" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should return an error (could not resolve), not crash
  if T.isInfixOf "error" out && T.isInfixOf "could not resolve" out
    then pure $ Right ()
    else pure $ Left $ "read_file broken symlink: " ++ T.unpack out

-- | listFilesTool rejects paths that escape the working directory via ...
testListFilesRejectsDotDotEscape :: Test
testListFilesRejectsDotDotEscape = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute listFilesTool (object
    [ "dir" .= ("../../tmp" :: T.Text)
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "list_files .. escape: " ++ T.unpack out

-- | listFilesTool rejects absolute paths outside the working directory.
testListFilesRejectsAbsoluteOutsideRoot :: Test
testListFilesRejectsAbsoluteOutsideRoot = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute listFilesTool (object
    [ "dir" .= ("C:\\Windows\\System32" :: T.Text)
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- On any platform, listing a system dir outside the working dir must fail.
  if T.isInfixOf "error" out
    then pure $ Right ()
    else pure $ Left $ "list_files absolute outside: " ++ T.unpack out

-- | listFilesTool allows normal relative paths within the working directory.
testListFilesNormalInRoot :: Test
testListFilesNormalInRoot = do
  (root, cleanupAction) <- createTestTree
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute listFilesTool (object
    [ "dir" .= ("src" :: T.Text)
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "Lib.hs" out && T.isInfixOf "Util.hs" out
    then pure $ Right ()
    else pure $ Left $ "list_files in-root: " ++ T.unpack out

-- | listFilesTool rejects symlinks that resolve outside the working directory.
testListFilesRejectsOutsideRootSymlink :: Test
testListFilesRejectsOutsideRootSymlink = do
  tmpDir <- getTemporaryDirectory
  let root   = tmpDir </> "haskode-lf-safety-root"
      target = tmpDir </> "haskode-lf-safety-target"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  createDirectory root
  createDirectory target
  writeFile (target </> "secret.hs") "secret module"
  writeFile (root </> "safe.hs") "safe module"
  createDirectoryLink target (root </> "outside-dir")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute listFilesTool (object
    [ "dir" .= ("outside-dir" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  let out = trOutput result
  -- Should reject the outside-root symlink, not list target contents
  if T.isInfixOf "error" out && T.isInfixOf "working directory" out
     && not (T.isInfixOf "secret.hs" out)
    then pure $ Right ()
    else pure $ Left $ "list_files outside symlink: " ++ T.unpack out

-- | listFilesTool handles broken symlinks gracefully.
testListFilesHandlesBrokenSymlink :: Test
testListFilesHandlesBrokenSymlink = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-lf-broken-root"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createDirectoryLink (root </> "nonexistent") (root </> "broken-dir")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute listFilesTool (object
    [ "dir" .= ("broken-dir" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should return an error (could not resolve), not crash
  if T.isInfixOf "error" out && T.isInfixOf "could not resolve" out
    then pure $ Right ()
    else pure $ Left $ "list_files broken symlink: " ++ T.unpack out

-- ---------------------------------------------------------------------------
-- Symlink-loop / visited-directory hardening tests
-- ---------------------------------------------------------------------------

-- | globTool does not hang on a directory symlink loop.
testGlobSymlinkLoop :: Test
testGlobSymlinkLoop = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-glob-symloop"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createDirectory (root </> "a")
  createDirectoryLink (root </> "a") (root </> "a" </> "loop")
  writeFile (root </> "a" </> "real.hs") "module Real where"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*.hs" :: T.Text)])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find real.hs and terminate (not hang)
  if T.isInfixOf "real.hs" out && T.isInfixOf "files match" out
    then pure $ Right ()
    else pure $ Left $ "glob symlink loop: " ++ T.unpack out

-- | searchTool does not hang on a directory symlink loop.
testSearchSymlinkLoop :: Test
testSearchSymlinkLoop = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-search-symloop"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createDirectory (root </> "a")
  createDirectoryLink (root </> "a") (root </> "a" </> "loop")
  writeFile (root </> "a" </> "real.hs") "unique_token_symloop_search"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query" .= ("unique_token_symloop_search" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find the match and terminate (not hang)
  if T.isInfixOf "real.hs:" out && T.isInfixOf "1 matches" out
    then pure $ Right ()
    else pure $ Left $ "search symlink loop: " ++ T.unpack out

-- | globTool skips a revisited canonical directory (two paths to same dir)
--   and reports it in traversal stats.
testGlobRevisitedDir :: Test
testGlobRevisitedDir = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-glob-revisit"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createDirectory (root </> "real")
  writeFile (root </> "real" </> "A.hs") "module A where"
  -- Create a symlink so "real" is reachable via two paths:
  --   real/  and  alias/ -> real/
  createDirectoryLink (root </> "real") (root </> "alias")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*.hs" :: T.Text)])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find A.hs exactly once and report revisited dirs
  if T.isInfixOf "A.hs" out && T.isInfixOf "1 files match" out
     && T.isInfixOf "revisited dirs" out
    then pure $ Right ()
    else pure $ Left $ "glob revisited dir: " ++ T.unpack out

-- | searchTool skips a revisited canonical directory and reports stats.
testSearchRevisitedDir :: Test
testSearchRevisitedDir = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-search-revisit"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createDirectory (root </> "real")
  writeFile (root </> "real" </> "A.hs") "unique_token_revisit_search"
  createDirectoryLink (root </> "real") (root </> "alias")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query" .= ("unique_token_revisit_search" :: T.Text)
    ])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find the match exactly once and report revisited dirs
  if T.isInfixOf "1 matches" out && T.isInfixOf "revisited dirs" out
    then pure $ Right ()
    else pure $ Left $ "search revisited dir: " ++ T.unpack out

-- | globTool follows a normal in-root directory symlink (no loop)
--   and finds files through it.
testGlobNormalSymlinkBehavior :: Test
testGlobNormalSymlinkBehavior = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-glob-normsym"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createDirectory (root </> "src")
  writeFile (root </> "src" </> "Lib.hs") "module Lib where"
  createDirectoryLink (root </> "src") (root </> "link")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*.hs" :: T.Text)])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find Lib.hs once (via real path), not twice.
  -- The symlink dir is revisited, so we expect a revisited stat.
  if T.isInfixOf "Lib.hs" out && T.isInfixOf "1 files match" out
    then pure $ Right ()
    else pure $ Left $ "glob normal symlink: " ++ T.unpack out

-- | .agentignore causes glob to skip a directory BEFORE traversal
--   enters it (so no revisited-dir stat for the ignored subtree).
testGlobAgentIgnoreBeforeTraversal :: Test
testGlobAgentIgnoreBeforeTraversal = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-glob-aibefore"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createDirectory (root </> "src")
  writeFile (root </> "src" </> "A.hs") "module A where"
  createDirectory (root </> "skipme")
  writeFile (root </> "skipme" </> "B.hs") "module B where"
  createDirectoryLink (root </> "src") (root </> "skipme" </> "link")
  writeFile (root </> ".agentignore") "skipme\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*.hs" :: T.Text)])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find A.hs only via src/, skip skipme/ entirely,
  -- and NOT report revisited dirs (agentignore blocked entry).
  if T.isInfixOf "A.hs" out && not (T.isInfixOf "B.hs" out)
     && T.isInfixOf ".agentignore" out
     && not (T.isInfixOf "revisited dirs" out)
    then pure $ Right ()
    else pure $ Left $ "glob agentignore before traversal: " ++ T.unpack out

-- | root containment still blocks outside-root symlinks in glob.
testGlobRootContainmentSymlink :: Test
testGlobRootContainmentSymlink = do
  tmpDir <- getTemporaryDirectory
  let root   = tmpDir </> "haskode-glob-rootcontain"
      target = tmpDir </> "haskode-glob-rootcontain-target"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  createDirectory root
  createDirectory target
  writeFile (target </> "secret.hs") "module Secret where"
  writeFile (root </> "safe.hs") "module Safe where"
  createDirectoryLink target (root </> "outside")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*.hs" :: T.Text)])
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  let out = trOutput result
  -- Should find safe.hs but NOT secret.hs; should report outside root
  if T.isInfixOf "safe.hs" out && not (T.isInfixOf "secret.hs" out)
     && T.isInfixOf "outside" out
    then pure $ Right ()
    else pure $ Left $ "glob root containment symlink: " ++ T.unpack out

-- | formatStats includes revisited dirs when present.
testFormatStatsRevisited :: Test
testFormatStatsRevisited =
  let stats = emptyStats { tsRevisitedDirs = 2 }
      out   = formatStats stats
  in if T.isInfixOf "2 revisited dirs" out
       then pure $ Right ()
       else pure $ Left $ "formatStats revisited: " ++ T.unpack out

-- ---------------------------------------------------------------------------
-- .agentignore tests
-- ---------------------------------------------------------------------------

-- | loadAgentIgnore returns an empty list when no .agentignore file exists.
testLoadAgentIgnoreNoFile :: Test
testLoadAgentIgnoreNoFile = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-agentignore-nofile"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  patterns <- loadAgentIgnore root
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  if null patterns
    then pure $ Right ()
    else pure $ Left $ "Expected empty list, got: " ++ show patterns

-- | loadAgentIgnore correctly parses comments, blanks, and patterns.
testLoadAgentIgnoreParses :: Test
testLoadAgentIgnoreParses = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-agentignore-parse"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> ".agentignore")
    "# this is a comment\n\
    \\n\
    \  \n\
    \build\n\
    \*.log\n\
    \# another comment\n\
    \dist\n"
  patterns <- loadAgentIgnore root
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  if patterns == ["build", "*.log", "dist"]
    then pure $ Right ()
    else pure $ Left $ "Parsed patterns: " ++ show patterns

-- | Without .agentignore, glob preserves existing behavior.
testGlobNoAgentIgnorePreservesBehavior :: Test
testGlobNoAgentIgnorePreservesBehavior = do
  (root, cleanupAction) <- createTestTree
  -- Explicitly remove any stale .agentignore from prior test runs.
  _ <- try (removeFile (root </> ".agentignore")) :: IO (Either IOException ())
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*.hs" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Should find all .hs files, same as before .agentignore existed
  if T.isInfixOf "Main.hs" out && T.isInfixOf "Lib.hs" out
     && T.isInfixOf "5 files match" out
     && not (T.isInfixOf "by .agentignore" out)
    then pure $ Right ()
    else pure $ Left $ "glob no-agentignore: " ++ T.unpack out

-- | .agentignore causes glob to skip a directory.
testGlobAgentIgnoreSkipsDir :: Test
testGlobAgentIgnoreSkipsDir = do
  (root, cleanupAction) <- createTestTree
  -- Add .agentignore that ignores the "test" directory
  writeFile (root </> ".agentignore") "test\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*.hs" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Should find Main.hs, Lib.hs, Util.hs, Inner.hs but NOT test/Spec.hs
  if T.isInfixOf "Main.hs" out && T.isInfixOf "Lib.hs" out
     && not (T.isInfixOf "Spec.hs" out)
     && T.isInfixOf "4 files match" out
     && T.isInfixOf ".agentignore" out
    then pure $ Right ()
    else pure $ Left $ "glob agentignore dir: " ++ T.unpack out

-- | .agentignore causes search to skip a directory.
testSearchAgentIgnoreSkipsDir :: Test
testSearchAgentIgnoreSkipsDir = do
  (root, cleanupAction) <- createTestTree
  -- Add .agentignore that ignores the "test" directory
  writeFile (root </> ".agentignore") "test\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object ["query" .= ("module" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- "module" appears in Main.hs, Lib.hs, Util.hs, Inner.hs, Spec.hs
  -- but Spec.hs should be skipped because test/ is ignored
  if T.isInfixOf "Main.hs:" out && T.isInfixOf "Lib.hs:" out
     && not (T.isInfixOf "Spec.hs:" out)
     && T.isInfixOf ".agentignore" out
    then pure $ Right ()
    else pure $ Left $ "search agentignore dir: " ++ T.unpack out

-- | .agentignore still skips a directory when ignore_case is true.
testSearchAgentIgnoreWithIgnoreCase :: Test
testSearchAgentIgnoreWithIgnoreCase = do
  (root, cleanupAction) <- createTestTree
  writeFile (root </> ".agentignore") "test\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute searchTool (object
    [ "query"       .= ("MODULE" :: T.Text)
    , "ignore_case" .= True
    ])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  if T.isInfixOf "Main.hs:" out && T.isInfixOf "Lib.hs:" out
     && not (T.isInfixOf "Spec.hs:" out)
     && T.isInfixOf ".agentignore" out
     && T.isInfixOf "case-insensitive" out
    then pure $ Right ()
    else pure $ Left $ "search agentignore ignore_case: " ++ T.unpack out

-- | .agentignore with a file pattern skips matching files.
testAgentIgnoreSkipsFile :: Test
testAgentIgnoreSkipsFile = do
  (root, cleanupAction) <- createTestTree
  -- Ignore all .txt files
  writeFile (root </> ".agentignore") "*.txt\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Should find .hs and .md files but NOT data.txt
  if T.isInfixOf "Main.hs" out && not (T.isInfixOf "data.txt" out)
     && T.isInfixOf ".agentignore" out
    then pure $ Right ()
    else pure $ Left $ "agentignore file pattern: " ++ T.unpack out

-- | Comments and blank lines in .agentignore do not cause errors.
testAgentIgnoreCommentsAndBlanks :: Test
testAgentIgnoreCommentsAndBlanks = do
  (root, cleanupAction) <- createTestTree
  writeFile (root </> ".agentignore")
    "# skip test dir\n\n  \ntest\n# end\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*.hs" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- test/ should be skipped; comments should be harmless
  if T.isInfixOf "Main.hs" out && not (T.isInfixOf "Spec.hs" out)
     && T.isInfixOf "4 files match" out
    then pure $ Right ()
    else pure $ Left $ "agentignore comments: " ++ T.unpack out

-- | Built-in ignored dirs (.git) still apply alongside .agentignore.
testAgentIgnoreBuiltInStillApplies :: Test
testAgentIgnoreBuiltInStillApplies = do
  (root, cleanupAction) <- createTestTree
  -- .agentignore ignores "test"; .git is built-in
  writeFile (root </> ".agentignore") "test\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Neither .git nor test should appear
  if not (T.isInfixOf ".git" out) && not (T.isInfixOf "config" out)
     && not (T.isInfixOf "Spec.hs" out)
     && T.isInfixOf "Main.hs" out
    then pure $ Right ()
    else pure $ Left $ "agentignore + built-in: " ++ T.unpack out

-- | Traversal stats correctly report agent-ignore skips.
testAgentIgnoreStats :: Test
testAgentIgnoreStats = do
  (root, cleanupAction) <- createTestTree
  writeFile (root </> ".agentignore") "test\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- toolExecute globTool (object ["pattern" .= ("**/*" :: T.Text)])
  setCurrentDirectory origDir
  cleanupAction
  let out = trOutput result
  -- Stats should mention .agentignore skips
  if T.isInfixOf "by .agentignore" out
    then pure $ Right ()
    else pure $ Left $ "agentignore stats: " ++ T.unpack out

-- | shouldIgnorePath matches single-component patterns against entry names.
testShouldIgnorePathSingleComponent :: Test
testShouldIgnorePathSingleComponent =
  let patterns = ["build", "*.log"]
  in if shouldIgnorePath patterns "build" "src/build"
        && shouldIgnorePath patterns "app.log" "logs/app.log"
        && not (shouldIgnorePath patterns "src" "src")
        && not (shouldIgnorePath patterns "Main.hs" "src/Main.hs")
     then pure $ Right ()
     else pure $ Left "shouldIgnorePath single-component failed"

-- | shouldIgnorePath matches multi-component patterns against relative paths.
testShouldIgnorePathMultiComponent :: Test
testShouldIgnorePathMultiComponent =
  let patterns = ["vendor/*", "src/deep"]
  in if shouldIgnorePath patterns "foo" "vendor/foo"
        && shouldIgnorePath patterns "deep" "src/deep"
        && not (shouldIgnorePath patterns "deep" "other/deep")
     then pure $ Right ()
     else pure $ Left "shouldIgnorePath multi-component failed"

-- ---------------------------------------------------------------------------
-- AGENTS.md tests
-- ---------------------------------------------------------------------------

-- | Without AGENTS.md, buildSystemPrompt produces the same prompt as before.
testBuildSystemPromptNoAgentsMd :: Test
testBuildSystemPromptNoAgentsMd = do
  let prompt = buildSystemPrompt defaultRegistry Nothing
  if T.isInfixOf "helpful coding assistant" prompt
     && T.isInfixOf "Available tools" prompt
     && not (T.isInfixOf "AGENTS.md" prompt)
     && not (T.isInfixOf "Repository instructions" prompt)
    then pure $ Right ()
    else pure $ Left $ "system prompt without AGENTS.md: " ++ T.unpack (T.take 200 prompt)

-- | With AGENTS.md content, the content appears in the system prompt.
testBuildSystemPromptWithAgentsMd :: Test
testBuildSystemPromptWithAgentsMd = do
  let agentsContent = "Always use tabs, not spaces.\nWrite tests first."
      prompt = buildSystemPrompt defaultRegistry (Just agentsContent)
  if T.isInfixOf "Repository instructions" prompt
     && T.isInfixOf "AGENTS.md" prompt
     && T.isInfixOf "Always use tabs" prompt
     && T.isInfixOf "Write tests first" prompt
    then pure $ Right ()
    else pure $ Left $ "system prompt with AGENTS.md: " ++ T.unpack (T.take 300 prompt)

-- | loadAgentsMd returns Nothing when no AGENTS.md exists.
testLoadAgentsMdNoFile :: Test
testLoadAgentsMdNoFile = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-agentsmd-nofile"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- loadAgentsMd
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Nothing -> pure $ Right ()
    Just _  -> pure $ Left "loadAgentsMd should return Nothing when no file"

-- | loadAgentsMd reads content correctly from a valid AGENTS.md.
testLoadAgentsMdContent :: Test
testLoadAgentsMdContent = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-agentsmd-content"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "AGENTS.md") "# Project rules\nUse Haskell.\n"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- loadAgentsMd
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Just txt | T.isInfixOf "Use Haskell" txt -> pure $ Right ()
    other -> pure $ Left $ "loadAgentsMd content: " ++ show other

-- | loadAgentsMd returns Nothing for a broken symlink.
testLoadAgentsMdBrokenSymlink :: Test
testLoadAgentsMdBrokenSymlink = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-agentsmd-broken"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  createFileLink (root </> "nonexistent") (root </> "AGENTS.md")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- loadAgentsMd
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Nothing -> pure $ Right ()
    Just _  -> pure $ Left "loadAgentsMd should return Nothing for broken symlink"

-- | loadAgentsMd returns Nothing for a symlink pointing outside root.
testLoadAgentsMdOutsideRoot :: Test
testLoadAgentsMdOutsideRoot = do
  tmpDir <- getTemporaryDirectory
  let root   = tmpDir </> "haskode-agentsmd-outside"
      target = tmpDir </> "haskode-agentsmd-target"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  createDirectory root
  createDirectory target
  writeFile (target </> "AGENTS.md") "secret instructions"
  createFileLink (target </> "AGENTS.md") (root </> "AGENTS.md")
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  result <- loadAgentsMd
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  _ <- try (removeDirectoryRecursive target) :: IO (Either IOException ())
  case result of
    Nothing -> pure $ Right ()
    Just _  -> pure $ Left "loadAgentsMd should reject outside-root symlink"

tests :: [Test]
tests =
  [ testTruncateTextNoOp
  , testTruncateTextTruncates
  , testTruncateTextExactLimit
  , testTruncateTextEmpty
  , testFormatTruncMetaNoOp
  , testFormatTruncMetaTruncated
  , testMatchGlobSimpleStar
  , testMatchGlobDoubleStar
  , testMatchGlobDirPrefix
  , testMatchGlobExact
  , testIsIgnoredDir
  , testSearchInText
  , testSearchInTextEmptyQuery
  , testSearchInTextNoMatch
  , testSearchInTextIgnoreCase
  , testSearchInTextIgnoreCaseNoMatch
  , testSearchInTextCaseSensitiveDefault
  , testFormatSearchMatch
  , testFormatSearchMatchTruncates
  , testGlobInRegistry
  , testSearchInRegistry
  , testGlobToolSimple
  , testGlobToolRecursive
  , testGlobToolSkipsIgnored
  , testGlobToolDirPrefix
  , testGlobToolNoMatch
  , testSearchToolFindsMatches
  , testSearchToolNoMatch
  , testSearchToolSingleFile
  , testSearchToolSkipsBinary
  , testSearchToolSkipsIgnoredDirs
  , testIsUnderRootExact
  , testIsUnderRootSubdir
  , testIsUnderRootSibling
  , testIsUnderRootOutside
  , testSearchDefaultDir
  , testSearchScopedDir
  , testSearchRejectsOutsideRoot
  , testSearchNotFound
  , testSearchCaseSensitiveDefault
  , testSearchIgnoreCaseTrue
  , testSearchIgnoreCaseScoped
  , testSearchSkipsLargeFile
  , testSearchMaxFileSizePositive
  , testFormatStatsEmpty
  , testFormatStatsMixed
  , testFormatStatsOutside
  , testFormatStatsRevisited
  , testSafeCanonicalizeExisting
  , skipIfNoSymlinks testSafeCanonicalizeBrokenSymlink
  , skipIfNoSymlinks testGlobBrokenSymlink
  , skipIfNoSymlinks testGlobOutsideRootSymlink
  , skipIfNoSymlinks testSearchBrokenSymlink
  , skipIfNoSymlinks testSearchOutsideRootSymlink
  , skipOnWindows testSearchUnreadableDir
  , skipOnWindows testReadFileUnreadable
  , skipOnWindows testListFilesUnreadable
  , testReadFileRejectsDotDotEscape
  , testReadFileRejectsAbsoluteOutsideRoot
  , testReadFileNormalInRoot
  , skipIfNoSymlinks testReadFileRejectsOutsideRootSymlink
  , skipIfNoSymlinks testReadFileHandlesBrokenSymlink
  , testListFilesRejectsDotDotEscape
  , testListFilesRejectsAbsoluteOutsideRoot
  , testListFilesNormalInRoot
  , skipIfNoSymlinks testListFilesRejectsOutsideRootSymlink
  , skipIfNoSymlinks testListFilesHandlesBrokenSymlink
  , skipIfNoSymlinks testGlobSymlinkLoop
  , skipIfNoSymlinks testSearchSymlinkLoop
  , skipIfNoSymlinks testGlobRevisitedDir
  , skipIfNoSymlinks testSearchRevisitedDir
  , skipIfNoSymlinks testGlobNormalSymlinkBehavior
  , skipIfNoSymlinks testGlobAgentIgnoreBeforeTraversal
  , skipIfNoSymlinks testGlobRootContainmentSymlink
  , testLoadAgentIgnoreNoFile
  , testLoadAgentIgnoreParses
  , testGlobNoAgentIgnorePreservesBehavior
  , testGlobAgentIgnoreSkipsDir
  , testSearchAgentIgnoreSkipsDir
  , testSearchAgentIgnoreWithIgnoreCase
  , testAgentIgnoreSkipsFile
  , testAgentIgnoreCommentsAndBlanks
  , testAgentIgnoreBuiltInStillApplies
  , testAgentIgnoreStats
  , testShouldIgnorePathSingleComponent
  , testShouldIgnorePathMultiComponent
  , testBuildSystemPromptNoAgentsMd
  , testBuildSystemPromptWithAgentsMd
  , testLoadAgentsMdNoFile
  , testLoadAgentsMdContent
  , skipIfNoSymlinks testLoadAgentsMdBrokenSymlink
  , skipIfNoSymlinks testLoadAgentsMdOutsideRoot
  ]
