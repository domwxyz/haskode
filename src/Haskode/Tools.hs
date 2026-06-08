{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Tool registry.
--
-- Tools are the mechanism by which the LLM interacts with the outside
-- world: reading files, writing files, running shell commands, etc.
--
-- The registry is a simple map from tool name to 'Tool' record.  Each
-- tool has:
--   * A name (used in tool-call requests from the LLM)
--   * A JSON schema describing its parameters (for prompt injection)
--   * An execute function that takes JSON args and returns text output
--
-- This module intentionally avoids advanced type-level tricks.
-- The goal is clarity over cleverness.

module Haskode.Tools
  ( -- * Tool type
    Tool (..)
    -- * Registry
  , ToolRegistry
  , emptyRegistry
  , registerTool
  , lookupTool
  , toolNames
    -- * Built-in tools
  , readFileTool
  , listFilesTool
  , shellTool
  , globTool
  , searchTool
  , previewPatchTool
  , applyPatchTool
  , writeFileTool
  , defaultRegistry
    -- * Helpers
  , extractTextField
  , extractBoolField
  , computePatchPreview
  , computeWriteFilePreview
    -- * Shell output truncation
  , TruncResult (..)
  , truncateText
  , formatTruncMeta
    -- * Glob helpers (pure)
  , matchGlob
  , isIgnoredDir
    -- * Agent-ignore support
  , loadAgentIgnore
  , shouldIgnorePath
    -- * Search helpers (pure)
  , searchInText
  , formatSearchMatch
  , isUnderRoot
  , searchMaxFileSize
    -- * Traversal stats
  , TraversalStats (..)
  , emptyStats
  , formatStats
  , safeCanonicalize
  ) where

import Control.Exception (IOException, try)
import Control.Monad     (foldM)
import Data.Aeson       (Value (..), object, (.=))
import qualified Data.Aeson.Key    as Key
import qualified Data.Aeson.KeyMap as KM
import Data.List        (isPrefixOf, tails)
import Data.Map.Strict  (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text        (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (canonicalizePath, doesDirectoryExist, doesFileExist, doesPathExist, getFileSize, listDirectory)
import System.FilePath  ((</>), takeExtension)
import System.Process   (readCreateProcessWithExitCode, shell)

import Haskode.Core (ToolResult (..))
import Haskode.Patch (Patch (..), applyPatch, makePatch, showDiff)

-- ---------------------------------------------------------------------------
-- Tool definition
-- ---------------------------------------------------------------------------

-- | A single callable tool.
data Tool = Tool
  { toolName        :: !Text
  , toolDescription :: !Text
  , toolSchema      :: !Value    -- ^ JSON schema for the LLM
  , toolExecute     :: Value -> IO ToolResult
    -- ^ Takes JSON args, returns a result.
    --   The callId is filled in by the caller, not the tool itself.
  }

instance Show Tool where
  show t = "Tool { name = " ++ T.unpack (toolName t) ++ " }"

-- ---------------------------------------------------------------------------
-- Registry
-- ---------------------------------------------------------------------------

-- | A map from tool name to tool definition.
type ToolRegistry = Map Text Tool

emptyRegistry :: ToolRegistry
emptyRegistry = Map.empty

registerTool :: Tool -> ToolRegistry -> ToolRegistry
registerTool t = Map.insert (toolName t) t

lookupTool :: Text -> ToolRegistry -> Maybe Tool
lookupTool = Map.lookup

toolNames :: ToolRegistry -> [Text]
toolNames = Map.keys

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Resolve the working directory root and a target path, verifying
--   that the target is under the root.  Calls @k rootCanon pathCanon@
--   on success.  Returns a 'ToolResult' error on failure (unresolvable
--   root, unresolvable path, or path outside root).
--
--   This eliminates the repeated @safeCanonicalize "."@ + @isUnderRoot@
--   pattern that every file tool must perform.
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

-- | Extract a text field from a JSON object.
--   Returns 'Nothing' if the field is missing or not a string.
extractTextField :: Text -> Value -> Maybe Text
extractTextField key (Object o) =
  case KM.lookup (Key.fromText key) o of
    Just (String s) -> Just s
    _               -> Nothing
extractTextField _ _ = Nothing

-- | Extract a boolean field from a JSON object.
--   Returns 'Nothing' if the field is missing or not a boolean.
extractBoolField :: Text -> Value -> Maybe Bool
extractBoolField key (Object o) =
  case KM.lookup (Key.fromText key) o of
    Just (Bool b) -> Just b
    _             -> Nothing
extractBoolField _ _ = Nothing

-- ---------------------------------------------------------------------------
-- Built-in tools
-- ---------------------------------------------------------------------------

-- | Read a file and return its contents.
readFileTool :: Tool
readFileTool = Tool
  { toolName        = "read_file"
  , toolDescription = "Read the contents of a file. The path must be under the working directory."
  , toolSchema      = object
      [ "type"       .= ("object" :: Text)
      , "properties" .= object
          [ "path" .= object
              [ "type"        .= ("string" :: Text)
              , "description" .= ("Path to the file to read" :: Text)
              ]
          ]
      , "required"   .= (["path"] :: [Text])
      ]
  , toolExecute = \args -> do
      let path = case extractTextField "path" args of
            Just p  -> T.unpack p
            Nothing -> ""
      if null path
        then pure $ ToolResult "" "error: missing required field 'path'"
        else withRootContainment path $ \_rootCanon _pathCanon -> do
          result <- try (TIO.readFile path) :: IO (Either IOException Text)
          case result of
            Left e    -> pure $ ToolResult ""
              ("error reading " <> T.pack path <> ": " <> T.pack (show e))
            Right txt -> pure $ ToolResult "" txt
  }

-- | List files in a directory.
listFilesTool :: Tool
listFilesTool = Tool
  { toolName        = "list_files"
  , toolDescription = "List files and directories in a directory. Defaults to the working directory."
  , toolSchema      = object
      [ "type"       .= ("object" :: Text)
      , "properties" .= object
          [ "dir" .= object
              [ "type"        .= ("string" :: Text)
              , "description" .= ("Directory path to list (default: working directory)" :: Text)
              ]
          ]
      , "required"   .= (["dir"] :: [Text])
      ]
  , toolExecute = \args -> do
      let dir = case extractTextField "dir" args of
            Just d  -> T.unpack d
            Nothing -> "."
      withRootContainment dir $ \_rootCanon _dirCanon -> do
        result <- try (listDirectory dir) :: IO (Either IOException [FilePath])
        case result of
          Left e     -> pure $ ToolResult ""
            ("error listing " <> T.pack dir <> ": " <> T.pack (show e))
          Right files -> pure $ ToolResult "" (T.unlines (map T.pack files))
  }

-- | Maximum characters kept from stdout or stderr.
--   Output beyond this limit is truncated with a metadata line so the
--   LLM knows data was lost.  This prevents enormous command outputs
--   from blowing up the context window.
shellOutputLimit :: Int
shellOutputLimit = 4096

-- | Structured result of truncating a text value.
data TruncResult = TruncResult
  { truncText           :: !Text  -- ^ The (possibly truncated) text
  , truncOriginalLength :: !Int   -- ^ Character count of the original input
  , truncReturnedLength :: !Int   -- ^ Character count of the returned text
  , truncDidTruncate    :: !Bool  -- ^ Whether truncation occurred
  , truncDropped        :: !Int   -- ^ Number of characters removed (0 if not truncated)
  } deriving stock (Show, Eq)

-- | Truncate text to at most @n@ characters.
--   Returns a 'TruncResult' with full metadata regardless of whether
--   truncation occurred.  This is a pure function, easy to test.
truncateText :: Int -> Text -> TruncResult
truncateText limit t
  | len <= limit = TruncResult
      { truncText           = t
      , truncOriginalLength = len
      , truncReturnedLength = len
      , truncDidTruncate    = False
      , truncDropped        = 0
      }
  | otherwise = TruncResult
      { truncText           = T.take limit t
      , truncOriginalLength = len
      , truncReturnedLength = limit
      , truncDidTruncate    = True
      , truncDropped        = len - limit
      }
  where len = T.length t

-- | Format truncation metadata for appending to tool output.
--   Returns an empty string when no truncation occurred, so callers
--   can safely append it unconditionally.
--
--   Example output when truncation occurs:
--   @\"\\n[truncated: returned 4096 of 5000 chars, 904 dropped]\"@
formatTruncMeta :: TruncResult -> Text
formatTruncMeta tr
  | not (truncDidTruncate tr) = ""
  | otherwise =
      "\n[truncated: returned "
        <> T.pack (show (truncReturnedLength tr))
        <> " of " <> T.pack (show (truncOriginalLength tr))
        <> " chars, " <> T.pack (show (truncDropped tr))
        <> " dropped]"

-- | Execute a shell command (dangerous — gated by Policy module).
--
--   The result is structured with clear section markers so the LLM can
--   distinguish exit code, stdout, and stderr.  When stdout or stderr
--   exceeds 'shellOutputLimit' characters, the output is truncated and
--   a @[truncated: returned N of M chars, D dropped]@ metadata line is
--   appended via 'formatTruncMeta'.
shellTool :: Tool
shellTool = Tool
  { toolName        = "shell"
  , toolDescription = "Execute a shell command and return stdout/stderr. Requires user confirmation before execution. Obviously dangerous commands (e.g. rm -rf /) may be denied by policy."
  , toolSchema      = object
      [ "type"       .= ("object" :: Text)
      , "properties" .= object
          [ "command" .= object
              [ "type"        .= ("string" :: Text)
              , "description" .= ("Shell command to execute" :: Text)
              ]
          ]
      , "required"   .= (["command"] :: [Text])
      ]
  , toolExecute = \args -> do
      let cmd = case extractTextField "command" args of
            Just c  -> T.unpack c
            Nothing -> "echo missing-command"
      -- 'shell' uses cmd /c on Windows and sh -c elsewhere,
      -- handling quoting correctly on both platforms.
      (exitCode, stdout', stderr') <- readCreateProcessWithExitCode (shell cmd) ""
      let trStdout  = truncateText shellOutputLimit (T.pack stdout')
          trStderr  = truncateText shellOutputLimit (T.pack stderr')
          output = T.intercalate "\n"
            [ "[exit] " <> T.pack (show exitCode)
            , "[stdout]"
            , truncText trStdout
            , "[stderr]"
            , truncText trStderr
            ] <> formatTruncMeta trStdout <> formatTruncMeta trStderr
      pure $ ToolResult "" output
  }

-- ---------------------------------------------------------------------------
-- Glob pattern matching (pure)
-- ---------------------------------------------------------------------------

-- | Split a string by a delimiter character.
splitOn :: Char -> String -> [String]
splitOn _ [] = [""]
splitOn sep (c:cs)
  | c == sep  = "" : splitOn sep cs
  | otherwise = case splitOn sep cs of
      (x:xs) -> (c:x) : xs
      []     -> [[c]]  -- unreachable

-- | Match a single glob segment (no @\/@) against a string.
--   @*@ matches zero or more characters.
matchGlobSegment :: String -> String -> Bool
matchGlobSegment [] [] = True
matchGlobSegment ('*':ps) str = any (matchGlobSegment ps) (tails str)
matchGlobSegment (p:ps) (s:str) = p == s && matchGlobSegment ps str
matchGlobSegment _ _ = False

-- | Match a glob pattern against a relative file path.
--
--   Supported wildcards:
--
--     * @*@  — matches any characters except @\/@
--     * @**@ — matches any characters including @\/@ (zero or more path segments)
--
--   Examples:
--
--     @matchGlob "*.hs" "Foo.hs"@           = @True@
--     @matchGlob "*.hs" "src\/Foo.hs"@       = @False@
--     @matchGlob "**\/\/*.hs" "src\/Foo.hs"@ = @True@
--     @matchGlob "src\/**\/*.hs" "src\/A\/B.hs"@ = @True@
--
--   On Windows, backslash path separators are normalized to @/@ before matching.
matchGlob :: String -> FilePath -> Bool
matchGlob pattern path =
  let normalizeSep = map (\c -> if c == '\\' then '/' else c)
      patParts  = splitOn '/' pattern
      pathParts = splitOn '/' (normalizeSep path)
  in matchParts patParts pathParts
  where
    matchParts [] [] = True
    matchParts ("**":ps) xs = any (matchParts ps) (tails xs)
    matchParts (p:ps) (x:xs) = matchGlobSegment p x && matchParts ps xs
    matchParts _ _ = False

-- | Directories to skip during recursive traversal.
--   Build artifacts, version control, and caches.
ignoredDirs :: [String]
ignoredDirs = [".git", "dist-newstyle", ".stack-work", "node_modules", "__pycache__"]

-- | Check if a directory name should be skipped during traversal.
isIgnoredDir :: FilePath -> Bool
isIgnoredDir name = name `elem` ignoredDirs

-- ---------------------------------------------------------------------------
-- .agentignore support
-- ---------------------------------------------------------------------------

-- | Load ignore rules from @.agentignore@ in the given directory.
--   Returns an empty list if the file does not exist or is unreadable.
--
--   Syntax (deliberately minimal):
--
--     * Blank lines are ignored.
--     * Lines starting with @#@ are comments.
--     * Everything else is a pattern:
--
--         * Single-component patterns (no @\/@) match the entry name at
--           any depth — e.g. @build@ ignores every directory named
--           @build@, @*.log@ ignores every @.log@ file.
--         * Multi-component patterns (containing @\/@) are matched
--           against the full relative path with 'matchGlob' — e.g.
--           @vendor\/\/*@ matches @vendor\/foo@.
loadAgentIgnore :: FilePath -> IO [String]
loadAgentIgnore dir = do
  let path = dir </> ".agentignore"
  result <- try (TIO.readFile path) :: IO (Either IOException Text)
  case result of
    Left _  -> pure []          -- no file or unreadable — silently ok
    Right txt ->
      pure [ T.unpack line
           | line <- T.lines txt
           , let stripped = T.strip line
           , not (T.null stripped)
           , not (T.isPrefixOf "#" stripped)
           ]

-- | Check whether an entry should be ignored according to the
--   agent-ignore rules.  For single-component patterns the entry name
--   is checked; for multi-component patterns the full relative path is
--   checked via 'matchGlob'.
shouldIgnorePath :: [String] -> FilePath -> FilePath -> Bool
shouldIgnorePath patterns entryName relPath =
  any (\pat -> if '/' `elem` pat
                 then matchGlob pat relPath
                 else matchGlobSegment pat entryName) patterns

-- ---------------------------------------------------------------------------
-- File search (pure helpers)
-- ---------------------------------------------------------------------------

-- | Search text for a query string.  Returns @(lineNumber, line)@ pairs
--   for each line containing the query.  Line numbers are 1-based.
--   Returns no matches for an empty query.
--   When @ignoreCase@ is 'True', matching is case-insensitive.
searchInText :: Bool -> Text -> Text -> [(Int, Text)]
searchInText ignoreCase query body
  | T.null query = []
  | otherwise    = let q = if ignoreCase then T.toLower query else query
                   in [ (n, line)
                      | (n, line) <- zip [1..] (T.lines body)
                      , let haystack = if ignoreCase then T.toLower line else line
                      , q `T.isInfixOf` haystack
                      ]

-- | Format a single search match as @path:lineNum:snippet@.
--   The snippet is truncated to 120 characters.
formatSearchMatch :: (FilePath, Int, Text) -> Text
formatSearchMatch (path, lineNum, line) =
  T.pack path <> ":" <> T.pack (show lineNum) <> ":" <> T.take 120 (T.strip line)

-- ---------------------------------------------------------------------------
-- Glob tool
-- ---------------------------------------------------------------------------

-- | Maximum number of files returned by the glob tool.
globResultLimit :: Int
globResultLimit = 200

-- | List files matching a glob pattern recursively.
--   Starts from @startDir@, skips 'ignoredDirs' and entries matched by
--   the agent-ignore list loaded from @.agentignore@.
--   @root@ is the canonical working directory used for containment checks.
--   Returns @(matches, stats)@ where stats tracks skipped entries.
--   Maintains a visited set of canonical directory paths to prevent
--   infinite traversal from symlink loops or revisiting the same dir.
listFilesByGlob :: FilePath -> String -> FilePath -> [String] -> IO ([FilePath], TraversalStats)
listFilesByGlob root pattern startDir agentIgnore = do
  -- Seed the visited set with the start directory's canonical path.
  startCanon <- safeCanonicalize startDir
  let initVisited = case startCanon of
        Just p  -> Set.singleton p
        Nothing -> Set.empty
  go initVisited ""
  where
    go visited relDir = do
      let absDir = if null relDir then startDir else startDir </> relDir
      result <- try (listDirectory absDir) :: IO (Either IOException [FilePath])
      case result of
        Left _ -> pure ([], emptyStats { tsSkippedUnreadable = 1 })
        Right entries -> do
          (matches, stats, _visFinal) <- foldM (processEntry visited relDir) ([], emptyStats, visited) entries
          pure (matches, stats)

    processEntry _visited relDir (accMatches, accStats, vis) entry = do
      let relPath  = if null relDir then entry else relDir </> entry
          fullPath = startDir </> relPath
      canonResult <- safeCanonicalize fullPath
      case canonResult of
        Nothing ->
          pure (accMatches, accStats { tsSkippedUnreadable = tsSkippedUnreadable accStats + 1 }, vis)
        Just canon
          | not (isUnderRoot root canon) ->
            pure (accMatches, accStats { tsOutsideRoot = tsOutsideRoot accStats + 1 }, vis)
          | otherwise -> do
              isDir <- doesDirectoryExist fullPath
              if isDir
                then if isIgnoredDir entry
                  then pure (accMatches, accStats, vis)
                  else if shouldIgnorePath agentIgnore entry relPath
                    then pure (accMatches, accStats { tsIgnoredByAgent = tsIgnoredByAgent accStats + 1 }, vis)
                    else if Set.member canon vis
                      then pure (accMatches, accStats { tsRevisitedDirs = tsRevisitedDirs accStats + 1 }, vis)
                      else do
                        let vis' = Set.insert canon vis
                        (dirMatches, dirStats) <- go vis' relPath
                        pure (accMatches ++ dirMatches, mergeStats accStats dirStats, vis')
                else if shouldIgnorePath agentIgnore entry relPath
                  then pure (accMatches, accStats { tsIgnoredByAgent = tsIgnoredByAgent accStats + 1 }, vis)
                  else if matchGlob pattern relPath
                    then pure (accMatches ++ [relPath], accStats, vis)
                    else pure (accMatches, accStats, vis)

-- | The @glob@ tool: list files matching a pattern under the working
--   directory.  Supports @*@ (any filename characters) and @**@ (any
--   path).  Returns at most 'globResultLimit' results.
globTool :: Tool
globTool = Tool
  { toolName        = "glob"
  , toolDescription = "List files matching a glob pattern under the working directory (e.g. *.hs, src/**/*.hs, **/*.md). * matches within a single directory, ** matches across directories. Respects .agentignore."
  , toolSchema      = object
      [ "type"       .= ("object" :: Text)
      , "properties" .= object
          [ "pattern" .= object
              [ "type"        .= ("string" :: Text)
              , "description" .= ("Glob pattern to match against file paths" :: Text)
              ]
          ]
      , "required"   .= (["pattern"] :: [Text])
      ]
  , toolExecute = \args -> do
      let pattern = case extractTextField "pattern" args of
            Just p  -> T.unpack p
            Nothing -> ""
      if null pattern
        then pure $ ToolResult "" "error: missing required field 'pattern'"
        else do
          rootCanon <- canonicalizePath "."
          agentIgnore <- loadAgentIgnore "."
          (matches, stats) <- listFilesByGlob rootCanon pattern "." agentIgnore
          let limited   = take globResultLimit matches
              total     = length matches
              truncated = total > globResultLimit
              header    = T.pack (show total) <> " files match \"" <> T.pack pattern <> "\""
              body      = T.unlines (map T.pack limited)
              truncMsg  = if truncated
                then "\n[truncated: showing first " <> T.pack (show globResultLimit)
                     <> " of " <> T.pack (show total) <> " results]"
                else ""
              skipMsg   = formatStats stats
          pure $ ToolResult "" (header <> "\n" <> body <> truncMsg <> skipMsg)
  }

-- ---------------------------------------------------------------------------
-- Search tool
-- ---------------------------------------------------------------------------

-- | Maximum number of matches returned by the search tool.
searchResultLimit :: Int
searchResultLimit = 50

-- | Maximum file size (in bytes) that the search tool will read.
--   Files larger than this are skipped to avoid blowing up memory.
searchMaxFileSize :: Integer
searchMaxFileSize = 1000000

-- | Extensions that indicate binary files — skipped during search.
binaryExtensions :: [String]
binaryExtensions =
  [ ".png", ".jpg", ".jpeg", ".gif", ".bmp", ".ico", ".svg"
  , ".pdf", ".zip", ".tar", ".gz", ".bz2", ".xz", ".7z"
  , ".exe", ".dll", ".so", ".dylib", ".o", ".a"
  , ".woff", ".woff2", ".ttf", ".eot"
  , ".pyc", ".pyo", ".class", ".beam"
  ]

-- | Check if a file is likely binary based on its extension.
isBinaryFile :: FilePath -> Bool
isBinaryFile path = takeExtension path `elem` binaryExtensions

-- | Check whether @path@ is the same as, or a subdirectory of, @root@.
--   Both paths must already be canonicalized.
--   Handles both @/@ and @\\@ separators for Windows compatibility.
isUnderRoot :: FilePath -> FilePath -> Bool
isUnderRoot root path =
  root == path
  || (root ++ "/")  `isPrefixOf` path
  || (root ++ "\\") `isPrefixOf` path

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

-- | Accumulated stats for a search or glob traversal.
--   Tracks entries that were skipped for various reasons.
data TraversalStats = TraversalStats
  { tsSkippedLarge      :: !Int  -- ^ Files skipped due to size limit
  , tsSkippedUnreadable :: !Int  -- ^ Files/dirs skipped (IO errors)
  , tsOutsideRoot       :: !Int  -- ^ Symlinks resolving outside root
  , tsIgnoredByAgent    :: !Int  -- ^ Entries skipped by .agentignore rules
  , tsRevisitedDirs     :: !Int  -- ^ Canonical dirs revisited (symlink loops / convergent paths)
  } deriving stock (Show, Eq)

-- | Zero-valued stats (nothing skipped).
emptyStats :: TraversalStats
emptyStats = TraversalStats 0 0 0 0 0

-- | Combine two stats records by summing each field.
mergeStats :: TraversalStats -> TraversalStats -> TraversalStats
mergeStats a b = TraversalStats
  { tsSkippedLarge      = tsSkippedLarge a  + tsSkippedLarge b
  , tsSkippedUnreadable = tsSkippedUnreadable a + tsSkippedUnreadable b
  , tsOutsideRoot       = tsOutsideRoot a  + tsOutsideRoot b
  , tsIgnoredByAgent    = tsIgnoredByAgent a  + tsIgnoredByAgent b
  , tsRevisitedDirs     = tsRevisitedDirs a  + tsRevisitedDirs b
  }

-- | Format traversal stats as a concise metadata line.
--   Returns an empty string when nothing was skipped.
--
--   Example: @\"\\n[skipped 2 large files, 1 unreadable]\"@
formatStats :: TraversalStats -> Text
formatStats stats =
  let parts  = [ ("large files",         tsSkippedLarge stats)
               , ("unreadable",          tsSkippedUnreadable stats)
               , ("outside project root", tsOutsideRoot stats)
               , ("by .agentignore",     tsIgnoredByAgent stats)
               , ("revisited dirs",      tsRevisitedDirs stats)
               ]
      active = [(d, n) | (d, n) <- parts, n > 0]
  in if null active
       then ""
       else "\n[skipped "
              <> T.intercalate ", " [ T.pack (show n) <> " " <> d | (d, n) <- active ]
              <> "]"

-- | Search a single file for a query string.
--   @root@ is the canonical working directory for containment checks.
--   Catches IO errors and reports them via stats.
searchSingleFile :: FilePath -> Text -> Bool -> FilePath -> IO ([(FilePath, Int, Text)], TraversalStats)
searchSingleFile root query ignoreCase path = do
  canonResult <- safeCanonicalize path
  case canonResult of
    Nothing -> pure ([], emptyStats { tsSkippedUnreadable = 1 })
    Just canon
      | not (isUnderRoot root canon) ->
        pure ([], emptyStats { tsOutsideRoot = 1 })
      | otherwise -> do
          sizeResult <- try (getFileSize path) :: IO (Either IOException Integer)
          case sizeResult of
            Left _ -> pure ([], emptyStats { tsSkippedUnreadable = 1 })
            Right size
              | size > searchMaxFileSize ->
                pure ([], emptyStats { tsSkippedLarge = 1 })
              | otherwise -> do
                  readResult <- try (TIO.readFile path) :: IO (Either IOException Text)
                  case readResult of
                    Left _ -> pure ([], emptyStats { tsSkippedUnreadable = 1 })
                    Right contents ->
                      let matches = map (\(n, line) -> (path, n, line))
                                        (searchInText ignoreCase query contents)
                      in pure (matches, emptyStats)

-- | Recursively search files in a directory for a query string.
--   @root@ is the canonical working directory for containment checks.
--   Skips 'ignoredDirs', entries matched by the agent-ignore list,
--   binary files, large files, and unreadable entries.
--   Maintains a visited set of canonical directory paths to prevent
--   infinite traversal from symlink loops or revisiting the same dir.
searchDirectory :: FilePath -> Text -> Bool -> FilePath -> [String] -> IO ([(FilePath, Int, Text)], TraversalStats)
searchDirectory root query ignoreCase dir agentIgnore = do
  -- Seed the visited set with the search directory's canonical path.
  startCanon <- safeCanonicalize dir
  let initVisited = case startCanon of
        Just p  -> Set.singleton p
        Nothing -> Set.empty
  go initVisited dir
  where
    go visited d = do
      result <- try (listDirectory d) :: IO (Either IOException [FilePath])
      case result of
        Left _ -> pure ([], emptyStats { tsSkippedUnreadable = 1 })
        Right entries -> do
          (matches, stats, _visited') <- foldM (processEntry visited d) ([], emptyStats, visited) entries
          pure (matches, stats)

    processEntry _visited d (accMatches, accStats, vis) entry = do
      let fullPath = d </> entry
          relPath  = makeRelativePath dir fullPath
      canonResult <- safeCanonicalize fullPath
      case canonResult of
        Nothing ->
          pure (accMatches, accStats { tsSkippedUnreadable = tsSkippedUnreadable accStats + 1 }, vis)
        Just canon
          | not (isUnderRoot root canon) ->
            pure (accMatches, accStats { tsOutsideRoot = tsOutsideRoot accStats + 1 }, vis)
          | otherwise -> do
              isDir <- doesDirectoryExist fullPath
              if isDir
                then if isIgnoredDir entry
                  then pure (accMatches, accStats, vis)
                  else if shouldIgnorePath agentIgnore entry relPath
                    then pure (accMatches, accStats { tsIgnoredByAgent = tsIgnoredByAgent accStats + 1 }, vis)
                    else if Set.member canon vis
                      then pure (accMatches, accStats { tsRevisitedDirs = tsRevisitedDirs accStats + 1 }, vis)
                      else do
                        let vis' = Set.insert canon vis
                        (dirMatches, dirStats) <- go vis' fullPath
                        pure (accMatches ++ dirMatches, mergeStats accStats dirStats, vis')
                else if shouldIgnorePath agentIgnore entry relPath
                  then pure (accMatches, accStats { tsIgnoredByAgent = tsIgnoredByAgent accStats + 1 }, vis)
                  else if isBinaryFile entry
                    then pure (accMatches, accStats, vis)
                    else do
                      (fileMatches, fileStats) <- searchSingleFile root query ignoreCase fullPath
                      pure (accMatches ++ fileMatches, mergeStats accStats fileStats, vis)

-- | Compute a relative path from a base directory to a full path.
--   Used to build the path that agent-ignore patterns are matched against.
--   Assumes @fullPath@ is always under @base@ (the caller already
--   verified containment).
makeRelativePath :: FilePath -> FilePath -> FilePath
makeRelativePath base fullPath =
  case stripPrefix (base ++ "/") fullPath of
    Just rel -> rel
    Nothing  -> fullPath   -- fallback: use the path as-is
  where
    stripPrefix [] ys = Just ys
    stripPrefix (x:xs) (y:ys) | x == y = stripPrefix xs ys
    stripPrefix _ _ = Nothing

-- | The @search@ tool: search text files for a query string.
--
--   * Searches the whole directory tree by default.
--   * Accepts an optional @directory@ to scope the search.
--   * Paths outside the working directory are rejected.
--   * Files larger than 1 MB are skipped.
--   * Returns at most 'searchResultLimit' matches.
searchTool :: Tool
searchTool = Tool
  { toolName        = "search"
  , toolDescription = "Search text files for a query string (case-sensitive by default, case-insensitive with ignore_case:true). Returns matching lines with file path, line number, and content. Searches the whole directory tree by default; use the directory parameter to scope. Skips binary files and respects .agentignore."
  , toolSchema      = object
      [ "type"       .= ("object" :: Text)
      , "properties" .= object
          [ "query" .= object
              [ "type"        .= ("string" :: Text)
              , "description" .= ("Text to search for (case-sensitive by default)" :: Text)
              ]
          , "directory" .= object
              [ "type"        .= ("string" :: Text)
              , "description" .= ("Directory to search (default: project root). Must be under the working directory." :: Text)
              ]
          , "ignore_case" .= object
              [ "type"        .= ("boolean" :: Text)
              , "description" .= ("If true, matching is case-insensitive (default: false)" :: Text)
              ]
          ]
      , "required"   .= (["query"] :: [Text])
      ]
  , toolExecute = \args -> do
      let query = case extractTextField "query" args of
            Just q  -> q
            Nothing -> ""
          dir = case extractTextField "directory" args of
            Just d  -> T.unpack d
            Nothing -> "."
          ignoreCase = case extractBoolField "ignore_case" args of
            Just b  -> b
            Nothing -> False
      if T.null query
        then pure $ ToolResult "" "error: missing required field 'query'"
        else do
          -- Resolve the working directory for containment checks.
          rootCanonResult <- safeCanonicalize "."
          case rootCanonResult of
            Nothing -> pure $ ToolResult ""
              "error: could not resolve working directory"
            Just rootCanon -> do
              -- Check existence first so canonicalizePath does not throw.
              isDir  <- doesDirectoryExist dir
              isFile <- doesFileExist dir
              if not isDir && not isFile
                then pure $ ToolResult ""
                  ("error: path not found: " <> T.pack dir)
                else do
                  dirCanonResult <- safeCanonicalize dir
                  case dirCanonResult of
                    Nothing -> pure $ ToolResult ""
                      ("error: could not resolve path: " <> T.pack dir)
                    Just dirCanon
                      | not (isUnderRoot rootCanon dirCanon) ->
                        pure $ ToolResult ""
                          ("error: directory must be under the working directory: "
                           <> T.pack dir)
                      | otherwise -> do
                          agentIgnore <- loadAgentIgnore "."
                          (matches, stats) <- if isDir
                            then searchDirectory rootCanon query ignoreCase dir agentIgnore
                            else searchSingleFile rootCanon query ignoreCase dir
                          let limited   = take searchResultLimit matches
                              total     = length matches
                              truncated = total > searchResultLimit
                              modeTag   = if ignoreCase
                                            then " (case-insensitive)"
                                            else ""
                              header    = T.pack (show total)
                                            <> " matches for \"" <> query <> "\""
                                            <> modeTag
                              body      = T.unlines (map formatSearchMatch limited)
                              truncMsg  = if truncated
                                then "\n[truncated: showing first "
                                       <> T.pack (show searchResultLimit)
                                       <> " of " <> T.pack (show total) <> " results]"
                                else ""
                              skipMsg   = formatStats stats
                          pure $ ToolResult ""
                            (header <> "\n" <> body <> truncMsg <> skipMsg)
  }

-- ---------------------------------------------------------------------------
-- Preview-patch tool (read-only diff preview)
-- ---------------------------------------------------------------------------

-- | Maximum character count for a preview diff.  Diffs larger than
--   this are refused with a conservative message so the output does
--   not flood the model context.
previewDiffLimit :: Int
previewDiffLimit = 8192

-- | Preview a unified diff for a proposed file replacement.
--
--   This tool is strictly read-only: it reads the current file,
--   computes a diff against the proposed replacement text, and
--   returns the diff.  It never modifies the filesystem.
--
--   Uses the same root-containment pattern as 'readFileTool':
--   'safeCanonicalize' + 'isUnderRoot' to prevent symlink escape
--   and path traversal.
previewPatchTool :: Tool
previewPatchTool = Tool
  { toolName        = "preview_patch"
  , toolDescription = "Preview a unified diff for a proposed file replacement without modifying the filesystem. Reads the current file and shows what would change. Use this before apply_patch to review changes. Does NOT write to disk."
  , toolSchema      = object
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
  , toolExecute = \args -> do
      let path = case extractTextField "path" args of
            Just p  -> T.unpack p
            Nothing -> ""
          replacement = case extractTextField "replacement" args of
            Just r  -> r
            Nothing -> ""
      if null path
        then pure $ ToolResult "" "error: missing required field 'path'"
        else withRootContainment path $ \_rootCanon _pathCanon -> do
          -- Read the current file content.
          readResult <- try (TIO.readFile path) :: IO (Either IOException Text)
          case readResult of
            Left e -> pure $ ToolResult ""
              ("error reading " <> T.pack path <> ": " <> T.pack (show e))
            Right current -> do
              -- Compute the diff and apply the size limit.
              let patch = makePatch path current replacement
                  diff  = showDiff patch
              if T.length diff > previewDiffLimit
                then pure $ ToolResult ""
                  ("error: diff too large ("
                   <> T.pack (show (T.length diff))
                   <> " chars, limit "
                   <> T.pack (show previewDiffLimit)
                   <> "). Consider reading the file and making the change manually.")
                else pure $ ToolResult ""
                  ("Diff preview (no files modified):\n" <> diff)
  }

-- | Apply a patch to a single existing in-root file.
--
--   This tool writes the replacement text to disk.  It is gated by
--   the policy system: the default policy treats it as 'AskUser',
--   so the user must confirm before the write happens.
--
--   Accepts the same arguments as 'previewPatchTool' (@path@ and
--   @replacement@) and enforces the same root-containment checks.
--   Returns the unified diff in the result so the user/agent can
--   see exactly what changed.
applyPatchTool :: Tool
applyPatchTool = Tool
  { toolName        = "apply_patch"
  , toolDescription = "Apply a patch to exactly one existing file under the working directory. Reads the current file, writes the replacement content, and returns the diff. Requires user confirmation before writing. Cannot create new files or delete files."
  , toolSchema      = object
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
  , toolExecute = \args -> do
      let path = case extractTextField "path" args of
            Just p  -> T.unpack p
            Nothing -> ""
          replacement = case extractTextField "replacement" args of
            Just r  -> r
            Nothing -> ""
      if null path
        then pure $ ToolResult "" "error: missing required field 'path'"
        else withRootContainment path $ \_rootCanon _pathCanon -> do
          -- Read the current file content.
          readResult <- try (TIO.readFile path) :: IO (Either IOException Text)
          case readResult of
            Left e -> pure $ ToolResult ""
              ("error reading " <> T.pack path <> ": " <> T.pack (show e))
            Right current -> do
              -- Compute the diff for the result message.
              let patch = makePatch path current replacement
                  diff  = showDiff patch
              -- Apply the patch (writes to disk).
              applyResult <- try (applyPatch patch) :: IO (Either IOException Patch)
              case applyResult of
                Left e -> pure $ ToolResult ""
                  ("error writing " <> T.pack path <> ": " <> T.pack (show e))
                Right _ -> pure $ ToolResult ""
                  ("Patch applied:\n" <> diff)
  }

-- | Create a new file under the working directory.
--
--   This tool writes new content to a file that does not yet exist.
--   It is gated by the policy system: the default policy treats it as
--   'AskUser', so the user must confirm before the write happens.
--
--   Accepts @path@ (target file) and @content@ (file content).
--   Enforces the same root-containment checks as 'readFileTool'.
--   Refuses to overwrite existing files or write to directory paths.
--   The parent directory must already exist.
--
--   Returns a concise diff-like preview in the result so the user/agent
--   can see exactly what was created.
writeFileTool :: Tool
writeFileTool = Tool
  { toolName        = "write_file"
  , toolDescription = "Create a new file under the working directory with the given content. Requires user confirmation before writing. Cannot overwrite existing files or create directories. The parent directory must already exist."
  , toolSchema      = object
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
  , toolExecute = \args -> do
      let path = case extractTextField "path" args of
            Just p  -> T.unpack p
            Nothing -> ""
          content = case extractTextField "content" args of
            Just c  -> c
            Nothing -> ""
      if null path
        then pure $ ToolResult "" "error: missing required field 'path'"
        else do
          -- Resolve working directory for containment (same as read_file).
          rootCanonResult <- safeCanonicalize "."
          case rootCanonResult of
            Nothing -> pure $ ToolResult ""
              "error: could not resolve working directory"
            Just rootCanon -> do
              -- Check if the target path already exists (file or dir).
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
                  -- The path does not exist yet, so canonicalizePath
                  -- will not resolve it.  Instead, canonicalize the
                  -- parent directory and verify containment.
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
                        -- Verify the resolved parent is a directory.
                        parentIsDir <- doesDirectoryExist parentDir
                        if not parentIsDir
                          then pure $ ToolResult ""
                            ("error: parent path is not a directory: "
                             <> T.pack parentDir)
                          else do
                            -- Build the diff preview for the result.
                            let preview = "--- (new file)\n+++ " <> T.pack path <> "\n"
                                        <> T.unlines (map ("+" <>) (T.lines content))
                            -- Write the file.
                            writeResult <- try (TIO.writeFile path content)
                                           :: IO (Either IOException ())
                            case writeResult of
                              Left e -> pure $ ToolResult ""
                                ("error writing " <> T.pack path <> ": "
                                 <> T.pack (show e))
                              Right _ -> pure $ ToolResult ""
                                ("File created:\n" <> preview)
  }

-- | Compute a preview for a @write_file@ tool call.
--   Returns @(path, preview)@ on success, or an error message on failure.
--   This is used by the agent to show the user a preview before asking
--   for confirmation.
--
--   This function does NOT write to disk.
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
                      else do
                        let preview = "--- (new file)\n+++ " <> T.pack path <> "\n"
                                    <> T.unlines (map ("+" <>) (T.lines content))
                        pure $ Right (path, preview)

-- | Take the parent directory of a path.
--   Returns \".\" if the path has no directory component.
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

-- | Compute a patch preview for a tool call that looks like an
--   @apply_patch@ invocation.  Returns @(path, diff)@ on success,
--   or an error message on failure.  This is used by the agent to
--   show the user a diff preview before asking for confirmation.
--
--   This function does NOT write to disk — it reuses 'makePatch' and
--   'showDiff' from "Haskode.Patch" for the diff computation.
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

-- | Default registry with all built-in tools.
defaultRegistry :: ToolRegistry
defaultRegistry = foldr registerTool emptyRegistry
  [ readFileTool
  , listFilesTool
  , shellTool
  , globTool
  , searchTool
  , previewPatchTool
  , applyPatchTool
  , writeFileTool
  ]
