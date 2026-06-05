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
  , defaultRegistry
    -- * Helpers
  , extractTextField
  ) where

import Data.Aeson       (Value (..), object, (.=))
import qualified Data.Aeson.Key    as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Map.Strict  (Map)
import qualified Data.Map.Strict as Map
import Data.Text        (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Directory (doesFileExist, listDirectory)
import System.Process   (readProcessWithExitCode)

import Haskode.Core (ToolResult (..))

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

-- | Extract a text field from a JSON object.
--   Returns 'Nothing' if the field is missing or not a string.
extractTextField :: Text -> Value -> Maybe Text
extractTextField key (Object o) =
  case KM.lookup (Key.fromText key) o of
    Just (String s) -> Just s
    _               -> Nothing
extractTextField _ _ = Nothing

-- ---------------------------------------------------------------------------
-- Built-in tools
-- ---------------------------------------------------------------------------

-- | Read a file and return its contents.
readFileTool :: Tool
readFileTool = Tool
  { toolName        = "read_file"
  , toolDescription = "Read the contents of a file"
  , toolSchema      = object
      [ "type"       .= ("object" :: Text)
      , "properties" .= object
          [ "path" .= object [ "type" .= ("string" :: Text) ] ]
      , "required"   .= (["path"] :: [Text])
      ]
  , toolExecute = \args -> do
      let path = case extractTextField "path" args of
            Just p  -> T.unpack p
            Nothing -> ""
      if null path
        then pure $ ToolResult "" "error: missing required field 'path'"
        else do
          exists <- doesFileExist path
          if exists
            then do
              contents <- TIO.readFile path
              pure $ ToolResult "" contents
            else pure $ ToolResult "" ("File not found: " <> T.pack path)
  }

-- | List files in a directory.
listFilesTool :: Tool
listFilesTool = Tool
  { toolName        = "list_files"
  , toolDescription = "List files in a directory"
  , toolSchema      = object
      [ "type"       .= ("object" :: Text)
      , "properties" .= object
          [ "dir" .= object [ "type" .= ("string" :: Text) ] ]
      , "required"   .= (["dir"] :: [Text])
      ]
  , toolExecute = \args -> do
      let dir = case extractTextField "dir" args of
            Just d  -> T.unpack d
            Nothing -> "."
      files <- listDirectory dir
      pure $ ToolResult "" (T.unlines (map T.pack files))
  }

-- | Execute a shell command (dangerous — gated by Policy module).
shellTool :: Tool
shellTool = Tool
  { toolName        = "shell"
  , toolDescription = "Execute a shell command and return stdout/stderr"
  , toolSchema      = object
      [ "type"       .= ("object" :: Text)
      , "properties" .= object
          [ "command" .= object [ "type" .= ("string" :: Text) ] ]
      , "required"   .= (["command"] :: [Text])
      ]
  , toolExecute = \args -> do
      let cmd = case extractTextField "command" args of
            Just c  -> T.unpack c
            Nothing -> "echo missing-command"
      (exitCode, stdout', stderr') <- readProcessWithExitCode "sh" ["-c", cmd] ""
      let output = T.pack $ "exit: " ++ show exitCode
                ++ "\nstdout:\n" ++ stdout'
                ++ "\nstderr:\n" ++ stderr'
      pure $ ToolResult "" output
  }

-- | Default registry with all built-in tools.
defaultRegistry :: ToolRegistry
defaultRegistry = foldr registerTool emptyRegistry
  [ readFileTool
  , listFilesTool
  , shellTool
  ]
