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
  , showDiff
  ) where

import Data.Text       (Text)
import qualified Data.Text as T
import System.IO       (hPutStrLn, stderr)

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

-- | Show a simple unified-style diff (line-based).
--   This is a placeholder; a real implementation would use a proper
--   diff algorithm.
showDiff :: Patch -> Text
showDiff p = T.unlines $
  [ "--- " <> T.pack (patchPath p)
  , "+++ " <> T.pack (patchPath p)
  ] ++ map ("-" <>) oldLines
    ++ map ("+" <>) newLines
  where
    oldLines = T.lines (patchOld p)
    newLines = T.lines (patchNew p)
