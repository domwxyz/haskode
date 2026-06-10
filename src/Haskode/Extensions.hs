-- | Compiled extension registration point.
--
-- This is the single place to register extensions in a fork.  To add a
-- compiled extension:
--
--   1. Create a module (e.g. @MyExtension.hs@) that exports an
--      'Extension' value with a unique name, optional tools, optional policy
--      rules for those tools, and optional pure text slash commands.
--   2. Import it here and add it to the 'compiledExtensions' list.
--   3. Rebuild with @cabal build all@.
--
-- Duplicate extension names, duplicate tool names (across built-ins and
-- extensions), and duplicate command names or aliases are rejected at startup
-- with a clear error.  Extension policy rules are combined with the default
-- policy for enabled extension tools; disabled extension tools are not
-- advertised or executable.  Extension commands are pure text only: they
-- cannot run IO, mutate state, inspect tools, call providers, add display
-- hooks, reset, or exit.
--
-- There is no runtime plugin system, no config-based code loading, and no
-- external executable tool or command mechanism.  Extensions are ordinary
-- Haskell values compiled into the binary.
--
-- The default public build intentionally registers no extensions.
module Haskode.Extensions
  ( compiledExtensions
  ) where

import Haskode.Extension (Extension)

-- | Extensions compiled into this build.
--
-- Edit this list to register your fork's extensions.  The default is
-- empty; every entry must be an imported 'Extension' value with a
-- unique 'extensionName'.
compiledExtensions :: [Extension]
compiledExtensions = []
