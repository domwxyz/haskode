{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Source-level extension seam.
--
-- Extensions are ordinary Haskell values compiled into Haskode.  They can
-- contribute tools and policy rules to the same paths used by built-ins;
-- there is no dynamic loading, no separate extension runtime, and no
-- provider extension surface.
--
-- Register extensions in "Haskode.Extensions".
module Haskode.Extension
  ( Extension (..)
  , mergeExtensionTools
  , buildFinalToolRegistry
  , buildFinalPolicy
  , buildFinalRuntime
  ) where

import Control.Monad    (foldM)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Text        (Text)
import qualified Data.Text as T

import Haskode.Core
  ( ToolCall (..)
  )
import Haskode.Tools
  ( Tool (..)
  , ToolRegistry
  , defaultRegistry
  , disableTools
  , registerTool
  )
import Haskode.Policy
  ( Policy
  , Rule
  )

-- | A statically compiled Haskode extension.
--
-- Each extension has a unique name, a human-readable description, and a
-- list of tools and policy rules it contributes.  Tool names are global:
-- they cannot collide with built-in tools or tools from other extensions.
data Extension = Extension
  { extensionName        :: !Text   -- ^ Unique extension identifier (for error messages)
  , extensionDescription :: !Text   -- ^ Human-readable description
  , extensionTools       :: ![Tool] -- ^ Tools contributed by this extension
  , extensionPolicyRules :: ![Rule] -- ^ Policy rules for this extension's enabled tools
  }

instance Show Extension where
  show ext =
    "Extension { extensionName = "
    ++ show (extensionName ext)
    ++ ", extensionDescription = "
    ++ show (extensionDescription ext)
    ++ ", extensionTools = "
    ++ show (extensionTools ext)
    ++ ", extensionPolicyRules = "
    ++ show (length (extensionPolicyRules ext))
    ++ " rule(s) }"

-- | Merge extension tools into a base registry.
--
-- Tool names are global.  An extension tool cannot replace a built-in tool,
-- and two extensions cannot contribute the same tool name.
mergeExtensionTools :: ToolRegistry -> [Extension] -> Either Text ToolRegistry
mergeExtensionTools base extensions = do
  rejectDuplicateExtensionNames extensions
  foldM mergeExtension base extensions
  where
    mergeExtension reg ext =
      foldM (registerExtensionTool (extensionName ext)) reg (extensionTools ext)

    registerExtensionTool extName reg tool
      | Map.member name reg =
          Left $
            "duplicate compiled tool name: "
            <> name
            <> " (from extension "
            <> extName
            <> ")"
      | otherwise = Right (registerTool tool reg)
      where
        name = toolName tool

-- | Build the final enabled tool registry for startup.
--
-- Configured disabled tools are applied after built-ins and extension tools
-- are merged, so the setting covers every compiled tool name.
buildFinalToolRegistry :: [Extension] -> [Text] -> Either Text ToolRegistry
buildFinalToolRegistry extensions disabledTools = do
  merged <- mergeExtensionTools defaultRegistry extensions
  disableTools disabledTools merged

-- | Build the final policy used by the agent.
--
-- Merge order is conservative: built-in/default policy rules run first,
-- then extension rules run only for tools contributed by their own extension
-- and still present in the final enabled registry.  Built-in denials cannot
-- be replaced by an extension rule, disabled extension tools lose their
-- extension policy rules, and no-match calls still fall through to the
-- default 'AskUser' decision from the policy checker.
buildFinalPolicy :: Policy -> ToolRegistry -> [Extension] -> Policy
buildFinalPolicy basePolicy finalRegistry extensions =
  basePolicy ++ concatMap scopedExtensionRules extensions
  where
    scopedExtensionRules ext =
      let enabledNames =
            Set.fromList
              [ name
              | tool <- extensionTools ext
              , let name = toolName tool
              , Map.member name finalRegistry
              ]
      in map (scopeRule enabledNames) (extensionPolicyRules ext)

    scopeRule enabledNames rule tc
      | Set.member (tcName tc) enabledNames = rule tc
      | otherwise = Nothing

-- | Build the final enabled tool registry and final agent policy together.
--
-- Startup should use this helper so providers and the agent state are built
-- from the same finalized extension view.
buildFinalRuntime :: Policy -> [Extension] -> [Text] -> Either Text (ToolRegistry, Policy)
buildFinalRuntime basePolicy extensions disabledTools = do
  registry <- buildFinalToolRegistry extensions disabledTools
  pure (registry, buildFinalPolicy basePolicy registry extensions)

-- | Reject duplicate extension names early, before any tools are merged.
--   Returns the duplicate names in first-seen order.
rejectDuplicateExtensionNames :: [Extension] -> Either Text ()
rejectDuplicateExtensionNames extensions =
  case duplicatesInOrder (map extensionName extensions) of
    [] -> Right ()
    duplicateNames ->
      Left $
        "duplicate extension name(s): "
        <> T.intercalate ", " duplicateNames

duplicatesInOrder :: Ord a => [a] -> [a]
duplicatesInOrder = go Set.empty Set.empty
  where
    go _seen _reported [] = []
    go seen reported (x:xs)
      | Set.member x seen && Set.notMember x reported =
          x : go seen (Set.insert x reported) xs
      | otherwise =
          go (Set.insert x seen) reported xs
