{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Policy and permission gate.
--
-- Before executing any tool call the agent must consult the policy.
-- This prevents the LLM from, say, deleting the user's home directory
-- without asking.
--
-- Policies are composable: we combine a list of 'Rule' values into a
-- single 'Policy'.  The first rule that matches decides the outcome.
--
-- Design goals:
--   * Keep the policy language dead simple (pattern matching on tool
--     name + args).
--   * Make it easy to add interactive confirmation later.
--   * Log every decision for auditability (via the Session module).

module Haskode.Policy
  ( -- * Policy types
    Decision (..)
  , Rule
  , Policy
    -- * Policy construction
  , defaultPolicy
  , checkPolicy
    -- * Built-in rules
  , allowReads
  , denyDangerous
  ) where

import Data.Aeson       (Value (..))
import qualified Data.Aeson.Key    as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Text        (Text)
import qualified Data.Text as T

import Haskode.Core     (ToolCall (..))

-- ---------------------------------------------------------------------------
-- Decision
-- ---------------------------------------------------------------------------

-- | The outcome of a policy check.
data Decision
  = Allow             -- ^ Proceed without user interaction
  | Deny !Text        -- ^ Refuse; the text explains why
  | AskUser !Text     -- ^ Prompt the user for confirmation
  deriving stock (Show, Eq)

-- ---------------------------------------------------------------------------
-- Rules
-- ---------------------------------------------------------------------------

-- | A single policy rule.  Returns 'Nothing' when it does not apply.
type Rule = ToolCall -> Maybe Decision

-- | A policy is an ordered list of rules.
type Policy = [Rule]

-- | Check a tool call against a policy.  If no rule matches, the
--   default is to ask the user (conservative default).
checkPolicy :: Policy -> ToolCall -> Decision
checkPolicy []     _tc = AskUser "no policy rule matched; confirmation required"
checkPolicy (r:rs) tc  = case r tc of
  Just dec -> dec
  Nothing  -> checkPolicy rs tc

-- ---------------------------------------------------------------------------
-- Helpers
-- ---------------------------------------------------------------------------

-- | Try to extract a text field from the tool-call args.
--   Handles both structured JSON objects (the normal case) and
--   bare JSON strings (for backward compatibility).
extractArgText :: Text -> ToolCall -> Maybe Text
extractArgText key tc = case tcArgs tc of
  Object o -> case KM.lookup (Key.fromText key) o of
    Just (String s) -> Just s
    _               -> Nothing
  String s -> Just s
  _        -> Nothing

-- ---------------------------------------------------------------------------
-- Built-in rules
-- ---------------------------------------------------------------------------

-- | Allow all read-only tools.
allowReads :: Rule
allowReads tc
  | tcName tc `elem` ["read_file", "list_files", "search", "glob", "preview_patch", "preview_patch_batch"] = Just Allow
  | otherwise = Nothing

-- | Deny obviously dangerous commands.
--   Matches shell commands containing dangerous patterns.
--   Supports both structured args @{"command": "..."}@ and bare strings.
denyDangerous :: Rule
denyDangerous tc
  | tcName tc == "shell" = case extractArgText "command" tc of
      Just cmd
        | any (`T.isInfixOf` cmd) ["rm -rf /", "mkfs", "dd if="]
          -> Just $ Deny "dangerous shell command blocked"
      _ -> Nothing
  | otherwise = Nothing

-- | The default policy shipped with Haskode.
defaultPolicy :: Policy
defaultPolicy =
  [ allowReads
  , denyDangerous
  -- Everything else falls through to AskUser
  ]
