{-# LANGUAGE OverloadedStrings #-}

-- | Pure helpers for interactive slash commands.
--
-- These functions handle parsing, lookup, and formatting for the
-- interactive REPL commands.  Keeping them pure makes them easy to
-- test without IO.

module Haskode.Commands
  ( CommandAction (..)
  , CommandSpec (..)
  , DoctorCheck (..)
  , DoctorStatus (..)
  , commandRegistry
  , parseSlashCommand
  , lookupCommand
  , formatHelpFor
  , formatHelp
  , formatStatus
  , formatUnknownCommand
  , formatNewConfirmation
  , resetConversation
  , formatContextUsage
  , formatDoctorChecks
  , formatDoctor
  , doctorChecks
  ) where

import Data.List          (find)
import Data.Text          (Text)
import qualified Data.Text as T
import System.Directory   (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath    ((</>))

import Haskode.Agent      (AgentState (..), ContextStats (..), contextStats)
import Haskode.Config     (Config (..), ProviderConfig (..))
import Haskode.Core       (Conversation, emptyConversation)
import Haskode.Provider   (Provider (..))
import Haskode.Session    (events)
import Haskode.Tools      (toolNames)

-- ---------------------------------------------------------------------------
-- Command registry
-- ---------------------------------------------------------------------------

-- | The small set of actions currently supported by slash commands.
--   The CLI dispatches on these tags; a future TUI can handle the
--   same actions without parsing terminal-specific text.
data CommandAction
  = CmdHelp
  | CmdStatus
  | CmdNew
  | CmdExit
  | CmdDoctor
  deriving (Eq, Show)

-- | Metadata for one slash command.
data CommandSpec = CommandSpec
  { cmdName           :: !Text
  , cmdDescription    :: !Text
  , cmdAction         :: !CommandAction
  , cmdAvailableInCli :: !Bool
  , cmdAvailableInTui :: !Bool
  } deriving (Eq, Show)

-- | Registered slash commands.
--
-- Keep this small and ordered by help display preference.  @/quit@ is
-- represented as a separate command that shares the 'CmdExit' action
-- with @/exit@ so alias behavior stays explicit and easy to test.
commandRegistry :: [CommandSpec]
commandRegistry =
  [ CommandSpec "help"   "show this help"                    CmdHelp   True True
  , CommandSpec "new"    "start a fresh conversation"         CmdNew    True True
  , CommandSpec "status" "show current runtime/config status" CmdStatus True True
  , CommandSpec "doctor" "local diagnostic checks"            CmdDoctor True True
  , CommandSpec "exit"   "save session log and exit"          CmdExit   True True
  , CommandSpec "quit"   "same as /exit"                      CmdExit   True True
  ]

-- ---------------------------------------------------------------------------
-- Command parsing
-- ---------------------------------------------------------------------------

-- | Parse a slash command from user input.
--
--   * @\"\/help\"@  -> @Just \"help\"@
--   * @\"\/foo\"@   -> @Just \"foo\"@
--   * @\"hello\"@   -> @Nothing@
--
-- Leading and trailing whitespace is stripped before checking for the
-- leading @\'\/\'@.
parseSlashCommand :: Text -> Maybe Text
parseSlashCommand input =
  case T.strip input of
    t | T.null t      -> Nothing
      | T.head t == '/' -> Just (T.tail t)
      | otherwise       -> Nothing

-- | Look up a slash command by command name, without the leading slash.
lookupCommand :: Text -> Maybe CommandSpec
lookupCommand name =
  find (\spec -> cmdName spec == name) commandRegistry

-- ---------------------------------------------------------------------------
-- Help
-- ---------------------------------------------------------------------------

-- | Concise help text for interactive mode.
formatHelpFor :: (CommandSpec -> Bool) -> Text
formatHelpFor isAvailable =
  T.unlines $
    "Interactive commands:" : map formatCommand commands
  where
    commands = filter isAvailable commandRegistry
    maxNameWidth = maximum (0 : map (T.length . cmdName) commands)
    formatCommand spec =
      "  /"
      <> cmdName spec
      <> T.replicate (maxNameWidth - T.length (cmdName spec) + 2) " "
      <> "\8212 "
      <> cmdDescription spec

formatHelp :: Text
formatHelp = formatHelpFor cmdAvailableInCli

-- ---------------------------------------------------------------------------
-- Status
-- ---------------------------------------------------------------------------

-- | Format current runtime status.
--
-- Shows provider, model, base URL, working directory, verbose mode,
-- streaming availability, context limits, session event count, and
-- registered tool names.
--
-- API keys are never printed.
formatStatus :: AgentState -> Text
formatStatus st =
  let cfg = asConfig st
      pc  = cfgProvider cfg
      evs = events (asSession st)
      maxC = cfgMaxContextChars cfg
      names = toolNames (asRegistry st)
      disabled = cfgDisabledTools cfg
      hasStream = case providerStream (asProvider st) of
                    Just _  -> ("yes" :: Text)
                    Nothing -> "no"
      disabledLine
        | null disabled = "Disabled tools: none"
        | otherwise     = "Disabled tools: " <> T.intercalate ", " disabled
  in T.unlines
       [ "Provider:        " <> T.pack (pcProvider pc)
       , "Model:           " <> T.pack (pcModel pc)
       , "Base URL:        " <> T.pack (pcBaseUrl pc)
       , "Working dir:     " <> T.pack (cfgWorkingDir cfg)
       , "Verbose:         " <> (if cfgVerbose cfg then "on" else "off")
       , "Streaming:       " <> hasStream
       , "Resumed:         " <> (if asResumed st then "yes" else "no")
       , "Max context:     " <> T.pack (show maxC) <> " chars"
       , formatContextUsage (asConversation st) maxC
       , "Max session log: " <> T.pack (show (cfgMaxSessionLogBytes cfg)) <> " bytes"
       , "Session events:  " <> T.pack (show (length evs))
       , disabledLine
       , "Tools:           " <> T.pack (show (length names))
                             <> " \8212 "
                             <> T.intercalate ", " names
       ]

-- ---------------------------------------------------------------------------
-- Context usage
-- ---------------------------------------------------------------------------

-- | Estimated conversation context usage relative to the configured limit.
--
-- Uses the same character-estimate logic as the context guardrail in
-- "Haskode.Agent".  Shows estimated chars, limit, and remaining headroom
-- (or overage).  This is an estimate, not exact token count.
formatContextUsage :: Conversation -> Int -> Text
formatContextUsage conv maxChars =
  let stats    = contextStats conv maxChars
      overLine
        | csRemaining stats < 0 =
            "Over limit:     " <> T.pack (show (negate (csRemaining stats))) <> " chars"
        | otherwise =
            "Remaining:      " <> T.pack (show (csRemaining stats)) <> " chars"
  in T.unlines
       [ "Context est:    " <> T.pack (show (csCurrent stats)) <> " chars ("
                            <> T.pack (show (csPercent stats)) <> "% used)"
       , overLine
       ]

-- ---------------------------------------------------------------------------
-- Unknown command
-- ---------------------------------------------------------------------------

-- | Format a message for an unrecognized slash command.
formatUnknownCommand :: Text -> Text
formatUnknownCommand cmd =
  "Unknown command: /" <> cmd <> ". Type /help for commands."

-- ---------------------------------------------------------------------------
-- /new
-- ---------------------------------------------------------------------------

-- | Confirmation message printed after /new resets the conversation.
formatNewConfirmation :: Text
formatNewConfirmation = "Started a fresh conversation."

-- | Reset an agent state's conversation to empty.
--
-- Session events are left untouched; only the in-memory conversation
-- is cleared so the next user prompt starts a fresh agent context.
resetConversation :: AgentState -> AgentState
resetConversation st = st { asConversation = emptyConversation }

-- ---------------------------------------------------------------------------
-- Doctor
-- ---------------------------------------------------------------------------

-- | Severity tag for a doctor check.
data DoctorStatus = Ok | Warn | Info
  deriving (Eq, Show)

-- | A single doctor diagnostic check.
data DoctorCheck = DoctorCheck
  { dcLabel  :: !Text
  , dcStatus :: !DoctorStatus
  , dcDetail :: !Text
  } deriving (Eq, Show)

-- | Format a list of doctor checks as plain text.
--
-- Pure; easy to test.
--
-- >>> formatDoctorChecks [DoctorCheck "provider" Ok "openai (hosted)"]
-- "[ok] provider: openai (hosted)\n"
formatDoctorChecks :: [DoctorCheck] -> Text
formatDoctorChecks checks = T.unlines (map formatOne checks)
  where
    formatOne c = statusTag (dcStatus c) <> " " <> dcLabel c <> ": " <> dcDetail c
    statusTag Ok   = "[ok]"
    statusTag Warn = "[warn]"
    statusTag Info = "[info]"

-- | Run local diagnostic checks and format the result.
--
-- Read-only: does not contact providers, run tools, write files,
-- or execute shell commands.  API keys are never printed.
formatDoctor :: AgentState -> IO Text
formatDoctor st = fmap formatDoctorChecks (doctorChecks st)

-- | Build the list of doctor checks from the current agent state.
--
-- Checks performed:
--
--   * Provider name recognized
--   * Provider kind (local vs hosted)
--   * Model value present
--   * Base URL value or provider default
--   * API key presence\/absence (never the value)
--   * Working directory
--   * @SYSTEM.md@ presence
--   * @AGENTS.md@ presence
--   * Disabled tools
--   * Available tools count
--   * Session log path and max size
doctorChecks :: AgentState -> IO [DoctorCheck]
doctorChecks st = do
  let cfg = asConfig st
      pc  = cfgProvider cfg
      provName = pcProvider pc
      reg = asRegistry st
      disabled = cfgDisabledTools cfg
      names = toolNames reg

  let provCheck = checkProvider provName
      modelCheck = checkModel (pcModel pc)
      baseUrlCheck = checkBaseUrl provName (pcBaseUrl pc)
  keyCheck    <- checkApiKey provName (pcApiKey pc)
  let workDirCheck = checkWorkingDir (cfgWorkingDir cfg)
  systemMdCheck <- checkFileExists "SYSTEM.md" (cfgWorkingDir cfg) "SYSTEM.md"
  agentsMdCheck <- checkFileExists "AGENTS.md" (cfgWorkingDir cfg) "AGENTS.md"
  let disabledCheck = checkDisabledTools disabled
      toolsCheck = checkAvailableTools names
      sessionCheck = checkSessionConfig cfg

  pure $ provCheck ++ modelCheck ++ baseUrlCheck ++ [keyCheck]
      ++ workDirCheck ++ systemMdCheck ++ agentsMdCheck
      ++ disabledCheck ++ toolsCheck ++ sessionCheck

-- | Check whether the provider name is recognized.
--
-- Returns two checks: recognition status and provider kind.
checkProvider :: String -> [DoctorCheck]
checkProvider name =
  case classifyProvider name of
    (True, kind) ->
      [ DoctorCheck "provider" Ok (T.pack name <> " (" <> kind <> ")") ]
    (False, _) ->
      [ DoctorCheck "provider" Warn
          (T.pack name <> " (unknown; supported: openai, anthropic, ollama, vllm, litellm, openrouter, stub)") ]

-- | Classify a provider name as recognized and its kind.
classifyProvider :: String -> (Bool, Text)
classifyProvider name = case name of
  "stub"       -> (True, "local development")
  "ollama"     -> (True, "local model server")
  "vllm"       -> (True, "local model server")
  "openai"     -> (True, "hosted")
  "litellm"    -> (True, "hosted/proxy")
  "openrouter" -> (True, "hosted/proxy")
  "anthropic"  -> (True, "hosted")
  _            -> (False, "unknown")

-- | Whether a provider requires an API key.
providerRequiresKey :: String -> Bool
providerRequiresKey name = name `notElem` ["stub", "ollama", "vllm"]

-- | Check whether the model field is non-empty.
checkModel :: String -> [DoctorCheck]
checkModel model
  | null model = [DoctorCheck "model" Warn "empty"]
  | otherwise  = [DoctorCheck "model" Ok (T.pack model)]

-- | Check the base URL configuration.
--
-- Reports the configured URL or notes that the provider default will be used.
checkBaseUrl :: String -> String -> [DoctorCheck]
checkBaseUrl provName url
  | null url  = [DoctorCheck "base URL" Info (defaultBaseUrlHint provName)]
  | otherwise = [DoctorCheck "base URL" Ok (T.pack url)]

-- | Hint text for the default base URL of a known provider.
defaultBaseUrlHint :: String -> Text
defaultBaseUrlHint name = case name of
  "openai"     -> "provider default (https://api.openai.com)"
  "anthropic"  -> "provider default (https://api.anthropic.com)"
  "ollama"     -> "provider default (http://localhost:11434)"
  "vllm"       -> "provider default (http://localhost:8000)"
  "litellm"    -> "provider default (http://localhost:4000)"
  "openrouter" -> "provider default (https://openrouter.ai/api)"
  "stub"       -> "not needed"
  _            -> "provider default"

-- | Check API key presence without printing the key value.
--
-- For providers that require a key, checks pcApiKey first (which
-- includes any CLI override), then falls back to the provider-specific
-- environment variable.
checkApiKey :: String -> String -> IO DoctorCheck
checkApiKey provName configKey
  | not (providerRequiresKey provName) =
      pure $ DoctorCheck "API key" Ok "not required"
  | not (null configKey) =
      pure $ DoctorCheck "API key" Ok "present"
  | otherwise = do
      mEnvKey <- lookupEnv (envVarForProvider provName)
      case mEnvKey of
        Just k | not (null k) ->
          pure $ DoctorCheck "API key" Ok "present (from env)"
        _ ->
          pure $ DoctorCheck "API key" Warn "missing"

-- | The environment variable name for a provider's API key.
envVarForProvider :: String -> String
envVarForProvider "anthropic" = "ANTHROPIC_API_KEY"
envVarForProvider _           = "OPENAI_API_KEY"

-- | Check the working directory.
checkWorkingDir :: FilePath -> [DoctorCheck]
checkWorkingDir dir =
  [DoctorCheck "working dir" Ok (T.pack dir)]

-- | Check whether a file exists in the working directory.
checkFileExists :: Text -> FilePath -> FilePath -> IO [DoctorCheck]
checkFileExists label dir filename = do
  let path = dir </> filename
  exists <- doesFileExist path
  pure [DoctorCheck label (if exists then Ok else Info)
          (if exists then "found" else "not found")]

-- | Report disabled tools.
checkDisabledTools :: [Text] -> [DoctorCheck]
checkDisabledTools disabled
  | null disabled = [DoctorCheck "disabled tools" Info "none"]
  | otherwise     = [DoctorCheck "disabled tools" Ok
                       (T.intercalate ", " disabled)]

-- | Report available tool count and names.
checkAvailableTools :: [Text] -> [DoctorCheck]
checkAvailableTools names =
  [DoctorCheck "available tools" Ok
    (T.pack (show (length names)) <> " \8212 " <> T.intercalate ", " names)]

-- | Report session log configuration.
checkSessionConfig :: Config -> [DoctorCheck]
checkSessionConfig cfg =
  let dir = cfgWorkingDir cfg
      maxB = cfgMaxSessionLogBytes cfg
      logPath = dir </> "session.jsonl"
  in [DoctorCheck "session log" Info
        (T.pack logPath <> " (max " <> T.pack (show maxB) <> " bytes)")]
