{-# LANGUAGE OverloadedStrings #-}

-- | Pure helpers for interactive slash commands.
--
-- These functions handle parsing, lookup, and formatting for the
-- interactive REPL commands.  Keeping them pure makes them easy to
-- test without IO.

module Haskode.Commands
  ( CommandAction (..)
  , CommandResolution (..)
  , CommandSpec (..)
  , CommandRegistry
  , ExtensionCommand (..)
  , DoctorCheck (..)
  , DoctorStatus (..)
  , commandRegistry
  , mergeExtensionCommands
  , parseSlashCommand
  , lookupCommand
  , lookupCommandIn
  , resolveCommandFor
  , resolveCommandActionFor
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

import Data.Char          (isSpace)
import Data.List          (find)
import qualified Data.Set as Set
import Data.Text          (Text)
import qualified Data.Text as T
import System.Directory   (doesFileExist)
import System.Environment (lookupEnv)
import System.FilePath    ((</>))

import Haskode.Agent      (AgentState (..), ContextStats (..), contextStats)
import Haskode.Config     (Config (..), ProviderConfig (..))
import Haskode.Core       (Conversation, emptyConversation)
import Haskode.Provider   (Provider (..))
import Haskode.Provider.Resolve
  ( providerApiKeyEnvVar
  , providerDefaultBaseUrlHint
  , providerKindLabel
  , providerRequiresApiKey
  )
import Haskode.Session    (events)
import Haskode.Tools      (toolNames)

-- ---------------------------------------------------------------------------
-- Command registry
-- ---------------------------------------------------------------------------

-- | The small set of actions currently supported by slash commands.
--   Front ends dispatch on these tags after shared command resolution.
data CommandAction
  = CmdHelp
  | CmdStatus
  | CmdNew
  | CmdCompact
  | CmdExit
  | CmdDoctor
  | CmdExtensionText !Text
  deriving (Eq, Show)

-- | Pure text-only command contributed by a compiled extension.
--
-- This is intentionally narrow: extension commands can only return static
-- text.  They cannot run IO, inspect or mutate agent state, call providers,
-- request exit, or reset the conversation.  Names and aliases are written
-- without a leading slash; aliases are displayed and resolved like ordinary
-- slash-command entries.
data ExtensionCommand = ExtensionCommand
  { extensionCommandName        :: !Text
  , extensionCommandAliases     :: ![Text]
  , extensionCommandDescription :: !Text
  , extensionCommandOutput      :: !Text
  } deriving (Eq, Show)

-- | Metadata for one slash command.
data CommandSpec = CommandSpec
  { cmdName           :: !Text
  , cmdDescription    :: !Text
  , cmdAction         :: !CommandAction
  , cmdAvailableInCli :: !Bool
  , cmdAvailableInTui :: !Bool
  } deriving (Eq, Show)

-- | Result of resolving a parsed slash command for a specific front end.
--
-- The resolver keeps command lookup and availability checks pure and shared,
-- while CLI/TUI layers remain responsible for running effects and rendering.
data CommandResolution
  = CommandResolved !CommandSpec
  | CommandUnknown !Text
  deriving (Eq, Show)

type CommandRegistry = [CommandSpec]

-- | Registered slash commands.
--
-- Keep this small and ordered by help display preference.  @/quit@ is
-- represented as a separate command that shares the 'CmdExit' action
-- with @/exit@ so alias behavior stays explicit and easy to test.
commandRegistry :: CommandRegistry
commandRegistry =
  [ CommandSpec "help"   "show this help"                    CmdHelp   True True
  , CommandSpec "new"    "start a fresh conversation"         CmdNew    True True
  , CommandSpec "compact" "summarize and replace conversation context" CmdCompact True True
  , CommandSpec "status" "show current runtime/config status" CmdStatus True True
  , CommandSpec "doctor" "local diagnostic checks"            CmdDoctor True True
  , CommandSpec "exit"   "save session log and exit"          CmdExit   True True
  , CommandSpec "quit"   "same as /exit"                      CmdExit   True True
  ]

-- | Merge pure text commands contributed by compiled extensions into a
-- command registry.
--
-- Names and aliases share one namespace.  Extension commands cannot replace
-- built-ins, and two extension commands cannot reuse the same name or alias.
-- Aliases become ordinary registry entries so @/help@, CLI dispatch, and TUI
-- dispatch all read the same final registry.
mergeExtensionCommands :: CommandRegistry -> [ExtensionCommand] -> Either Text CommandRegistry
mergeExtensionCommands base extensionCommands = do
  mapM_ validateExtensionCommand extensionCommands
  case duplicatesInOrder (map cmdName base ++ map cmdName contributedSpecs) of
    [] -> Right (base ++ contributedSpecs)
    duplicateNames ->
      Left $
        "duplicate compiled command name/alias: "
        <> T.intercalate ", " duplicateNames
        <> " (extension commands cannot reuse built-in or extension command names/aliases)"
  where
    contributedSpecs = concatMap extensionCommandRegistryEntries extensionCommands

extensionCommandRegistryEntries :: ExtensionCommand -> [CommandSpec]
extensionCommandRegistryEntries extCommand =
  [ mkSpec (extensionCommandName extCommand) ] ++ map mkSpec (extensionCommandAliases extCommand)
  where
    action = CmdExtensionText (extensionCommandOutput extCommand)
    mkSpec name =
      CommandSpec name (extensionCommandDescription extCommand) action True True

validateExtensionCommand :: ExtensionCommand -> Either Text ()
validateExtensionCommand extCommand = do
  validateCommandToken "command name" (extensionCommandName extCommand)
  mapM_ (validateCommandToken "command alias") (extensionCommandAliases extCommand)

validateCommandToken :: Text -> Text -> Either Text ()
validateCommandToken label token
  | T.null token =
      Left ("invalid compiled " <> label <> ": empty")
  | T.isPrefixOf "/" token =
      Left ("invalid compiled " <> label <> ": " <> token <> " (omit the leading slash)")
  | T.any isSpace token =
      Left ("invalid compiled " <> label <> ": " <> token <> " (whitespace is not allowed)")
  | otherwise =
      Right ()

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
  lookupCommandIn commandRegistry name

-- | Look up a slash command in the supplied registry.
lookupCommandIn :: CommandRegistry -> Text -> Maybe CommandSpec
lookupCommandIn registry name =
  find (\spec -> cmdName spec == name) registry

-- | Resolve a command name for a front end.
--
-- Unregistered commands and commands unavailable to the caller both resolve
-- as 'CommandUnknown' so visible behavior stays the same.
resolveCommandFor :: CommandRegistry -> (CommandSpec -> Bool) -> Text -> CommandResolution
resolveCommandFor registry isAvailable name =
  case lookupCommandIn registry name of
    Just spec | isAvailable spec -> CommandResolved spec
    _                            -> CommandUnknown name

-- | Resolve a command name directly to the action a front end should run.
--
-- This keeps lookup and availability checks shared while avoiding duplicated
-- @CommandSpec@ plumbing in CLI/TUI command dispatch.
resolveCommandActionFor :: CommandRegistry -> (CommandSpec -> Bool) -> Text -> Either Text CommandAction
resolveCommandActionFor registry isAvailable name =
  case resolveCommandFor registry isAvailable name of
    CommandResolved spec     -> Right (cmdAction spec)
    CommandUnknown unknown   -> Left unknown

-- ---------------------------------------------------------------------------
-- Help
-- ---------------------------------------------------------------------------

-- | Concise help text for interactive mode.
formatHelpFor :: CommandRegistry -> (CommandSpec -> Bool) -> Text
formatHelpFor registry isAvailable =
  T.unlines $
    "Interactive commands:" : map formatCommand commands
  where
    commands = filter isAvailable registry
    maxNameWidth = maximum (0 : map (T.length . cmdName) commands)
    formatCommand spec =
      "  /"
      <> cmdName spec
      <> T.replicate (maxNameWidth - T.length (cmdName spec) + 2) " "
      <> "\8212 "
      <> cmdDescription spec

formatHelp :: Text
formatHelp = formatHelpFor commandRegistry cmdAvailableInCli

duplicatesInOrder :: Ord a => [a] -> [a]
duplicatesInOrder = go Set.empty Set.empty
  where
    go _seen _reported [] = []
    go seen reported (x:xs)
      | Set.member x seen && Set.notMember x reported =
          x : go seen (Set.insert x reported) xs
      | otherwise =
          go (Set.insert x seen) reported xs

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
  case providerKindLabel name of
    Just kind ->
      [ DoctorCheck "provider" Ok (T.pack name <> " (" <> kind <> ")") ]
    Nothing ->
      [ DoctorCheck "provider" Warn
          (T.pack name <> " (unknown; supported: openai, anthropic, ollama, vllm, litellm, openrouter, stub)") ]

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
  | null url  = [DoctorCheck "base URL" Info (providerDefaultBaseUrlHint provName)]
  | otherwise = [DoctorCheck "base URL" Ok (T.pack url)]

-- | Check API key presence without printing the key value.
--
-- For providers that require a key, checks pcApiKey first (which
-- includes any CLI override), then falls back to the provider-specific
-- environment variable.
checkApiKey :: String -> String -> IO DoctorCheck
checkApiKey provName configKey
  | not (providerRequiresApiKey provName) =
      pure $ DoctorCheck "API key" Ok "not required"
  | not (null configKey) =
      pure $ DoctorCheck "API key" Ok "present"
  | otherwise = do
      mEnvKey <- lookupEnv (providerApiKeyEnvVar provName)
      case mEnvKey of
        Just k | not (null k) ->
          pure $ DoctorCheck "API key" Ok "present (from env)"
        _ ->
          pure $ DoctorCheck "API key" Warn "missing"

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
