{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Haskode CLI entry point.
--
-- Usage:
--
-- @
--   haskode                                    Start an interactive session (stub provider)
--   haskode --prompt "..."                     Send a single prompt and exit
--   haskode --provider openai --prompt "..."   Use a real OpenAI-compatible provider
--   haskode --help                             Show help
-- @
--
-- Provider selection:
--
--   The provider is chosen from the config file's @pcProvider@ field,
--   overridden by @\-\-provider@ on the command line.  Supported values:
--
--     * @openai@, @ollama@, @vllm@, @litellm@, @openrouter@ — any
--       OpenAI-compatible @\/v1\/chat\/completions@ endpoint
--     * @anthropic@ — Anthropic Messages API
--     * @stub@ — local echo provider (for development and testing)
--
-- All other values produce a clear error message.

module Main (main) where

import Control.Monad      (unless, when)
import Data.Text          (Text)
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Exit        (exitFailure, exitSuccess)
import System.FilePath    ((</>))
import System.IO          (hFlush, stdout, hSetBuffering, stdin, BufferMode (..))

import Haskode.Commands        (CommandAction (..), CommandSpec (..), parseSlashCommand, lookupCommand,
                                formatHelp, formatStatus, formatDoctor, formatUnknownCommand, formatNewConfirmation, resetConversation)
import Haskode.Config          (Config (..), ProviderConfig (..), loadConfig, loadConfigFrom)
import Haskode.Display         (formatVerbose)
import Haskode.Provider        (Provider, stubProvider)
import Haskode.Provider.Anthropic (anthropicProvider)
import Haskode.Provider.OpenAI (openaiProviderWithApiKeyOverride)
import Haskode.Policy          (defaultPolicy)
import Haskode.Session         (flushLog, flushLogOnException, summarizeSession, formatSessionSummary, isMeaningfulSession, loadResumeEvents, ResumeResult (..), formatResumeSummary)
import Haskode.Tools           (ToolRegistry, defaultRegistry, disableTools)
import Haskode.Tui             (runTui)
import Haskode.Agent           (AgentState (..), initState, runAgent, terminalApproval,
                                recordSessionStart, recordSessionStartWithResume,
                                recordSessionEnd, recordConversationReset)

-- ---------------------------------------------------------------------------
-- CLI options
-- ---------------------------------------------------------------------------

-- | Command-line options.  Every field is optional; values not set on
--   the command line fall back to the config file, then to defaults.
data Options = Options
  { optPrompt  :: !(Maybe Text)      -- ^ Single-shot prompt (if any)
  , optConfig  :: !(Maybe FilePath)  -- ^ Explicit config path
  , optProvider :: !(Maybe String)   -- ^ Provider override (e.g. "openai", "stub")
  , optModel   :: !(Maybe String)    -- ^ Model override (e.g. "gpt-4o")
  , optApiKey  :: !(Maybe String)    -- ^ API key override (bypasses env var)
  , optBaseUrl :: !(Maybe String)    -- ^ Base URL override
  , optVerbose :: !Bool              -- ^ Verbose output
  , optShowSession :: !Bool          -- ^ Show session log summary and exit
  , optResume  :: !Bool              -- ^ Resume conversation from session.jsonl
  , optTui     :: !Bool              -- ^ Launch experimental Brick TUI mode
  } deriving stock (Show)

optionsParser :: Parser Options
optionsParser = Options
  <$> optional (strOption
        ( long "prompt"
       <> short 'p'
       <> metavar "TEXT"
       <> help "Send a single prompt and exit"
        ))
  <*> optional (strOption
        ( long "config"
       <> short 'c'
       <> metavar "PATH"
       <> help "Path to config file"
        ))
  <*> optional (strOption
        ( long "provider"
       <> short 'P'
       <> metavar "NAME"
       <> help "Provider: openai, anthropic, ollama, vllm, litellm, openrouter, stub"
        ))
  <*> optional (strOption
        ( long "model"
       <> short 'm'
       <> metavar "MODEL"
       <> help "Model identifier (e.g. gpt-4o, llama3.1)"
        ))
  <*> optional (strOption
        ( long "api-key"
       <> metavar "KEY"
       <> help "API key (overrides provider-specific env vars)"
        ))
  <*> optional (strOption
        ( long "base-url"
       <> metavar "URL"
       <> help "API base URL (e.g. https://api.openai.com)"
        ))
  <*> switch
        ( long "verbose"
       <> short 'v'
       <> help "Print debug info (provider, model, config source)"
        )
  <*> switch
        ( long "show-session"
       <> help "Print session log summary and exit (read-only, no replay)"
        )
  <*> switch
        ( long "resume"
       <> help "Resume conversation from session.jsonl in working directory"
        )
  <*> switch
        ( long "tui"
       <> help "Launch the experimental minimal Brick TUI"
        )

-- ---------------------------------------------------------------------------
-- CLI info
-- ---------------------------------------------------------------------------

opts :: ParserInfo Options
opts = info (optionsParser <**> helper)
  ( fullDesc
 <> progDesc "Haskode: a small, hackable LLM coding harness"
 <> header "haskode - Haskell-native coding agent"
  )

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  opts' <- execParser opts
  cfg   <- maybe loadConfig loadConfigFrom (optConfig opts')
  let cfg' = applyOverrides cfg opts'
  registry <- resolveToolRegistry cfg'

  handleShowSession cfg' opts'
  unless (optTui opts') printBanner

  prov <- resolveProvider cfg' opts' registry
  unless (optTui opts') (printVerboseInfo cfg' opts')

  state <- loadInitialState cfg' prov registry (optResume opts')
  runSelectedMode opts' state

-- ---------------------------------------------------------------------------
-- Startup-phase helpers
-- ---------------------------------------------------------------------------

-- | @\-\-show-session@: print session log summary and exit.
--   Read-only; no provider setup, no resume, no banner.
handleShowSession :: Config -> Options -> IO ()
handleShowSession cfg' opts' =
  when (optShowSession opts') $ do
    let dir = cfgWorkingDir cfg'
    summary <- summarizeSession dir
    TIO.putStrLn (formatSessionSummary summary)
    exitSuccess

-- | Print the startup banner for normal interactive\/single-shot runs.
--
-- Uses Unicode box-drawing characters (@┌@, @─@, @│@, @└@) for
-- cosmetic framing.  These are intentional; on Windows consoles
-- without UTF-8 support they may render as fallback glyphs but
-- the surrounding text remains readable.
printBanner :: IO ()
printBanner = do
  putStrLn "┌─────────────────────────────────────┐"
  putStrLn "│  Haskode -- Haskell-native harness  │"
  putStrLn "│    0BSD · hackable · educational    │"
  putStrLn "└─────────────────────────────────────┘"
  putStrLn ""

-- | Print provider\/model\/base-url diagnostics when @\-\-verbose@ is set.
printVerboseInfo :: Config -> Options -> IO ()
printVerboseInfo cfg' opts' =
  when (optVerbose opts') $ do
    let pc = cfgProvider cfg'
    putStrLn $ formatVerbose "provider" (pcProvider pc)
    putStrLn $ formatVerbose "model"    (pcModel pc)
    putStrLn $ formatVerbose "base url" (pcBaseUrl pc)
    putStrLn ""

-- | Build the runtime tool registry from config.
--
-- A misspelled disabled-tool name is a config error: failing clearly is
-- safer than silently leaving a tool enabled.
resolveToolRegistry :: Config -> IO ToolRegistry
resolveToolRegistry cfg' =
  case disableTools (cfgDisabledTools cfg') defaultRegistry of
    Right reg -> pure reg
    Left err -> do
      TIO.putStrLn ("Error: " <> err)
      exitFailure

-- | Build the initial 'AgentState' and, when @\-\-resume@ is set,
--   reconstruct safe text context from @session.jsonl@.
--   Resume does not replay provider calls or tools.
loadInitialState :: Config -> Provider -> ToolRegistry -> Bool -> IO AgentState
loadInitialState cfg' prov registry resumed = do
  let state0 = initState cfg' prov defaultPolicy registry terminalApproval resumed
  (state1, resumeInfo) <- if resumed
    then do
      let dir = cfgWorkingDir cfg'
          logPath = dir </> "session.jsonl"
      mResult <- loadResumeEvents dir
      case mResult of
        Nothing -> do
          putStrLn "No session.jsonl found; starting fresh."
          pure (state0, Nothing)
        Just rr -> do
          let n = rrMessageCount rr
          if n == 0
            then do
              putStrLn $ "session.jsonl exists but reconstructed 0 messages; starting fresh."
              TIO.putStr (formatResumeSummary logPath rr)
              pure (state0, Just rr)
            else do
              TIO.putStr (formatResumeSummary logPath rr)
              putStrLn ""
              pure (state0 { asConversation = rrResumeContext rr }, Just rr)
    else pure (state0, Nothing)
  case resumeInfo of
    Just rr -> recordSessionStartWithResume state1 (rrMessageCount rr)
    Nothing -> recordSessionStart state1

-- | Dispatch to single-shot or interactive mode.
--   Flushes the session log on normal exit.
runSelectedMode :: Options -> AgentState -> IO ()
runSelectedMode opts' state =
  if optTui opts'
    then do
      finalState <- runTui (optPrompt opts') state
      stateEnd <- recordSessionEnd finalState
      flushSession stateEnd
    else case optPrompt opts' of
      Just prompt -> do
        state' <- runAgentSafe state prompt
        stateEnd <- recordSessionEnd state'
        flushSession stateEnd
      Nothing -> do
        finalState <- interactiveLoop state
        stateEnd <- recordSessionEnd finalState
        flushSession stateEnd

-- ---------------------------------------------------------------------------
-- Provider resolution
-- ---------------------------------------------------------------------------

-- | The set of provider names that are treated as OpenAI-compatible
--   HTTP endpoints.  All of these speak the @\/v1\/chat\/completions@
--   wire format and are handled by 'openaiProvider'.
openaiCompatibleProviders :: [String]
openaiCompatibleProviders = ["openai", "ollama", "vllm", "litellm", "openrouter"]

-- | Resolve a 'Provider' from the config.
--
--   Supported provider names:
--
--     * @openai@, @ollama@, @vllm@, @litellm@, @openrouter@ —
--       any OpenAI-compatible endpoint, handled by 'openaiProvider'
--     * @anthropic@ — Anthropic Messages API
--     * @stub@ — local echo provider (for development)
--
--   Any other name prints a clear error and exits.
resolveProvider :: Config -> Options -> ToolRegistry -> IO Provider
resolveProvider cfg opts' registry =
  let name = pcProvider (cfgProvider cfg)
  in case () of
    _ | name `elem` openaiCompatibleProviders -> do
          eitherProv <- openaiProviderWithApiKeyOverride (optApiKey opts') cfg registry
          case eitherProv of
            Left err -> do
              putStrLn $ "Error: " ++ err
              putStrLn ""
              putStrLn "To set an API key, either:"
              putStrLn "  1. Pass --api-key on the command line, or"
              putStrLn "  2. Set the OPENAI_API_KEY environment variable, or"
              putStrLn "  3. Add \"pcApiKey\" to your haskode.json config file."
              putStrLn ""
              putStrLn "Example:"
              putStrLn "  export OPENAI_API_KEY=\"sk-...\""
              putStrLn "  cabal run haskode -- --provider openai --prompt \"Hello\""
              exitFailure
            Right prov -> pure prov
      | name == "stub" -> pure stubProvider
      | name == "anthropic" -> do
          eitherProv <- anthropicProvider cfg registry
          case eitherProv of
            Left err -> do
              putStrLn $ "Error: " ++ err
              putStrLn ""
              putStrLn "To set an Anthropic API key, either:"
              putStrLn "  1. Pass --api-key on the command line, or"
              putStrLn "  2. Add \"pcApiKey\" to your haskode.json config file, or"
              putStrLn "  3. Set the ANTHROPIC_API_KEY environment variable."
              putStrLn ""
              putStrLn "Example:"
              putStrLn "  export ANTHROPIC_API_KEY=\"sk-ant-...\""
              putStrLn "  cabal run haskode -- --provider anthropic --model claude-3-5-sonnet-latest --prompt \"Hello\""
              exitFailure
            Right prov -> pure prov
      | otherwise -> do
          putStrLn $ "Error: unknown provider \"" ++ name ++ "\"."
          putStrLn ""
          putStrLn "Supported providers:"
          putStrLn $ "  " ++ unwords openaiCompatibleProviders ++ "  (OpenAI-compatible HTTP)"
          putStrLn "  anthropic                                  (Anthropic Messages API)"
          putStrLn "  stub                                        (local echo, for development)"
          putStrLn ""
          putStrLn "Example:"
          putStrLn "  cabal run haskode -- --provider openai --prompt \"Hello\""
          exitFailure

-- ---------------------------------------------------------------------------
-- Config overrides
-- ---------------------------------------------------------------------------

-- | Apply command-line overrides on top of a loaded config.
--   CLI values take precedence; config file values are used when
--   the CLI flag is not set.
applyOverrides :: Config -> Options -> Config
applyOverrides cfg opts' = cfg
  { cfgProvider = (cfgProvider cfg)
      { pcProvider = maybe (pcProvider pc) id (optProvider opts')
      , pcModel    = maybe (pcModel    pc) id (optModel    opts')
      , pcBaseUrl  = maybe (pcBaseUrl  pc) id (optBaseUrl  opts')
      , pcApiKey   = maybe (pcApiKey   pc) id (optApiKey   opts')
      }
  , cfgVerbose  = cfgVerbose cfg || optVerbose opts'
  }
  where
    pc = cfgProvider cfg

-- ---------------------------------------------------------------------------
-- Interactive loop
-- ---------------------------------------------------------------------------

-- | Read user input, run the agent, repeat.
--   Returns the final 'AgentState' so the caller can flush the log.
--   The loop body is wrapped with 'flushLogOnException' so that an
--   unexpected EOF (or other stdin IO failure) flushes the accumulated
--   session events before the exception propagates.
interactiveLoop :: AgentState -> IO AgentState
interactiveLoop state = do
  hSetBuffering stdin LineBuffering
  putStr "You: "
  hFlush stdout
  let dir  = cfgWorkingDir (asConfig state)
      maxB = cfgMaxSessionLogBytes (asConfig state)
  flushLogOnException dir maxB (asSession state) $ do
    input <- TIO.getLine
    case parseSlashCommand input of
      Just cmd ->
        case lookupCommand cmd of
          Just spec
            | cmdAvailableInCli spec ->
                runCliCommand (cmdAction spec) state
          Nothing -> do
            TIO.putStrLn (formatUnknownCommand cmd)
            interactiveLoop state
          _ -> do
            TIO.putStrLn (formatUnknownCommand cmd)
            interactiveLoop state
      Nothing -> do
        state' <- runAgentSafe state input
        interactiveLoop state'

-- | Execute a parsed slash-command action in the CLI loop.
runCliCommand :: CommandAction -> AgentState -> IO AgentState
runCliCommand commandAction state =
  case commandAction of
    CmdExit -> do
      putStrLn "Goodbye!"
      pure state
    CmdHelp -> do
      TIO.putStr formatHelp
      interactiveLoop state
    CmdStatus -> do
      TIO.putStr (formatStatus state)
      interactiveLoop state
    CmdDoctor -> do
      output <- formatDoctor state
      TIO.putStr output
      interactiveLoop state
    CmdNew -> do
      TIO.putStrLn formatNewConfirmation
      let state1 = resetConversation state
      state2 <- recordConversationReset state1
      interactiveLoop state2

-- | Run the agent with exception-safe session flush.
--   On exception the current session log is flushed before re-throwing.
runAgentSafe :: AgentState -> Text -> IO AgentState
runAgentSafe st input =
  flushLogOnException (cfgWorkingDir (asConfig st))
                      (cfgMaxSessionLogBytes (asConfig st))
                      (asSession st)
    (runAgent st input)

-- | Flush the session log to @session.jsonl@ in the working directory.
--   Called on normal exit (single-shot completion or interactive /exit).
--   Skips the write when the session contains only lifecycle events
--   (e.g. immediate /exit, /help then /exit, /new then /exit).
flushSession :: AgentState -> IO ()
flushSession state = do
  let dir  = cfgWorkingDir (asConfig state)
      sess = asSession state
  when (isMeaningfulSession sess) $
    flushLog dir (cfgMaxSessionLogBytes (asConfig state)) sess
