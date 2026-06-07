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
--     * @stub@ — local echo provider (for development and testing)
--
-- All other values produce a clear error message.

module Main (main) where

import Control.Monad      (when)
import Data.Text          (Text)
import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import Options.Applicative
import System.Exit        (exitFailure, exitSuccess)
import System.IO          (hFlush, stdout, hSetBuffering, stdin, BufferMode (..))

import Haskode.Config         (Config (..), ProviderConfig (..), loadConfig)
import Haskode.Provider       (Provider, stubProvider)
import Haskode.Provider.OpenAI (openaiProvider)
import Haskode.Policy         (defaultPolicy)
import Haskode.Session        (flushLog, flushLogOnException, summarizeSession, formatSessionSummary)
import Haskode.Tools          (defaultRegistry)
import Haskode.Agent          (AgentState (..), initState, runAgent, terminalApproval)

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
       <> help "Provider: openai, ollama, vllm, litellm, openrouter, stub"
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
       <> help "API key (overrides OPENAI_API_KEY env var)"
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

-- ---------------------------------------------------------------------------
-- Main
-- ---------------------------------------------------------------------------

main :: IO ()
main = do
  opts' <- execParser opts
  cfg   <- loadConfig

  -- Apply CLI overrides to the loaded config
  let cfg' = applyOverrides cfg opts'

  -- --show-session: print summary and exit (read-only, no replay)
  when (optShowSession opts') $ do
    let dir = cfgWorkingDir cfg'
    summary <- summarizeSession dir
    TIO.putStrLn (formatSessionSummary summary)
    exitSuccess

  -- Print banner
  putStrLn "┌─────────────────────────────────────┐"
  putStrLn "│  Haskode -- Haskell-native harness  │"
  putStrLn "│    0BSD · hackable · educational    │"
  putStrLn "└─────────────────────────────────────┘"
  putStrLn ""

  -- Resolve the provider based on config/CLI setting
  prov <- resolveProvider cfg'

  -- Print diagnostics in verbose mode
  when (optVerbose opts') $ do
    let pc = cfgProvider cfg'
    putStrLn $ "[verbose] provider: " ++ pcProvider pc
    putStrLn $ "[verbose] model:    " ++ pcModel pc
    putStrLn $ "[verbose] base url: " ++ pcBaseUrl pc
    putStrLn ""

  -- Build initial state
  let state = initState cfg' prov defaultPolicy defaultRegistry terminalApproval

  -- Single-shot or interactive mode
  case optPrompt opts' of
    Just prompt -> do
      state' <- runAgentSafe state prompt
      flushSession state'
    Nothing -> do
      finalState <- interactiveLoop state
      flushSession finalState
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Haskode: a small, hackable LLM coding harness"
     <> header "haskode — Haskell-native coding agent"
      )

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
--     * @stub@ — local echo provider (for development)
--
--   Any other name prints a clear error and exits.
resolveProvider :: Config -> IO Provider
resolveProvider cfg =
  let name = pcProvider (cfgProvider cfg)
  in case () of
    _ | name `elem` openaiCompatibleProviders -> do
          eitherProv <- openaiProvider cfg defaultRegistry
          case eitherProv of
            Left err -> do
              putStrLn $ "Error: " ++ err
              putStrLn ""
              putStrLn "To set an API key, either:"
              putStrLn "  1. Set the OPENAI_API_KEY environment variable, or"
              putStrLn "  2. Add \"pcApiKey\" to your haskode.json config file, or"
              putStrLn "  3. Pass --api-key on the command line."
              putStrLn ""
              putStrLn "Example:"
              putStrLn "  export OPENAI_API_KEY=\"sk-...\""
              putStrLn "  cabal run haskode -- --provider openai --prompt \"Hello\""
              exitFailure
            Right prov -> pure prov
      | name == "stub" -> pure stubProvider
      | otherwise -> do
          putStrLn $ "Error: unknown provider \"" ++ name ++ "\"."
          putStrLn ""
          putStrLn "Supported providers:"
          putStrLn $ "  " ++ unwords openaiCompatibleProviders ++ "  (OpenAI-compatible HTTP)"
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
    if T.strip input `elem` ["/exit", "/quit", "exit", "quit"]
      then do
        putStrLn "Goodbye!"
        pure state
      else do
        state' <- runAgentSafe state input
        interactiveLoop state'

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
flushSession :: AgentState -> IO ()
flushSession state = do
  let dir = cfgWorkingDir (asConfig state)
  flushLog dir (cfgMaxSessionLogBytes (asConfig state)) (asSession state)
