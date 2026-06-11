{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

-- | Slash-command parser and formatter tests.
module Haskode.Test.Commands (tests) where

import Haskode.Agent
    ( AgentState(asConversation, asSession),
      autoApprove,
      estimateContextChars,
      initState )
import Haskode.Commands
    ( CommandAction(..),
      CommandResolution(..),
      CommandSpec(..),
      DoctorCheck(..),
      DoctorStatus(..),
      commandRegistry,
      parseSlashCommand,
      lookupCommand,
      resolveCommandFor,
      resolveCommandActionFor,
      formatHelp,
      formatHelpFor,
      formatStatus,
      formatUnknownCommand,
      formatNewConfirmation,
      resetConversation,
      formatContextUsage,
      formatDoctorChecks,
      doctorChecks )
import Haskode.Config
    ( defaultConfig,
      Config(cfgProvider, cfgVerbose, cfgDisabledTools),
      ProviderConfig(pcApiKey, pcProvider, pcModel, pcBaseUrl) )
import Haskode.Core ( mkUserMessage, Message(Message), Role(User) )
import Haskode.Policy ( defaultPolicy )
import Haskode.Provider ( stubProvider )
import Haskode.Session ( events )
import Haskode.Test.Util ( Test )
import Haskode.Tools ( defaultRegistry, disableTools, toolNames )
import qualified Data.Text as T ( isInfixOf, null, take, pack, unpack, lines, intercalate )
-- ---------------------------------------------------------------------------
-- Slash-command tests (pure)
-- ---------------------------------------------------------------------------

testParseSlashCommandHelp :: Test
testParseSlashCommandHelp =
  if parseSlashCommand "/help" == Just "help"
    then pure $ Right ()
    else pure $ Left "parseSlashCommand \"/help\" should be Just \"help\""

testParseSlashCommandStatus :: Test
testParseSlashCommandStatus =
  if parseSlashCommand "/status" == Just "status"
    then pure $ Right ()
    else pure $ Left "parseSlashCommand \"/status\" should be Just \"status\""

testParseSlashCommandExit :: Test
testParseSlashCommandExit =
  if parseSlashCommand "/exit" == Just "exit"
    then pure $ Right ()
    else pure $ Left "parseSlashCommand \"/exit\" should be Just \"exit\""

testParseSlashCommandQuit :: Test
testParseSlashCommandQuit =
  if parseSlashCommand "/quit" == Just "quit"
    then pure $ Right ()
    else pure $ Left "parseSlashCommand \"/quit\" should be Just \"quit\""

testParseSlashCommandUnknown :: Test
testParseSlashCommandUnknown =
  if parseSlashCommand "/foo" == Just "foo"
    then pure $ Right ()
    else pure $ Left "parseSlashCommand \"/foo\" should be Just \"foo\""

testParseSlashCommandNormalInput :: Test
testParseSlashCommandNormalInput =
  if parseSlashCommand "hello" == Nothing
    then pure $ Right ()
    else pure $ Left "parseSlashCommand \"hello\" should be Nothing"

testParseSlashCommandWhitespace :: Test
testParseSlashCommandWhitespace =
  if parseSlashCommand "  /help  " == Just "help"
    then pure $ Right ()
    else pure $ Left "parseSlashCommand \"  /help  \" should be Just \"help\""

testParseSlashCommandMixedWhitespace :: Test
testParseSlashCommandMixedWhitespace =
  if parseSlashCommand "\t/status \n" == Just "status"
    then pure $ Right ()
    else pure $ Left "parseSlashCommand should trim mixed leading/trailing whitespace"

testParseSlashCommandEmpty :: Test
testParseSlashCommandEmpty =
  if parseSlashCommand "" == Nothing
    then pure $ Right ()
    else pure $ Left "parseSlashCommand \"\" should be Nothing"

testParseSlashCommandSpacesOnly :: Test
testParseSlashCommandSpacesOnly =
  if parseSlashCommand "   " == Nothing
    then pure $ Right ()
    else pure $ Left "parseSlashCommand \"   \" should be Nothing"

testFormatHelpContent :: Test
testFormatHelpContent =
  if T.isInfixOf "/help" formatHelp
     && T.isInfixOf "/status" formatHelp
     && T.isInfixOf "/exit" formatHelp
     && T.isInfixOf "/quit" formatHelp
    then pure $ Right ()
    else pure $ Left $ "formatHelp missing expected commands: " ++ T.unpack (T.take 200 formatHelp)

testLookupCommandKnownActions :: Test
testLookupCommandKnownActions =
  let expected =
        [ ("help", CmdHelp)
        , ("status", CmdStatus)
        , ("new", CmdNew)
        , ("exit", CmdExit)
        ]
      mismatches =
        [ name
        | (name, action) <- expected
        , fmap cmdAction (lookupCommand name) /= Just action
        ]
  in if null mismatches
       then pure $ Right ()
       else pure $ Left $ "lookupCommand returned wrong actions for: " ++ show mismatches

testLookupCommandUnknown :: Test
testLookupCommandUnknown =
  if lookupCommand "missing" == Nothing
    then pure $ Right ()
    else pure $ Left "lookupCommand should return Nothing for unknown commands"

testResolveCommandForAvailable :: Test
testResolveCommandForAvailable =
  case resolveCommandFor commandRegistry cmdAvailableInCli "help" of
    CommandResolved spec | cmdAction spec == CmdHelp -> pure $ Right ()
    other -> pure $ Left $ "resolveCommandFor should resolve available /help, got: " ++ show other

testResolveCommandForUnknown :: Test
testResolveCommandForUnknown =
  if resolveCommandFor commandRegistry cmdAvailableInCli "missing" == CommandUnknown "missing"
    then pure $ Right ()
    else pure $ Left "resolveCommandFor should return CommandUnknown for missing commands"

testResolveCommandForUnavailable :: Test
testResolveCommandForUnavailable =
  if resolveCommandFor commandRegistry (const False) "help" == CommandUnknown "help"
    then pure $ Right ()
    else pure $ Left "resolveCommandFor should hide unavailable commands as unknown"

testResolveCommandActionForQuitAlias :: Test
testResolveCommandActionForQuitAlias =
  if resolveCommandActionFor commandRegistry cmdAvailableInCli "quit" == Right CmdExit
    then pure $ Right ()
    else pure $ Left "resolveCommandActionFor should map /quit to CmdExit"

testResolveCommandActionForUnknown :: Test
testResolveCommandActionForUnknown =
  if resolveCommandActionFor commandRegistry cmdAvailableInCli "missing" == Left "missing"
    then pure $ Right ()
    else pure $ Left "resolveCommandActionFor should preserve unknown command text"

testQuitAliasesExitAction :: Test
testQuitAliasesExitAction =
  case (lookupCommand "exit", lookupCommand "quit") of
    (Just exitSpec, Just quitSpec)
      | cmdAction exitSpec == CmdExit
        && cmdAction quitSpec == cmdAction exitSpec ->
          pure $ Right ()
    _ -> pure $ Left "/quit should resolve to the same action as /exit"

testFormatHelpIncludesEveryRegisteredCliCommand :: Test
testFormatHelpIncludesEveryRegisteredCliCommand =
  let cliCommands = filter cmdAvailableInCli commandRegistry
      missing =
        [ "/" <> cmdName spec
        | spec <- cliCommands
        , not (T.isInfixOf ("/" <> cmdName spec) formatHelp)
        ]
  in if null missing
       then pure $ Right ()
       else pure $ Left $
         "formatHelp missing registered commands: "
         ++ T.unpack (T.intercalate ", " missing)

testFormatHelpLineCountMatchesRegistry :: Test
testFormatHelpLineCountMatchesRegistry =
  let cliCommands = filter cmdAvailableInCli commandRegistry
      commandLines = filter (T.isInfixOf "  /") (T.lines formatHelp)
  in if length commandLines == length cliCommands
       then pure $ Right ()
       else pure $ Left $
         "formatHelp command count drifted from registry: "
         ++ show (length commandLines)
         ++ " help lines vs "
         ++ show (length cliCommands)
         ++ " registered CLI commands"

testFormatHelpIncludesRegisteredDescriptions :: Test
testFormatHelpIncludesRegisteredDescriptions =
  let cliCommands = filter cmdAvailableInCli commandRegistry
      missing =
        [ "/" <> cmdName spec <> " description: " <> cmdDescription spec
        | spec <- cliCommands
        , not (T.isInfixOf (cmdDescription spec) formatHelp)
        ]
  in if null missing
       then pure $ Right ()
       else pure $ Left $
         "formatHelp missing registered descriptions: "
         ++ T.unpack (T.intercalate ", " missing)

testFormatHelpForFiltersUnavailableCommands :: Test
testFormatHelpForFiltersUnavailableCommands =
  let out = formatHelpFor commandRegistry (const False)
      commandLines = filter (T.isInfixOf "  /") (T.lines out)
  in if null commandLines
       then pure $ Right ()
       else pure $ Left $
         "formatHelpFor should not include unavailable commands: "
         ++ T.unpack (T.intercalate "\n" commandLines)

testFormatStatusContent :: Test
testFormatStatusContent =
  let cfg = defaultConfig { cfgProvider = (cfgProvider defaultConfig)
                            { pcProvider = "openai"
                            , pcModel    = "gpt-4o"
                            , pcBaseUrl  = "https://api.openai.com"
                            , pcApiKey   = "sk-secret-key-12345"
                            }
                          }
      state = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove False
      out   = formatStatus state
  in if T.isInfixOf "openai" out
        && T.isInfixOf "gpt-4o" out
        && T.isInfixOf "https://api.openai.com" out
        && T.isInfixOf "Session events:" out
        && T.isInfixOf "Tools:" out
        && T.isInfixOf "Context est:" out
        && T.isInfixOf "Remaining:" out
        && T.isInfixOf "Verbose:" out
        && T.isInfixOf "Streaming:" out
        && T.isInfixOf "Resumed:" out
     then pure $ Right ()
     else pure $ Left $ "formatStatus missing expected fields: " ++ T.unpack (T.take 500 out)

testFormatStatusNoApiKey :: Test
testFormatStatusNoApiKey =
  let cfg = defaultConfig { cfgProvider = (cfgProvider defaultConfig)
                            { pcApiKey = "sk-super-secret-key-DO-NOT-PRINT" }
                          }
      state = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove False
      out   = formatStatus state
  in if T.isInfixOf "sk-super-secret-key-DO-NOT-PRINT" out
     then pure $ Left "formatStatus should not expose API key"
     else pure $ Right ()

testFormatStatusVerboseOff :: Test
testFormatStatusVerboseOff =
  let cfg = defaultConfig { cfgVerbose = False }
      state = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove False
      out   = formatStatus state
  in if T.isInfixOf "Verbose:         off" out
     then pure $ Right ()
     else pure $ Left $ "formatStatus should show verbose off: " ++ T.unpack (T.take 300 out)

testFormatStatusVerboseOn :: Test
testFormatStatusVerboseOn =
  let cfg = defaultConfig { cfgVerbose = True }
      state = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove False
      out   = formatStatus state
  in if T.isInfixOf "Verbose:         on" out
     then pure $ Right ()
     else pure $ Left $ "formatStatus should show verbose on: " ++ T.unpack (T.take 300 out)

testFormatStatusStreamingNo :: Test
testFormatStatusStreamingNo =
  let state = initState defaultConfig stubProvider defaultPolicy defaultRegistry autoApprove False
      out   = formatStatus state
  in if T.isInfixOf "Streaming:       no" out
     then pure $ Right ()
     else pure $ Left $ "formatStatus should show streaming no for stub: " ++ T.unpack (T.take 300 out)

testFormatStatusToolCount :: Test
testFormatStatusToolCount =
  let state = initState defaultConfig stubProvider defaultPolicy defaultRegistry autoApprove False
      out   = formatStatus state
  in if T.isInfixOf "Tools:           " out
     then pure $ Right ()
     else pure $ Left $ "formatStatus missing tool count: " ++ T.unpack (T.take 300 out)

testFormatStatusDisabledToolsDefaultNone :: Test
testFormatStatusDisabledToolsDefaultNone =
  let state = initState defaultConfig stubProvider defaultPolicy defaultRegistry autoApprove False
      out   = formatStatus state
  in if T.isInfixOf "Disabled tools: none" out
     then pure $ Right ()
     else pure $ Left $ "formatStatus should show no disabled tools: " ++ T.unpack (T.take 300 out)

testFormatStatusDisabledToolsConfigured :: Test
testFormatStatusDisabledToolsConfigured =
  case disableTools ["shell", "write_file"] defaultRegistry of
    Left err -> pure $ Left $ "disableTools failed: " ++ T.unpack err
    Right reg -> do
      let cfg = defaultConfig { cfgDisabledTools = ["shell", "write_file"] }
          state = initState cfg stubProvider defaultPolicy reg autoApprove False
          out = formatStatus state
          toolsLines = filter (T.isInfixOf "Tools:") (T.lines out)
          toolsLine = T.intercalate "\n" toolsLines
      if T.isInfixOf "Disabled tools: shell, write_file" out
         && not (T.isInfixOf "shell" toolsLine)
         && not (T.isInfixOf "write_file" toolsLine)
        then pure $ Right ()
        else pure $ Left $ "formatStatus disabled-tool info mismatch: " ++ T.unpack out

testFormatUnknownCommandContent :: Test
testFormatUnknownCommandContent =
  let out = formatUnknownCommand "foo"
  in if T.isInfixOf "/foo" out && T.isInfixOf "/help" out
     then pure $ Right ()
     else pure $ Left $ "formatUnknownCommand missing expected text: " ++ T.unpack out

testFormatContextUsageUnderLimit :: Test
testFormatContextUsageUnderLimit =
  let conv    = [mkUserMessage "hello"]   -- 5 chars + 20 overhead = 25
      maxC    = 120000
      out     = formatContextUsage conv maxC
      est     = estimateContextChars conv
      remain  = maxC - est
  in if T.isInfixOf (T.pack (show est)) out
        && T.isInfixOf "Context est:" out
        && T.isInfixOf (T.pack (show remain)) out
        && T.isInfixOf "Remaining:" out
        && T.isInfixOf "% used" out
     then pure $ Right ()
     else pure $ Left $ "formatContextUsage under-limit: " ++ T.unpack out

testFormatContextUsageExactLimit :: Test
testFormatContextUsageExactLimit =
  let conv    = [mkUserMessage "hello"]   -- 25 chars estimated
      maxC    = estimateContextChars conv  -- exactly at limit
      out     = formatContextUsage conv maxC
  in if T.isInfixOf "Remaining:" out
        && T.isInfixOf "0 chars" out
        && not (T.isInfixOf "Over limit:" out)
     then pure $ Right ()
     else pure $ Left $ "formatContextUsage exact-limit: " ++ T.unpack out

testFormatContextUsageOverLimit :: Test
testFormatContextUsageOverLimit =
  let conv    = [mkUserMessage "this is a longer message for testing"]
      maxC    = 10   -- deliberately small
      out     = formatContextUsage conv maxC
      est     = estimateContextChars conv
      overBy  = est - maxC
  in if T.isInfixOf "Over limit:" out
        && T.isInfixOf (T.pack (show overBy)) out
        && T.isInfixOf "Context est:" out
        && T.isInfixOf "% used" out
     then pure $ Right ()
     else pure $ Left $ "formatContextUsage over-limit: " ++ T.unpack out

testParseSlashCommandNew :: Test
testParseSlashCommandNew =
  if parseSlashCommand "/new" == Just "new"
    then pure $ Right ()
    else pure $ Left "parseSlashCommand \"/new\" should be Just \"new\""

testParseSlashCommandCompact :: Test
testParseSlashCommandCompact =
  if parseSlashCommand "/compact" == Just "compact"
    then pure $ Right ()
    else pure $ Left "parseSlashCommand \"/compact\" should be Just \"compact\""

testFormatHelpContentIncludesNew :: Test
testFormatHelpContentIncludesNew =
  if T.isInfixOf "/new" formatHelp
    then pure $ Right ()
    else pure $ Left $ "formatHelp missing /new: " ++ T.unpack (T.take 200 formatHelp)

testFormatHelpContentIncludesCompact :: Test
testFormatHelpContentIncludesCompact =
  if T.isInfixOf "/compact" formatHelp
    then pure $ Right ()
    else pure $ Left $ "formatHelp missing /compact: " ++ T.unpack (T.take 300 formatHelp)

testLookupCommandCompact :: Test
testLookupCommandCompact =
  case lookupCommand "compact" of
    Just spec | cmdAction spec == CmdCompact -> pure $ Right ()
    _ -> pure $ Left "lookupCommand \"compact\" should return CmdCompact"

testResolveCommandActionForCompactCliAndTui :: Test
testResolveCommandActionForCompactCliAndTui =
  case ( resolveCommandActionFor commandRegistry cmdAvailableInCli "compact"
       , resolveCommandActionFor commandRegistry cmdAvailableInTui "compact"
       ) of
    (Right CmdCompact, Right CmdCompact) -> pure $ Right ()
    resolved -> pure $ Left $ "CLI/TUI should both resolve /compact, got: " ++ show resolved

testFormatNewConfirmationText :: Test
testFormatNewConfirmationText =
  if "fresh conversation" `T.isInfixOf` formatNewConfirmation
    then pure $ Right ()
    else pure $ Left $ "formatNewConfirmation missing expected text: " ++ T.unpack formatNewConfirmation

testResetConversationClearsMessages :: Test
testResetConversationClearsMessages =
  let cfg = defaultConfig
      state0 = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove False
      msg = Message User "hello" Nothing Nothing
      state1 = state0 { asConversation = [msg] }
      state2 = resetConversation state1
  in if null (asConversation state2)
       then pure $ Right ()
       else pure $ Left "resetConversation should clear conversation"

testResetConversationPreservesSession :: Test
testResetConversationPreservesSession =
  let cfg = defaultConfig
      state0 = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove False
      msg = Message User "hello" Nothing Nothing
      state1 = state0 { asConversation = [msg] }
      state2 = resetConversation state1
  in if length (events (asSession state2)) == length (events (asSession state1))
       then pure $ Right ()
       else pure $ Left "resetConversation should preserve session events"

testFormatStatusResumedNo :: Test
testFormatStatusResumedNo =
  let state = initState defaultConfig stubProvider defaultPolicy defaultRegistry autoApprove False
      out   = formatStatus state
  in if T.isInfixOf "Resumed:         no" out
     then pure $ Right ()
     else pure $ Left $ "formatStatus should show Resumed: no: " ++ T.unpack (T.take 300 out)

testFormatStatusResumedYes :: Test
testFormatStatusResumedYes =
  let state = initState defaultConfig stubProvider defaultPolicy defaultRegistry autoApprove True
      out   = formatStatus state
  in if T.isInfixOf "Resumed:         yes" out
     then pure $ Right ()
     else pure $ Left $ "formatStatus should show Resumed: yes: " ++ T.unpack (T.take 300 out)


-- ---------------------------------------------------------------------------
-- Doctor tests (pure)
-- ---------------------------------------------------------------------------

testLookupCommandDoctor :: Test
testLookupCommandDoctor =
  case lookupCommand "doctor" of
    Just spec | cmdAction spec == CmdDoctor -> pure $ Right ()
    _ -> pure $ Left "lookupCommand \"doctor\" should return CmdDoctor"

testFormatHelpIncludesDoctor :: Test
testFormatHelpIncludesDoctor =
  if T.isInfixOf "/doctor" formatHelp
    then pure $ Right ()
    else pure $ Left $ "formatHelp missing /doctor: " ++ T.unpack (T.take 300 formatHelp)

testFormatDoctorChecksEmpty :: Test
testFormatDoctorChecksEmpty =
  let out = formatDoctorChecks []
  in if T.null out
       then pure $ Right ()
       else pure $ Left "formatDoctorChecks [] should be empty"

testFormatDoctorChecksTags :: Test
testFormatDoctorChecksTags =
  let checks = [ DoctorCheck "provider" Ok "openai (hosted)"
               , DoctorCheck "API key" Warn "missing"
               , DoctorCheck "model" Info "not set"
               ]
      out = formatDoctorChecks checks
  in if T.isInfixOf "[ok] provider: openai (hosted)" out
        && T.isInfixOf "[warn] API key: missing" out
        && T.isInfixOf "[info] model: not set" out
       then pure $ Right ()
       else pure $ Left $ "formatDoctorChecks tags mismatch: " ++ T.unpack out

testDoctorStubProviderChecks :: Test
testDoctorStubProviderChecks =
  let cfg = defaultConfig { cfgProvider = (cfgProvider defaultConfig)
                            { pcProvider = "stub"
                            , pcModel    = "stub"
                            , pcBaseUrl  = ""
                            , pcApiKey   = ""
                            }
                          }
      state = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove False
  in do
    checks <- doctorChecks state
    let statuses = [(dcLabel c, dcStatus c) | c <- checks]
        providerOk = any (\c -> dcLabel c == "provider" && dcStatus c == Ok) checks
        keyOk = any (\c -> dcLabel c == "API key" && dcStatus c == Ok
                           && "not required" `T.isInfixOf` dcDetail c) checks
        modelOk = any (\c -> dcLabel c == "model" && dcStatus c == Ok) checks
    if providerOk && keyOk && modelOk
      then pure $ Right ()
      else pure $ Left $ "stub doctor checks mismatch: " ++ show statuses

testDoctorHostedProviderMissingKey :: Test
testDoctorHostedProviderMissingKey =
  let cfg = defaultConfig { cfgProvider = (cfgProvider defaultConfig)
                            { pcProvider = "openai"
                            , pcModel    = "gpt-4o"
                            , pcBaseUrl  = ""
                            , pcApiKey   = ""
                            }
                          }
      state = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove False
  in do
    checks <- doctorChecks state
    let keyCheck = filter (\c -> dcLabel c == "API key") checks
    case keyCheck of
      [c] | dcStatus c == Warn && "missing" `T.isInfixOf` dcDetail c ->
        pure $ Right ()
      _ -> pure $ Left $ "openai missing key should produce warn: " ++ show keyCheck

testDoctorOpenAICompatibleEmptyBaseUrlWarns :: Test
testDoctorOpenAICompatibleEmptyBaseUrlWarns =
  let cfg = defaultConfig { cfgProvider = (cfgProvider defaultConfig)
                            { pcProvider = "openai"
                            , pcModel    = "gpt-4o"
                            , pcBaseUrl  = ""
                            , pcApiKey   = "sk-fake-key"
                            }
                          }
      state = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove False
  in do
    checks <- doctorChecks state
    let baseUrlCheck = filter (\c -> dcLabel c == "base URL") checks
    case baseUrlCheck of
      [c] | dcStatus c == Warn
            && "--base-url https://api.openai.com" `T.isInfixOf` dcDetail c ->
        pure $ Right ()
      _ -> pure $ Left $ "openai empty base URL should warn: " ++ show baseUrlCheck

testDoctorAnthropicEmptyBaseUrlUsesDefault :: Test
testDoctorAnthropicEmptyBaseUrlUsesDefault =
  let cfg = defaultConfig { cfgProvider = (cfgProvider defaultConfig)
                            { pcProvider = "anthropic"
                            , pcModel    = "claude-3-5-sonnet-latest"
                            , pcBaseUrl  = ""
                            , pcApiKey   = "sk-ant-fake"
                            }
                          }
      state = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove False
  in do
    checks <- doctorChecks state
    let baseUrlCheck = filter (\c -> dcLabel c == "base URL") checks
    case baseUrlCheck of
      [c] | dcStatus c == Info
            && "provider default (https://api.anthropic.com)" `T.isInfixOf` dcDetail c ->
        pure $ Right ()
      _ -> pure $ Left $ "anthropic empty base URL should report default: " ++ show baseUrlCheck

testDoctorHostedProviderHasKey :: Test
testDoctorHostedProviderHasKey =
  let cfg = defaultConfig { cfgProvider = (cfgProvider defaultConfig)
                            { pcProvider = "openai"
                            , pcModel    = "gpt-4o"
                            , pcBaseUrl  = ""
                            , pcApiKey   = "sk-fake-key"
                            }
                          }
      state = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove False
  in do
    checks <- doctorChecks state
    let keyCheck = filter (\c -> dcLabel c == "API key") checks
        out = formatDoctorChecks checks
    case keyCheck of
      [c] | dcStatus c == Ok && "present" `T.isInfixOf` dcDetail c ->
        if T.isInfixOf "sk-fake-key" out
          then pure $ Left "doctor output should not expose API key value"
          else pure $ Right ()
      _ -> pure $ Left $ "openai with key should produce ok: " ++ show keyCheck

testDoctorLocalProviderNoKey :: Test
testDoctorLocalProviderNoKey =
  let cfg = defaultConfig { cfgProvider = (cfgProvider defaultConfig)
                            { pcProvider = "ollama"
                            , pcModel    = "llama3.1"
                            , pcBaseUrl  = ""
                            , pcApiKey   = ""
                            }
                          }
      state = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove False
  in do
    checks <- doctorChecks state
    let keyCheck = filter (\c -> dcLabel c == "API key") checks
    case keyCheck of
      [c] | dcStatus c == Ok && "not required" `T.isInfixOf` dcDetail c ->
        pure $ Right ()
      _ -> pure $ Left $ "ollama no key should be ok: " ++ show keyCheck

testDoctorDisabledToolsInOutput :: Test
testDoctorDisabledToolsInOutput =
  case disableTools ["shell"] defaultRegistry of
    Left err -> pure $ Left $ "disableTools failed: " ++ T.unpack err
    Right reg -> do
      let cfg = defaultConfig { cfgDisabledTools = ["shell"] }
          state = initState cfg stubProvider defaultPolicy reg autoApprove False
      checks <- doctorChecks state
      let disabledCheck = filter (\c -> dcLabel c == "disabled tools") checks
          out = formatDoctorChecks checks
      case disabledCheck of
        [c] | "shell" `T.isInfixOf` dcDetail c ->
          if T.isInfixOf "shell" out
            then pure $ Right ()
            else pure $ Left "disabled tools should appear in formatted output"
        _ -> pure $ Left $ "disabled tools check mismatch: " ++ show disabledCheck

testDoctorAvailableToolsCount :: Test
testDoctorAvailableToolsCount =
  let state = initState defaultConfig stubProvider defaultPolicy defaultRegistry autoApprove False
  in do
    checks <- doctorChecks state
    let toolsCheck = filter (\c -> dcLabel c == "available tools") checks
    case toolsCheck of
      [c] | dcStatus c == Ok ->
        if T.isInfixOf (T.pack (show (length (toolNames defaultRegistry)))) (dcDetail c)
          then pure $ Right ()
          else pure $ Left $ "tools count mismatch: " ++ T.unpack (dcDetail c)
      _ -> pure $ Left $ "available tools check missing: " ++ show toolsCheck

testDoctorNoApiKeyValueLeak :: Test
testDoctorNoApiKeyValueLeak =
  let cfg = defaultConfig { cfgProvider = (cfgProvider defaultConfig)
                            { pcProvider = "openai"
                            , pcModel    = "gpt-4o"
                            , pcApiKey   = "sk-super-secret-DO-NOT-PRINT"
                            }
                          }
      state = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove False
  in do
    checks <- doctorChecks state
    let out = formatDoctorChecks checks
    if T.isInfixOf "sk-super-secret-DO-NOT-PRINT" out
      then pure $ Left "doctor output must not expose API key"
      else pure $ Right ()

tests :: [Test]
tests =
  [ testParseSlashCommandHelp
  , testParseSlashCommandStatus
  , testParseSlashCommandExit
  , testParseSlashCommandQuit
  , testParseSlashCommandUnknown
  , testParseSlashCommandNormalInput
  , testParseSlashCommandWhitespace
  , testParseSlashCommandMixedWhitespace
  , testParseSlashCommandEmpty
  , testParseSlashCommandSpacesOnly
  , testParseSlashCommandNew
  , testParseSlashCommandCompact
  , testLookupCommandKnownActions
  , testLookupCommandUnknown
  , testResolveCommandForAvailable
  , testResolveCommandForUnknown
  , testResolveCommandForUnavailable
  , testResolveCommandActionForQuitAlias
  , testResolveCommandActionForUnknown
  , testResolveCommandActionForCompactCliAndTui
  , testLookupCommandDoctor
  , testLookupCommandCompact
  , testQuitAliasesExitAction
  , testFormatHelpContent
  , testFormatHelpContentIncludesNew
  , testFormatHelpContentIncludesCompact
  , testFormatHelpIncludesDoctor
  , testFormatHelpIncludesEveryRegisteredCliCommand
  , testFormatHelpLineCountMatchesRegistry
  , testFormatHelpIncludesRegisteredDescriptions
  , testFormatHelpForFiltersUnavailableCommands
  , testFormatStatusContent
  , testFormatStatusNoApiKey
  , testFormatStatusVerboseOff
  , testFormatStatusVerboseOn
  , testFormatStatusStreamingNo
  , testFormatStatusToolCount
  , testFormatStatusDisabledToolsDefaultNone
  , testFormatStatusDisabledToolsConfigured
  , testFormatContextUsageUnderLimit
  , testFormatContextUsageExactLimit
  , testFormatContextUsageOverLimit
  , testFormatUnknownCommandContent
  , testFormatNewConfirmationText
  , testResetConversationClearsMessages
  , testResetConversationPreservesSession
  , testFormatStatusResumedNo
  , testFormatStatusResumedYes
  , testFormatDoctorChecksEmpty
  , testFormatDoctorChecksTags
  , testDoctorStubProviderChecks
  , testDoctorHostedProviderMissingKey
  , testDoctorOpenAICompatibleEmptyBaseUrlWarns
  , testDoctorAnthropicEmptyBaseUrlUsesDefault
  , testDoctorHostedProviderHasKey
  , testDoctorLocalProviderNoKey
  , testDoctorDisabledToolsInOutput
  , testDoctorAvailableToolsCount
  , testDoctorNoApiKeyValueLeak
  ]
