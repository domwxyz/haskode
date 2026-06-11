{-# LANGUAGE OverloadedStrings #-}

-- | Tests for compiled extension support.
module Haskode.Test.Extension (tests) where

import Data.Aeson (KeyValue ((.=)), Value (..), object)
import Data.Maybe (isJust, isNothing)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Text as T

import Haskode.Agent
  ( AgentState (asSession)
  , autoApprove
  , initState
  , runAgent
  )
import Haskode.Commands
  ( CommandAction (..)
  , CommandSpec (..)
  , ExtensionCommand (..)
  , commandRegistry
  , formatHelp
  , formatHelpFor
  , resolveCommandActionFor
  )
import Haskode.Config (defaultConfig)
import Haskode.Core
  ( ToolCall (..)
  , ToolResult (..)
  , mkAssistantMessage
  )
import Haskode.Extension
  ( Extension (..)
  , buildFinalCommandRegistry
  , buildFinalToolRegistry
  , buildFinalRuntime
  )
import Haskode.Extensions (compiledExtensions)
import Haskode.Policy
  ( Decision (..)
  , Rule
  , checkPolicy
  , defaultPolicy
  )
import Haskode.Provider
  ( CompletionResponse (..)
  , scriptedProvider
  )
import Haskode.Provider.OpenAI (toolsToJSON)
import Haskode.Session
  ( Event (evData, evType)
  , EventType (EToolCall, EToolResult)
  , events
  )
import Haskode.Test.Util (Test)
import Haskode.Tools
  ( Tool (..)
  , defaultRegistry
  , lookupTool
  , toolNames
  )

extensionTool :: T.Text -> Tool
extensionTool name = Tool
  { toolName = name
  , toolDescription = "test extension tool"
  , toolSchema = object ["type" .= ("object" :: T.Text)]
  , toolExecute = \_ -> pure (ToolResult "" "extension ok")
  }

extensionWith :: T.Text -> T.Text -> Extension
extensionWith extName contributedToolName =
  extensionWithRules extName contributedToolName []

extensionWithRules :: T.Text -> T.Text -> [Rule] -> Extension
extensionWithRules extName contributedToolName rules = Extension
  { extensionName = extName
  , extensionDescription = "test extension"
  , extensionTools = [extensionTool contributedToolName]
  , extensionPolicyRules = rules
  , extensionCommands = []
  }

extensionTextCommand :: T.Text -> [T.Text] -> T.Text -> T.Text -> ExtensionCommand
extensionTextCommand name aliases description output = ExtensionCommand
  { extensionCommandName = name
  , extensionCommandAliases = aliases
  , extensionCommandDescription = description
  , extensionCommandOutput = output
  }

extensionWithCommand :: T.Text -> ExtensionCommand -> Extension
extensionWithCommand extName command = Extension
  { extensionName = extName
  , extensionDescription = "test command extension"
  , extensionTools = []
  , extensionPolicyRules = []
  , extensionCommands = [command]
  }

testEmptyExtensionsPreserveBuiltins :: Test
testEmptyExtensionsPreserveBuiltins =
  case buildFinalToolRegistry [] [] of
    Left err -> pure $ Left $ "empty extension registry failed: " ++ T.unpack err
    Right reg
      | toolNames reg == toolNames defaultRegistry -> pure $ Right ()
      | otherwise -> pure $ Left $
          "empty extensions changed tool names: " ++ show (toolNames reg)

testDefaultCompiledExtensionsPreserveStartupRegistry :: Test
testDefaultCompiledExtensionsPreserveStartupRegistry =
  case buildFinalToolRegistry compiledExtensions [] of
    Left err -> pure $ Left $ "default compiled extensions failed: " ++ T.unpack err
    Right reg
      | null compiledExtensions && toolNames reg == toolNames defaultRegistry ->
          pure $ Right ()
      | otherwise -> pure $ Left $
          "default startup registry changed: extensions="
          ++ show (length compiledExtensions)
          ++ " tools="
          ++ show (toolNames reg)

testEmptyExtensionsPreserveCommandRegistry :: Test
testEmptyExtensionsPreserveCommandRegistry =
  case buildFinalCommandRegistry [] of
    Left err -> pure $ Left $ "empty command extension registry failed: " ++ T.unpack err
    Right reg
      | reg == commandRegistry
        && formatHelpFor reg cmdAvailableInCli == formatHelp ->
          pure $ Right ()
      | otherwise -> pure $ Left "empty extensions changed command registry/help behavior"

testDefaultCompiledExtensionsPreserveStartupCommands :: Test
testDefaultCompiledExtensionsPreserveStartupCommands =
  case buildFinalCommandRegistry compiledExtensions of
    Left err -> pure $ Left $ "default compiled command registry failed: " ++ T.unpack err
    Right reg
      | null compiledExtensions && reg == commandRegistry ->
          pure $ Right ()
      | otherwise -> pure $ Left $
          "default startup command registry changed: extensions="
          ++ show (length compiledExtensions)
          ++ " commands="
          ++ show (map cmdName reg)

testEmptyExtensionsPreservePolicyBehavior :: Test
testEmptyExtensionsPreservePolicyBehavior =
  case buildFinalRuntime defaultPolicy [] [] of
    Left err -> pure $ Left $ "empty extension runtime failed: " ++ T.unpack err
    Right (_reg, policy)
      | policyDecisions policy policySamples == policyDecisions defaultPolicy policySamples ->
          pure $ Right ()
      | otherwise -> pure $ Left $
          "empty extensions changed policy decisions: "
          ++ show (policyDecisions policy policySamples)

testExtensionWithNoPolicyRulesPreservesPolicyBehavior :: Test
testExtensionWithNoPolicyRulesPreservesPolicyBehavior =
  case buildFinalRuntime defaultPolicy [extensionWith "local" "local_echo"] [] of
    Left err -> pure $ Left $ "extension runtime failed: " ++ T.unpack err
    Right (_reg, policy) ->
      let samples = policySamples ++ [toolCall "local_echo"]
      in if policyDecisions policy samples == policyDecisions defaultPolicy samples
           then pure $ Right ()
           else pure $ Left $
             "extension without policy rules changed decisions: "
             ++ show (policyDecisions policy samples)

testExtensionPolicyRuleCanAllowTool :: Test
testExtensionPolicyRuleCanAllowTool =
  case buildFinalRuntime defaultPolicy
        [extensionWithRules "local" "local_echo" [decideTool "local_echo" Allow]]
        [] of
    Left err -> pure $ Left $ "extension allow policy failed: " ++ T.unpack err
    Right (_reg, policy)
      | checkPolicy policy (toolCall "local_echo") == Allow -> pure $ Right ()
      | otherwise -> pure $ Left "extension policy rule should Allow local_echo"

testExtensionPolicyRuleCanAskUserForTool :: Test
testExtensionPolicyRuleCanAskUserForTool =
  let decision = AskUser "extension confirmation required"
  in case buildFinalRuntime defaultPolicy
        [extensionWithRules "local" "local_echo" [decideTool "local_echo" decision]]
        [] of
    Left err -> pure $ Left $ "extension AskUser policy failed: " ++ T.unpack err
    Right (_reg, policy)
      | checkPolicy policy (toolCall "local_echo") == decision -> pure $ Right ()
      | otherwise -> pure $ Left "extension policy rule should AskUser for local_echo"

testExtensionPolicyRuleCanDenyTool :: Test
testExtensionPolicyRuleCanDenyTool =
  let decision = Deny "extension denied"
  in case buildFinalRuntime defaultPolicy
        [extensionWithRules "local" "local_echo" [decideTool "local_echo" decision]]
        [] of
    Left err -> pure $ Left $ "extension Deny policy failed: " ++ T.unpack err
    Right (_reg, policy)
      | checkPolicy policy (toolCall "local_echo") == decision -> pure $ Right ()
      | otherwise -> pure $ Left "extension policy rule should Deny local_echo"

testNoMatchExtensionToolFallsThroughToAskUser :: Test
testNoMatchExtensionToolFallsThroughToAskUser =
  case buildFinalRuntime defaultPolicy
        [extensionWithRules "local" "local_echo" [decideTool "other_tool" Allow]]
        [] of
    Left err -> pure $ Left $ "extension no-match policy failed: " ++ T.unpack err
    Right (_reg, policy) ->
      case checkPolicy policy (toolCall "local_echo") of
        AskUser reason
          | "no policy rule matched" `T.isInfixOf` reason -> pure $ Right ()
        decision -> pure $ Left $
          "no-match extension tool should AskUser, got: " ++ show decision

testExtensionPolicyCannotWeakenDangerousShellDeny :: Test
testExtensionPolicyCannotWeakenDangerousShellDeny =
  case buildFinalRuntime defaultPolicy
        [extensionWithRules "local" "local_echo" [\_ -> Just Allow]]
        [] of
    Left err -> pure $ Left $ "extension broad policy failed: " ++ T.unpack err
    Right (_reg, policy) ->
      case ( checkPolicy policy (shellCall "rm -rf /")
           , checkPolicy policy (shellCall "echo ok")
           ) of
        (Deny reason, AskUser _)
          | "dangerous shell command blocked" `T.isInfixOf` reason ->
              pure $ Right ()
        decisions -> pure $ Left $
          "extension policy weakened shell behavior: " ++ show decisions

testExtensionToolAppearsInFinalRegistry :: Test
testExtensionToolAppearsInFinalRegistry =
  case buildFinalToolRegistry [extensionWith "local" "local_echo"] [] of
    Left err -> pure $ Left $ "extension registry failed: " ++ T.unpack err
    Right reg ->
      case lookupTool "local_echo" reg of
        Just _  -> pure $ Right ()
        Nothing -> pure $ Left $
          "extension tool missing from final registry: " ++ show (toolNames reg)

testDuplicateExtensionToolNameFails :: Test
testDuplicateExtensionToolNameFails =
  case buildFinalToolRegistry
        [ extensionWith "one" "local_echo"
        , extensionWith "two" "local_echo"
        ]
        [] of
    Left err
      | "duplicate compiled tool name: local_echo" `T.isInfixOf` err ->
          pure $ Right ()
      | otherwise -> pure $ Left $ "duplicate tool error unclear: " ++ T.unpack err
    Right reg -> pure $ Left $
      "duplicate extension tool name should fail, got: " ++ show (toolNames reg)

testDuplicateBuiltInToolNameFails :: Test
testDuplicateBuiltInToolNameFails =
  case buildFinalToolRegistry [extensionWith "local" "read_file"] [] of
    Left err
      | "duplicate compiled tool name: read_file" `T.isInfixOf` err ->
          pure $ Right ()
      | otherwise -> pure $ Left $ "built-in collision error unclear: " ++ T.unpack err
    Right reg -> pure $ Left $
      "extension tool should not replace built-in, got: " ++ show (toolNames reg)

testDuplicateExtensionNameFails :: Test
testDuplicateExtensionNameFails =
  case buildFinalToolRegistry
        [ extensionWith "same" "local_one"
        , extensionWith "same" "local_two"
        ]
        [] of
    Left err
      | "duplicate extension name(s): same" `T.isInfixOf` err ->
          pure $ Right ()
      | otherwise -> pure $ Left $ "duplicate extension error unclear: " ++ T.unpack err
    Right reg -> pure $ Left $
      "duplicate extension names should fail, got: " ++ show (toolNames reg)

testDisabledToolsCanDisableExtensionTool :: Test
testDisabledToolsCanDisableExtensionTool =
  case buildFinalToolRegistry [extensionWith "local" "local_echo"] ["local_echo"] of
    Left err -> pure $ Left $ "disabled extension tool failed: " ++ T.unpack err
    Right reg ->
      if isNothing (lookupTool "local_echo" reg)
         && isJust (lookupTool "read_file" reg)
        then pure $ Right ()
        else pure $ Left $
          "disabled extension tool still present: " ++ show (toolNames reg)

testUnknownDisabledToolStillFails :: Test
testUnknownDisabledToolStillFails =
  case buildFinalToolRegistry [extensionWith "local" "local_echo"] ["missing_tool"] of
    Left err
      | "unknown disabled tool(s): missing_tool" `T.isInfixOf` err
        && "Known compiled tools:" `T.isInfixOf` err ->
          pure $ Right ()
      | otherwise -> pure $ Left $ "unknown disabled-tool error unclear: " ++ T.unpack err
    Right reg -> pure $ Left $
      "unknown disabled tool should fail, got: " ++ show (toolNames reg)

testDisabledExtensionToolNotAdvertised :: Test
testDisabledExtensionToolNotAdvertised =
  case buildFinalToolRegistry [extensionWith "local" "local_echo"] ["local_echo"] of
    Left err -> pure $ Left $ "disabled extension tool failed: " ++ T.unpack err
    Right reg ->
      let names = openAIToolNames (toolsToJSON reg)
      in if not ("local_echo" `elem` names)
            && length names == length (toolNames reg)
           then pure $ Right ()
           else pure $ Left $
             "disabled extension tool advertised: " ++ show names

testDisabledExtensionToolCannotExecute :: Test
testDisabledExtensionToolCannotExecute =
  case buildFinalRuntime defaultPolicy
        [extensionWithRules "local" "local_echo" [decideTool "local_echo" Allow]]
        ["local_echo"] of
    Left err -> pure $ Left $ "disabled extension runtime failed: " ++ T.unpack err
    Right (reg, policy) -> do
      prov <- scriptedProvider
        [ CompletionResponse
            { crReply = mkAssistantMessage "Calling extension tool."
            , crToolCalls = Just [ToolCall "tc-local" "local_echo" (object [])]
            }
        , CompletionResponse
            { crReply = mkAssistantMessage "Done."
            , crToolCalls = Nothing
            }
        ]
      let state = initState defaultConfig prov policy reg autoApprove False
      state' <- runAgent state "call the extension tool"
      let evts = events (asSession state')
          executed =
            [ e
            | e <- evts
            , evType e == EToolCall
            , "local_echo" `T.isInfixOf` evData e
            ]
          disabledResult =
            [ e
            | e <- evts
            , evType e == EToolResult
            , "unknown or disabled tool local_echo" `T.isInfixOf` evData e
            ]
      if null executed && not (null disabledResult)
        then pure $ Right ()
        else pure $ Left $
          "disabled extension tool should not execute; events: " ++ show evts

testBuiltInDisabledToolStillWorks :: Test
testBuiltInDisabledToolStillWorks =
  case buildFinalToolRegistry [] ["shell"] of
    Left err -> pure $ Left $ "built-in disabled tool failed: " ++ T.unpack err
    Right reg ->
      if isNothing (lookupTool "shell" reg)
         && isJust (lookupTool "read_file" reg)
        then pure $ Right ()
        else pure $ Left $
          "built-in disabled-tool behavior changed: " ++ show (toolNames reg)

testExtensionTextCommandResolves :: Test
testExtensionTextCommandResolves =
  let output = "hello from extension"
      ext = extensionWithCommand "local"
        (extensionTextCommand "hello_ext" [] "say hello" output)
  in case buildFinalCommandRegistry [ext] of
    Left err -> pure $ Left $ "extension command registry failed: " ++ T.unpack err
    Right reg ->
      case resolveCommandActionFor reg cmdAvailableInCli "hello_ext" of
        Right (CmdExtensionText text)
          | text == output -> pure $ Right ()
        other -> pure $ Left $
          "extension command should resolve to pure text, got: " ++ show other

testExtensionTextCommandAppearsInHelp :: Test
testExtensionTextCommandAppearsInHelp =
  let ext = extensionWithCommand "local"
        (extensionTextCommand "hello_ext" ["he"] "say hello" "hello")
  in case buildFinalCommandRegistry [ext] of
    Left err -> pure $ Left $ "extension command registry failed: " ++ T.unpack err
    Right reg ->
      let out = formatHelpFor reg cmdAvailableInCli
      in if "/hello_ext" `T.isInfixOf` out
            && "/he" `T.isInfixOf` out
            && "say hello" `T.isInfixOf` out
           then pure $ Right ()
           else pure $ Left $ "extension command missing from help: " ++ T.unpack out

testDuplicateCommandNameWithBuiltInFails :: Test
testDuplicateCommandNameWithBuiltInFails =
  let ext = extensionWithCommand "local"
        (extensionTextCommand "help" [] "bad duplicate" "no")
  in case buildFinalCommandRegistry [ext] of
    Left err
      | "duplicate compiled command name/alias: help" `T.isInfixOf` err ->
          pure $ Right ()
      | otherwise -> pure $ Left $ "duplicate command name error unclear: " ++ T.unpack err
    Right reg -> pure $ Left $
      "extension command should not replace built-in, got: " ++ show (map cmdName reg)

testDuplicateCommandAliasWithBuiltInFails :: Test
testDuplicateCommandAliasWithBuiltInFails =
  let ext = extensionWithCommand "local"
        (extensionTextCommand "hello_ext" ["status"] "bad duplicate" "no")
  in case buildFinalCommandRegistry [ext] of
    Left err
      | "duplicate compiled command name/alias: status" `T.isInfixOf` err ->
          pure $ Right ()
      | otherwise -> pure $ Left $ "duplicate command alias error unclear: " ++ T.unpack err
    Right reg -> pure $ Left $
      "extension alias should not replace built-in, got: " ++ show (map cmdName reg)

testDuplicateCommandNamesBetweenExtensionsFail :: Test
testDuplicateCommandNamesBetweenExtensionsFail =
  let ext1 = extensionWithCommand "one"
        (extensionTextCommand "same_ext" [] "one" "one")
      ext2 = extensionWithCommand "two"
        (extensionTextCommand "same_ext" [] "two" "two")
  in case buildFinalCommandRegistry [ext1, ext2] of
    Left err
      | "duplicate compiled command name/alias: same_ext" `T.isInfixOf` err ->
          pure $ Right ()
      | otherwise -> pure $ Left $ "duplicate extension command error unclear: " ++ T.unpack err
    Right reg -> pure $ Left $
      "duplicate extension command names should fail, got: " ++ show (map cmdName reg)

testDuplicateCommandAliasesBetweenExtensionsFail :: Test
testDuplicateCommandAliasesBetweenExtensionsFail =
  let ext1 = extensionWithCommand "one"
        (extensionTextCommand "one_ext" ["shared_ext"] "one" "one")
      ext2 = extensionWithCommand "two"
        (extensionTextCommand "two_ext" ["shared_ext"] "two" "two")
  in case buildFinalCommandRegistry [ext1, ext2] of
    Left err
      | "duplicate compiled command name/alias: shared_ext" `T.isInfixOf` err ->
          pure $ Right ()
      | otherwise -> pure $ Left $ "duplicate extension alias error unclear: " ++ T.unpack err
    Right reg -> pure $ Left $
      "duplicate extension command aliases should fail, got: " ++ show (map cmdName reg)

testCliAndTuiResolveExtensionCommandSameWay :: Test
testCliAndTuiResolveExtensionCommandSameWay =
  let ext = extensionWithCommand "local"
        (extensionTextCommand "hello_ext" [] "say hello" "same output")
  in case buildFinalCommandRegistry [ext] of
    Left err -> pure $ Left $ "extension command registry failed: " ++ T.unpack err
    Right reg ->
      case ( resolveCommandActionFor reg cmdAvailableInCli "hello_ext"
           , resolveCommandActionFor reg cmdAvailableInTui "hello_ext"
           ) of
        (Right (CmdExtensionText cliOut), Right (CmdExtensionText tuiOut))
          | cliOut == tuiOut -> pure $ Right ()
        resolved -> pure $ Left $
          "CLI/TUI should resolve extension command identically, got: " ++ show resolved

testBuiltInCommandActionsUnchangedInFinalRegistry :: Test
testBuiltInCommandActionsUnchangedInFinalRegistry =
  case buildFinalCommandRegistry [] of
    Left err -> pure $ Left $ "empty command registry failed: " ++ T.unpack err
    Right reg ->
      let expected =
            [ ("new", CmdNew)
            , ("compact", CmdCompact)
            , ("exit", CmdExit)
            , ("quit", CmdExit)
            , ("status", CmdStatus)
            , ("doctor", CmdDoctor)
            ]
          mismatches =
            [ (name, action, resolveCommandActionFor reg cmdAvailableInCli name)
            | (name, action) <- expected
            , resolveCommandActionFor reg cmdAvailableInCli name /= Right action
            ]
      in if null mismatches
           then pure $ Right ()
           else pure $ Left $ "built-in command actions changed: " ++ show mismatches

openAIToolNames :: [Value] -> [T.Text]
openAIToolNames toolValues =
  [ name
  | Object toolObj <- toolValues
  , Just (Object fn) <- [KM.lookup (Key.fromText "function") toolObj]
  , Just (String name) <- [KM.lookup (Key.fromText "name") fn]
  ]

policySamples :: [ToolCall]
policySamples =
  [ ToolCall "tc-read" "read_file"
      (object ["path" .= ("README.md" :: T.Text)])
  , shellCall "rm -rf /"
  , shellCall "echo ok"
  , ToolCall "tc-write" "write_file"
      (object ["path" .= ("x.txt" :: T.Text), "content" .= ("new" :: T.Text)])
  ]

policyDecisions :: [Rule] -> [ToolCall] -> [Decision]
policyDecisions policy = map (checkPolicy policy)

toolCall :: T.Text -> ToolCall
toolCall name = ToolCall "tc-extension" name (object [])

shellCall :: T.Text -> ToolCall
shellCall command = ToolCall "tc-shell" "shell" (object ["command" .= command])

decideTool :: T.Text -> Decision -> Rule
decideTool name decision tc
  | tcName tc == name = Just decision
  | otherwise = Nothing

tests :: [Test]
tests =
  [ testEmptyExtensionsPreserveBuiltins
  , testDefaultCompiledExtensionsPreserveStartupRegistry
  , testEmptyExtensionsPreserveCommandRegistry
  , testDefaultCompiledExtensionsPreserveStartupCommands
  , testEmptyExtensionsPreservePolicyBehavior
  , testExtensionWithNoPolicyRulesPreservesPolicyBehavior
  , testExtensionPolicyRuleCanAllowTool
  , testExtensionPolicyRuleCanAskUserForTool
  , testExtensionPolicyRuleCanDenyTool
  , testNoMatchExtensionToolFallsThroughToAskUser
  , testExtensionPolicyCannotWeakenDangerousShellDeny
  , testExtensionToolAppearsInFinalRegistry
  , testDuplicateExtensionToolNameFails
  , testDuplicateBuiltInToolNameFails
  , testDuplicateExtensionNameFails
  , testDisabledToolsCanDisableExtensionTool
  , testUnknownDisabledToolStillFails
  , testDisabledExtensionToolNotAdvertised
  , testDisabledExtensionToolCannotExecute
  , testBuiltInDisabledToolStillWorks
  , testExtensionTextCommandResolves
  , testExtensionTextCommandAppearsInHelp
  , testDuplicateCommandNameWithBuiltInFails
  , testDuplicateCommandAliasWithBuiltInFails
  , testDuplicateCommandNamesBetweenExtensionsFail
  , testDuplicateCommandAliasesBetweenExtensionsFail
  , testCliAndTuiResolveExtensionCommandSameWay
  , testBuiltInCommandActionsUnchangedInFinalRegistry
  ]
