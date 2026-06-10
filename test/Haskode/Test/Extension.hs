{-# LANGUAGE OverloadedStrings #-}

-- | Tests for the compiled extension seam.
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
import Haskode.Config (defaultConfig)
import Haskode.Core
  ( ToolCall (..)
  , ToolResult (..)
  , mkAssistantMessage
  )
import Haskode.Extension
  ( Extension (..)
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
  ]
