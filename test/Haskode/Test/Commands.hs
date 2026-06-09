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
    ( parseSlashCommand,
      formatHelp,
      formatStatus,
      formatUnknownCommand,
      formatNewConfirmation,
      resetConversation,
      formatContextUsage )
import Haskode.Config
    ( defaultConfig,
      Config(cfgProvider),
      ProviderConfig(pcApiKey, pcProvider, pcModel, pcBaseUrl) )
import Haskode.Core ( mkUserMessage, Message(Message), Role(User) )
import Haskode.Policy ( defaultPolicy )
import Haskode.Provider ( stubProvider )
import Haskode.Session ( events )
import Haskode.Test.Util ( Test )
import Haskode.Tools ( defaultRegistry )
import qualified Data.Text as T ( isInfixOf, take, pack, unpack )
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

testFormatStatusContent :: Test
testFormatStatusContent =
  let cfg = defaultConfig { cfgProvider = (cfgProvider defaultConfig)
                            { pcProvider = "openai"
                            , pcModel    = "gpt-4o"
                            , pcBaseUrl  = "https://api.openai.com"
                            , pcApiKey   = "sk-secret-key-12345"
                            }
                          }
      state = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove
      out   = formatStatus state
  in if T.isInfixOf "openai" out
        && T.isInfixOf "gpt-4o" out
        && T.isInfixOf "https://api.openai.com" out
        && T.isInfixOf "Session events:" out
        && T.isInfixOf "Tools:" out
        && T.isInfixOf "Context est:" out
        && T.isInfixOf "Remaining:" out
     then pure $ Right ()
     else pure $ Left $ "formatStatus missing expected fields: " ++ T.unpack (T.take 400 out)

testFormatStatusNoApiKey :: Test
testFormatStatusNoApiKey =
  let cfg = defaultConfig { cfgProvider = (cfgProvider defaultConfig)
                            { pcApiKey = "sk-super-secret-key-DO-NOT-PRINT" }
                          }
      state = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove
      out   = formatStatus state
  in if T.isInfixOf "sk-super-secret-key-DO-NOT-PRINT" out
     then pure $ Left "formatStatus should not expose API key"
     else pure $ Right ()

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

testFormatHelpContentIncludesNew :: Test
testFormatHelpContentIncludesNew =
  if T.isInfixOf "/new" formatHelp
    then pure $ Right ()
    else pure $ Left $ "formatHelp missing /new: " ++ T.unpack (T.take 200 formatHelp)

testFormatNewConfirmationText :: Test
testFormatNewConfirmationText =
  if "fresh conversation" `T.isInfixOf` formatNewConfirmation
    then pure $ Right ()
    else pure $ Left $ "formatNewConfirmation missing expected text: " ++ T.unpack formatNewConfirmation

testResetConversationClearsMessages :: Test
testResetConversationClearsMessages =
  let cfg = defaultConfig
      state0 = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove
      msg = Message User "hello" Nothing Nothing
      state1 = state0 { asConversation = [msg] }
      state2 = resetConversation state1
  in if null (asConversation state2)
       then pure $ Right ()
       else pure $ Left "resetConversation should clear conversation"

testResetConversationPreservesSession :: Test
testResetConversationPreservesSession =
  let cfg = defaultConfig
      state0 = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove
      msg = Message User "hello" Nothing Nothing
      state1 = state0 { asConversation = [msg] }
      state2 = resetConversation state1
  in if length (events (asSession state2)) == length (events (asSession state1))
       then pure $ Right ()
       else pure $ Left "resetConversation should preserve session events"


tests :: [Test]
tests =
  [ testParseSlashCommandHelp
  , testParseSlashCommandStatus
  , testParseSlashCommandExit
  , testParseSlashCommandQuit
  , testParseSlashCommandUnknown
  , testParseSlashCommandNormalInput
  , testParseSlashCommandWhitespace
  , testParseSlashCommandEmpty
  , testParseSlashCommandSpacesOnly
  , testParseSlashCommandNew
  , testFormatHelpContent
  , testFormatHelpContentIncludesNew
  , testFormatStatusContent
  , testFormatStatusNoApiKey
  , testFormatContextUsageUnderLimit
  , testFormatContextUsageExactLimit
  , testFormatContextUsageOverLimit
  , testFormatUnknownCommandContent
  , testFormatNewConfirmationText
  , testResetConversationClearsMessages
  , testResetConversationPreservesSession
  ]
