{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

-- | OpenAI provider JSON and streaming parser tests.
module Haskode.Test.OpenAI (tests) where

import Data.Aeson
    ( Value(..), encode, decode, eitherDecode, object, (.=) )
import Haskode.Agent ( buildSystemPrompt )
import Haskode.Config
    ( tokenLimitFieldName, ProviderConfig(ProviderConfig) )
import Haskode.Core
    ( mkUserMessage,
      Message(Message, msgContent),
      Role(Assistant, User),
      ToolCall(tcName, ToolCall, tcArgs, tcId) )
import Haskode.Provider
    ( CompletionResponse(crToolCalls, CompletionResponse, crReply),
      StreamHandler(onToken, StreamHandler) )
import Haskode.Provider.OpenAI
    ( buildRequestBody,
      buildStreamingRequestBody,
      messagesToJSON,
      messageToJSON,
      toolsToJSON,
      parseResponseBody,
      parseToolCall,
      parseSSELine,
      parseSSEEvent,
      parseDeltaContent,
      parseDeltaToolCalls,
      StreamingToolCall(..),
      assembleStreamToolCalls,
      OpenAIError(..) )
import Haskode.Test.Util ( toolDescriptionFromRegistry, Test )
import Haskode.Tools ( defaultRegistry, toolNames )
import qualified Data.IORef ( modifyIORef, newIORef, readIORef )
import qualified Data.ByteString as BS ( empty )
import qualified Data.ByteString.Char8 as BS8 ( pack )
import qualified Data.Aeson.KeyMap as KM ( lookup )
import qualified Data.Aeson.Key as Key ( fromText )
import qualified Data.ByteString.Lazy as LBS ( ByteString )
import qualified Data.Map.Strict as Map
    ( insert, empty, lookup, fromList )
import qualified Data.Text as T
    ( Text, intercalate, isInfixOf, pack, unpack )
import qualified Data.Vector as V ( (!) )
-- ---------------------------------------------------------------------------

-- | Helper: wrap a message JSON into a full OpenAI API response.
mkApiResponse :: Value -> LBS.ByteString
mkApiResponse msgJson = encode $ object
  [ "id"      .= ("chatcmpl-test" :: T.Text)
  , "object"  .= ("chat.completion" :: T.Text)
  , "choices" .= [object
      [ "index"         .= (0 :: Int)
      , "message"       .= msgJson
      , "finish_reason" .= ("stop" :: T.Text)
      ]]
  ]

-- | Sample: plain text reply.
sampleTextResponse :: LBS.ByteString
sampleTextResponse = mkApiResponse $ object
  [ "role"    .= ("assistant" :: T.Text)
  , "content" .= ("Hello! How can I help?" :: T.Text)
  ]

-- | Sample: single tool call.
sampleToolCallResponse :: LBS.ByteString
sampleToolCallResponse = encode $ object
  [ "id"      .= ("chatcmpl-def456" :: T.Text)
  , "object"  .= ("chat.completion" :: T.Text)
  , "choices" .= [object
      [ "index" .= (0 :: Int)
      , "message" .= object
          [ "role"    .= ("assistant" :: T.Text)
          , "content" .= Null
          , "tool_calls" .= [object
              [ "id"   .= ("call_001" :: T.Text)
              , "type" .= ("function" :: T.Text)
              , "function" .= object
                  [ "name"      .= ("read_file" :: T.Text)
                  , "arguments" .= ("{\"path\":\"src/Main.hs\"}" :: T.Text)
                  ]
              ]]
          ]
      , "finish_reason" .= ("tool_calls" :: T.Text)
      ]]
  ]

-- | Sample: multiple tool calls.
sampleMultiToolCallResponse :: LBS.ByteString
sampleMultiToolCallResponse = encode $ object
  [ "id"      .= ("chatcmpl-ghi789" :: T.Text)
  , "object"  .= ("chat.completion" :: T.Text)
  , "choices" .= [object
      [ "index" .= (0 :: Int)
      , "message" .= object
          [ "role"    .= ("assistant" :: T.Text)
          , "content" .= ("Let me check." :: T.Text)
          , "tool_calls" .=
              [ object
                  [ "id"   .= ("call_002" :: T.Text)
                  , "type" .= ("function" :: T.Text)
                  , "function" .= object
                      [ "name"      .= ("read_file" :: T.Text)
                      , "arguments" .= ("{\"path\":\"foo.hs\"}" :: T.Text)
                      ]
                  ]
              , object
                  [ "id"   .= ("call_003" :: T.Text)
                  , "type" .= ("function" :: T.Text)
                  , "function" .= object
                      [ "name"      .= ("list_files" :: T.Text)
                      , "arguments" .= ("{\"dir\":\".\"}" :: T.Text)
                      ]
                  ]
              ]
          ]
      , "finish_reason" .= ("tool_calls" :: T.Text)
      ]]
  ]

-- | buildRequestBody produces valid JSON with model, messages, max_tokens.
testOpenAIRequestShape :: Test
testOpenAIRequestShape = do
  let msgs = [mkUserMessage "hello"]
      body = buildRequestBody "max_tokens" msgs "gpt-4o" 1024 mempty
  case decode body of
    Nothing -> pure $ Left "buildRequestBody produced invalid JSON"
    Just (Object obj) -> do
      let hasModel    = KM.lookup (Key.fromText "model") obj == Just (String "gpt-4o")
          hasMaxTok   = KM.lookup (Key.fromText "max_tokens") obj == Just (Number 1024)
          hasMessages = case KM.lookup (Key.fromText "messages") obj of
                          Just (Array _) -> True
                          _              -> False
      if hasModel && hasMaxTok && hasMessages
        then pure $ Right ()
        else pure $ Left $ "Request JSON missing expected fields: " ++ show obj
    Just other -> pure $ Left $ "Request JSON is not an object: " ++ show other

-- | buildRequestBody includes tools and tool_choice when registry is non-empty.
testOpenAIRequestTools :: Test
testOpenAIRequestTools = do
  let msgs = [mkUserMessage "hello"]
      body = buildRequestBody "max_tokens" msgs "gpt-4o" 1024 defaultRegistry
  case decode body of
    Nothing -> pure $ Left "buildRequestBody with tools produced invalid JSON"
    Just (Object obj) -> do
      let hasTools = case KM.lookup (Key.fromText "tools") obj of
                       Just (Array arr) -> length arr == length (toolNames defaultRegistry)
                       _                -> False
          hasToolChoice = KM.lookup (Key.fromText "tool_choice") obj == Just (String "auto")
      if hasTools && hasToolChoice
        then pure $ Right ()
        else pure $ Left $ "tools=" ++ show hasTools ++ " tool_choice=" ++ show hasToolChoice
    Just other -> pure $ Left $ "Request JSON is not an object: " ++ show other

-- | parseResponseBody handles a plain text reply correctly.
testOpenAIResponseText :: Test
testOpenAIResponseText =
  case parseResponseBody sampleTextResponse of
    Left err -> pure $ Left $ "Failed to parse text response: " ++ show err
    Right resp
      | msgContent (crReply resp) == "Hello! How can I help?"
        && crToolCalls resp == Nothing -> pure $ Right ()
      | otherwise ->
          pure $ Left $ "Unexpected content: " ++ T.unpack (msgContent (crReply resp))

-- | parseResponseBody handles a tool-call response correctly.
testOpenAIResponseToolCall :: Test
testOpenAIResponseToolCall =
  case parseResponseBody sampleToolCallResponse of
    Left err -> pure $ Left $ "Failed to parse tool-call response: " ++ show err
    Right resp -> case crToolCalls resp of
      Nothing -> pure $ Left "Expected tool calls, got Nothing"
      Just [tc]
        | tcId tc == "call_001" && tcName tc == "read_file" -> pure $ Right ()
        | otherwise -> pure $ Left $ "Tool call mismatch: " ++ show tc
      Just tcs -> pure $ Left $ "Expected 1 tool call, got " ++ show (length tcs)

-- | parseResponseBody handles multiple tool calls.
testOpenAIResponseMultiToolCall :: Test
testOpenAIResponseMultiToolCall =
  case parseResponseBody sampleMultiToolCallResponse of
    Left err -> pure $ Left $ "Failed to parse multi-tool response: " ++ show err
    Right resp -> case crToolCalls resp of
      Just [tc1, tc2]
        | tcName tc1 == "read_file" && tcName tc2 == "list_files" -> pure $ Right ()
        | otherwise -> pure $ Left $ "Tool call names mismatch: " ++ show (tcName tc1, tcName tc2)
      Just tcs -> pure $ Left $ "Expected 2 tool calls, got " ++ show (length tcs)
      Nothing -> pure $ Left "Expected tool calls, got Nothing"

-- | parseResponseBody fails cleanly on malformed JSON.
testOpenAIMalformedResponse :: Test
testOpenAIMalformedResponse =
  case parseResponseBody "{bad json" of
    Left (ResponseParseError _) -> pure $ Right ()
    Left other -> pure $ Left $ "Expected ResponseParseError, got: " ++ show other
    Right _ -> pure $ Left "Expected error for malformed JSON, got success"

-- | parseResponseBody fails cleanly when choices array is missing.
testOpenAIMissingChoices :: Test
testOpenAIMissingChoices =
  case parseResponseBody "{\"id\":\"test\",\"object\":\"chat.completion\"}" of
    Left (ResponseParseError _) -> pure $ Right ()
    Left other -> pure $ Left $ "Expected ResponseParseError, got: " ++ show other
    Right _ -> pure $ Left "Expected error for missing choices, got success"

-- | parseToolCall handles a well-formed tool call JSON value.
testOpenAIParseToolCall :: Test
testOpenAIParseToolCall = do
  let tcJson = object
        [ "id"   .= ("call_test" :: T.Text)
        , "type" .= ("function" :: T.Text)
        , "function" .= object
            [ "name"      .= ("read_file" :: T.Text)
            , "arguments" .= ("{\"path\":\"foo.hs\"}" :: T.Text)
            ]
        ]
  case parseToolCall tcJson of
    Left err -> pure $ Left $ "parseToolCall failed: " ++ show err
    Right tc
      | tcId tc == "call_test" && tcName tc == "read_file" -> pure $ Right ()
      | otherwise -> pure $ Left $ "parseToolCall mismatch: " ++ show tc

-- | parseToolCall fails cleanly on missing function name.
testOpenAIParseToolCallMissing :: Test
testOpenAIParseToolCallMissing = do
  let tcJson = object
        [ "id"   .= ("call_bad" :: T.Text)
        , "type" .= ("function" :: T.Text)
        , "function" .= object
            [ "arguments" .= ("{}" :: T.Text)
            ]
        ]
  case parseToolCall tcJson of
    Left (ResponseParseError _) -> pure $ Right ()
    Left other -> pure $ Left $ "Expected ResponseParseError, got: " ++ show other
    Right _ -> pure $ Left "Expected error for missing function name, got success"

-- | messageToJSON produces correct wire format for user messages.
testOpenAIMessageToJSON :: Test
testOpenAIMessageToJSON = do
  let msg = mkUserMessage "hello"
      val = messageToJSON msg
  case val of
    Object obj
      | KM.lookup (Key.fromText "role") obj == Just (String "user")
        && KM.lookup (Key.fromText "content") obj == Just (String "hello") ->
          pure $ Right ()
      | otherwise -> pure $ Left $ "messageToJSON mismatch: " ++ show obj
    _ -> pure $ Left "messageToJSON did not produce an object"

-- | messageToJSON produces "tool" role for tool-result messages.
testOpenAIToolResultToJSON :: Test
testOpenAIToolResultToJSON = do
  let msg = Message User "file contents" (Just "call_123") Nothing
      val = messageToJSON msg
  case val of
    Object obj
      | KM.lookup (Key.fromText "role") obj == Just (String "tool")
        && KM.lookup (Key.fromText "tool_call_id") obj == Just (String "call_123") ->
          pure $ Right ()
      | otherwise -> pure $ Left $ "Tool result JSON mismatch: " ++ show obj
    _ -> pure $ Left "messageToJSON did not produce an object for tool result"

-- | buildRequestBody omits tools and tool_choice when registry is empty.
testOpenAIRequestNoTools :: Test
testOpenAIRequestNoTools = do
  let msgs = [mkUserMessage "hello"]
      body = buildRequestBody "max_tokens" msgs "gpt-4o" 1024 mempty
  case decode body of
    Nothing -> pure $ Left "buildRequestBody (empty reg) produced invalid JSON"
    Just (Object obj) -> do
      let noTools      = KM.lookup (Key.fromText "tools") obj == Nothing
          noToolChoice = KM.lookup (Key.fromText "tool_choice") obj == Nothing
      if noTools && noToolChoice
        then pure $ Right ()
        else pure $ Left $ "Expected no tools/tool_choice, got: " ++ show obj
    Just other -> pure $ Left $ "Request JSON is not an object: " ++ show other

-- | toolsToJSON produces correct OpenAI format for each tool.
testOpenAIToolsSchema :: Test
testOpenAIToolsSchema = do
  let toolList = toolsToJSON defaultRegistry
  -- Each tool must be {"type":"function","function":{"name":...,"description":...,"parameters":...}}
  let checkTool val = case val of
        Object obj -> do
          let hasType = KM.lookup (Key.fromText "type") obj == Just (String "function")
          case KM.lookup (Key.fromText "function") obj of
            Just (Object fn) -> do
              let hasName = case KM.lookup (Key.fromText "name") fn of
                              Just (String _) -> True
                              _               -> False
                  hasDesc = case KM.lookup (Key.fromText "description") fn of
                              Just (String _) -> True
                              _               -> False
                  hasParams = case KM.lookup (Key.fromText "parameters") fn of
                                Just (Object _) -> True
                                _               -> False
              if hasType && hasName && hasDesc && hasParams
                then Right ()
                else Left $ "Tool schema fields missing: " ++ show val
            _ -> Left $ "Tool missing 'function' object: " ++ show val
        _ -> Left $ "Tool is not an object: " ++ show val
  case mapM_ checkTool toolList of
    Right () -> pure $ Right ()
    Left err -> pure $ Left err

-- | parseResponseBody handles content:null with tool_calls (explicit).
testOpenAIResponseNullContentToolCall :: Test
testOpenAIResponseNullContentToolCall = do
  let resp = encode $ object
        [ "id"      .= ("chatcmpl-null" :: T.Text)
        , "object"  .= ("chat.completion" :: T.Text)
        , "choices" .= [object
            [ "index" .= (0 :: Int)
            , "message" .= object
                [ "role"    .= ("assistant" :: T.Text)
                , "content" .= Null
                , "tool_calls" .= [object
                    [ "id"   .= ("call_null" :: T.Text)
                    , "type" .= ("function" :: T.Text)
                    , "function" .= object
                        [ "name"      .= ("list_files" :: T.Text)
                        , "arguments" .= ("{\"dir\":\".\"}" :: T.Text)
                        ]
                    ]]
                ]
            , "finish_reason" .= ("tool_calls" :: T.Text)
            ]]
        ]
  case parseResponseBody resp of
    Left err -> pure $ Left $ "Failed to parse null-content tool response: " ++ show err
    Right cr -> do
      let contentOk  = msgContent (crReply cr) == ""
          callIdOk   = case crToolCalls cr of
                         Just [tc] -> tcId tc == "call_null"
                         _         -> False
          nameOk     = case crToolCalls cr of
                         Just [tc] -> tcName tc == "list_files"
                         _         -> False
          argsOk     = case crToolCalls cr of
                         Just [tc] -> tcArgs tc == object ["dir" .= ("." :: T.Text)]
                         _         -> False
      if contentOk && callIdOk && nameOk && argsOk
        then pure $ Right ()
        else pure $ Left $ "Null-content parse mismatch: content="
                ++ show contentOk ++ " callId=" ++ show callIdOk
                ++ " name=" ++ show nameOk ++ " args=" ++ show argsOk

-- | buildSystemPrompt does NOT tell the model to print JSON tool calls.
testBuildSystemPromptNoJsonInstruction :: Test
testBuildSystemPromptNoJsonInstruction = do
  let prompt = buildSystemPrompt defaultRegistry Nothing
  -- The old prompt contained these strings; the new one must not.
  if T.isInfixOf "{\"tool_call\"" prompt
    then pure $ Left "System prompt still contains JSON tool_call instruction"
    else pure $ Right ()

-- | buildSystemPrompt tells the model to use the native tool mechanism.
testBuildSystemPromptNativeTools :: Test
testBuildSystemPromptNativeTools = do
  let prompt = buildSystemPrompt defaultRegistry Nothing
  if T.isInfixOf "tool-calling mechanism" prompt
     && T.isInfixOf "Available tools" prompt
    then pure $ Right ()
    else pure $ Left "System prompt missing native tool-calling guidance"

-- ---------------------------------------------------------------------------
-- | preview_patch description distinguishes it as read-only and
--   mentions it does not modify the filesystem.
testPreviewPatchDescriptionPhrases :: Test
testPreviewPatchDescriptionPhrases =
  case toolDescriptionFromRegistry "preview_patch" of
    Nothing -> pure $ Left "preview_patch not in registry"
    Just desc
      | not (T.isInfixOf "without modifying" desc || T.isInfixOf "NOT" desc || T.isInfixOf "Does NOT" desc)
        -> pure $ Left $ "preview_patch description missing read-only signal: " ++ T.unpack desc
      | not (T.isInfixOf "diff" desc)
        -> pure $ Left $ "preview_patch description missing 'diff': " ++ T.unpack desc
      | otherwise -> pure $ Right ()

-- | apply_patch description states it requires user confirmation,
--   applies to exactly one existing file, and cannot create/delete.
testApplyPatchDescriptionPhrases :: Test
testApplyPatchDescriptionPhrases =
  case toolDescriptionFromRegistry "apply_patch" of
    Nothing -> pure $ Left "apply_patch not in registry"
    Just desc
      | not (T.isInfixOf "confirmation" desc || T.isInfixOf "confirm" desc)
        -> pure $ Left $ "apply_patch description missing confirmation: " ++ T.unpack desc
      | not (T.isInfixOf "existing" desc)
        -> pure $ Left $ "apply_patch description missing 'existing': " ++ T.unpack desc
      | not (T.isInfixOf "one" desc || T.isInfixOf "single" desc || T.isInfixOf "exactly" desc)
        -> pure $ Left $ "apply_patch description missing single-file constraint: " ++ T.unpack desc
      | otherwise -> pure $ Right ()

-- | search description mentions case-insensitive option and .agentignore.
testSearchDescriptionPhrases :: Test
testSearchDescriptionPhrases =
  case toolDescriptionFromRegistry "search" of
    Nothing -> pure $ Left "search not in registry"
    Just desc
      | not (T.isInfixOf "case-insensitive" desc)
        -> pure $ Left $ "search description missing 'case-insensitive': " ++ T.unpack desc
      | not (T.isInfixOf ".agentignore" desc)
        -> pure $ Left $ "search description missing '.agentignore': " ++ T.unpack desc
      | otherwise -> pure $ Right ()

-- | glob description mentions .agentignore.
testGlobDescriptionPhrases :: Test
testGlobDescriptionPhrases =
  case toolDescriptionFromRegistry "glob" of
    Nothing -> pure $ Left "glob not in registry"
    Just desc
      | not (T.isInfixOf ".agentignore" desc)
        -> pure $ Left $ "glob description missing '.agentignore': " ++ T.unpack desc
      | otherwise -> pure $ Right ()

-- | shell description mentions confirmation and dangerous commands.
testShellDescriptionPhrases :: Test
testShellDescriptionPhrases =
  case toolDescriptionFromRegistry "shell" of
    Nothing -> pure $ Left "shell not in registry"
    Just desc
      | not (T.isInfixOf "confirmation" desc || T.isInfixOf "confirm" desc)
        -> pure $ Left $ "shell description missing confirmation: " ++ T.unpack desc
      | not (T.isInfixOf "dangerous" desc)
        -> pure $ Left $ "shell description missing 'dangerous': " ++ T.unpack desc
      | otherwise -> pure $ Right ()

-- | The system prompt includes tool descriptions that contain the
--   key safety phrases for the five critical tools.
testSystemPromptToolPhrases :: Test
testSystemPromptToolPhrases = do
  let prompt = buildSystemPrompt defaultRegistry Nothing
      checks :: [(T.Text, [T.Text])]
      checks =
        [ ("preview_patch",  ["without modifying", "NOT"])
        , ("apply_patch",    ["confirmation", "existing"])
        , ("search",         ["case-sensitive", ".agentignore"])
        , ("glob",           [".agentignore"])
        , ("shell",          ["confirmation", "dangerous"])
        , ("write_file",     ["confirmation", "overwrite"])
        ]
      missing = concat
        [ [ (tool, phrase)
          | phrase <- phrases
          , not (T.isInfixOf phrase prompt)
          ]
        | (tool, phrases) <- checks
        ]
  if null missing
    then pure $ Right ()
    else pure $ Left $ "System prompt missing phrases: "
                     ++ T.unpack (T.intercalate ", "
                          [ t <> ":\"" <> p <> "\"" | (t, p) <- missing ])

-- | The OpenAI tool schema for each tool contains a description field
--   with the key safety phrases (via toolsToJSON).
testToolSchemaPhrasesInWireFormat :: Test
testToolSchemaPhrasesInWireFormat = do
  let toolList = toolsToJSON defaultRegistry
      descByName name = case
        [ d | Object obj <- toolList
        , Just (Object fn) <- [KM.lookup (Key.fromText "function") obj]
        , Just (String n)  <- [KM.lookup (Key.fromText "name") fn]
        , n == name
        , Just (String d)  <- [KM.lookup (Key.fromText "description") fn]
        ] of
          (d:_) -> d
          []    -> ""
      checks =
        [ ("preview_patch",  ["without modifying"])
        , ("apply_patch",    ["confirmation"])
        , ("search",         ["case-sensitive"])
        , ("glob",           [".agentignore"])
        , ("shell",          ["dangerous"])
        , ("write_file",     ["confirmation"])
        ]
      missing = concat
        [ [ (tool, phrase)
          | phrase <- phrases
          , not (T.isInfixOf phrase (descByName tool))
          ]
        | (tool, phrases) <- checks
        ]
  if null missing
    then pure $ Right ()
    else pure $ Left $ "Tool schema descriptions missing phrases: "
                     ++ show [(t, p) | (t, p) <- missing]

-- | tokenLimitFieldName returns "max_completion_tokens" for openai.
testTokenLimitFieldOpenAI :: Test
testTokenLimitFieldOpenAI =
  let pc = ProviderConfig "openai" "gpt-4o" "https://api.openai.com" ""
  in if tokenLimitFieldName pc == "max_completion_tokens"
       then pure $ Right ()
       else pure $ Left $ "Expected max_completion_tokens, got: "
                         ++ T.unpack (tokenLimitFieldName pc)

-- | tokenLimitFieldName returns "max_tokens" for ollama.
testTokenLimitFieldOllama :: Test
testTokenLimitFieldOllama =
  let pc = ProviderConfig "ollama" "llama3.1" "http://localhost:11434" ""
  in if tokenLimitFieldName pc == "max_tokens"
       then pure $ Right ()
       else pure $ Left $ "Expected max_tokens, got: "
                         ++ T.unpack (tokenLimitFieldName pc)

-- | tokenLimitFieldName returns "max_tokens" for vllm, litellm, openrouter.
testTokenLimitFieldOtherProviders :: Test
testTokenLimitFieldOtherProviders =
  let check name = tokenLimitFieldName (ProviderConfig name "m" "http://x" "") == "max_tokens"
      names = ["vllm", "litellm", "openrouter"]
  in if all check names
       then pure $ Right ()
       else pure $ Left $ "Some providers did not return max_tokens"

-- | buildRequestBody with "max_completion_tokens" uses that field name.
testOpenAIRequestMaxCompletionTokens :: Test
testOpenAIRequestMaxCompletionTokens = do
  let msgs = [mkUserMessage "hello"]
      body = buildRequestBody "max_completion_tokens" msgs "gpt-4o-mini" 2048 mempty
  case decode body of
    Nothing -> pure $ Left "buildRequestBody (max_completion_tokens) invalid JSON"
    Just (Object obj) -> do
      let hasField  = KM.lookup (Key.fromText "max_completion_tokens") obj == Just (Number 2048)
          noOldField = KM.lookup (Key.fromText "max_tokens") obj == Nothing
      if hasField && noOldField
        then pure $ Right ()
        else pure $ Left $ "max_completion_tokens=" ++ show hasField
                         ++ " no max_tokens=" ++ show noOldField
    Just other -> pure $ Left $ "Request JSON is not an object: " ++ show other

-- | buildRequestBody with "max_tokens" uses that field name (ollama/compatible).
testOpenAIRequestMaxTokensOllama :: Test
testOpenAIRequestMaxTokensOllama = do
  let msgs = [mkUserMessage "hello"]
      body = buildRequestBody "max_tokens" msgs "llama3.1" 4096 mempty
  case decode body of
    Nothing -> pure $ Left "buildRequestBody (max_tokens ollama) invalid JSON"
    Just (Object obj) -> do
      let hasField   = KM.lookup (Key.fromText "max_tokens") obj == Just (Number 4096)
          noNewField = KM.lookup (Key.fromText "max_completion_tokens") obj == Nothing
      if hasField && noNewField
        then pure $ Right ()
        else pure $ Left $ "max_tokens=" ++ show hasField
                         ++ " no max_completion_tokens=" ++ show noNewField
    Just other -> pure $ Left $ "Request JSON is not an object: " ++ show other

-- | An assistant message with tool_calls serializes with the tool_calls
--   array in OpenAI wire format (id, type=function, function.name,
--   function.arguments as a JSON string).
testOpenAIAssistantToolCallsJSON :: Test
testOpenAIAssistantToolCallsJSON = do
  let tcs = [ ToolCall "call_1" "read_file" (object ["path" .= ("foo.hs" :: T.Text)])
            , ToolCall "call_2" "list_files" (object ["dir" .= ("." :: T.Text)])
            ]
      msg = Message Assistant "Let me check." Nothing (Just tcs)
      val = messageToJSON msg
  case val of
    Object obj -> do
      let roleOk = KM.lookup (Key.fromText "role") obj == Just (String "assistant")
          contentOk = KM.lookup (Key.fromText "content") obj == Just (String "Let me check.")
      case KM.lookup (Key.fromText "tool_calls") obj of
        Just (Array arr) | length arr == 2 -> do
          -- Check first tool call structure
          case arr V.! 0 of
            Object tc1 -> do
              let idOk   = KM.lookup (Key.fromText "id") tc1 == Just (String "call_1")
                  typeOk = KM.lookup (Key.fromText "type") tc1 == Just (String "function")
              case KM.lookup (Key.fromText "function") tc1 of
                Just (Object fn) -> do
                  let nameOk = KM.lookup (Key.fromText "name") fn == Just (String "read_file")
                      argsOk = case KM.lookup (Key.fromText "arguments") fn of
                                 Just (String a) -> a == "{\"path\":\"foo.hs\"}"
                                 _               -> False
                  if roleOk && contentOk && idOk && typeOk && nameOk && argsOk
                    then pure $ Right ()
                    else pure $ Left $ "Field mismatch: role=" ++ show roleOk
                             ++ " content=" ++ show contentOk ++ " id=" ++ show idOk
                             ++ " type=" ++ show typeOk ++ " name=" ++ show nameOk
                             ++ " args=" ++ show argsOk
                _ -> pure $ Left "tool_call missing function object"
            _ -> pure $ Left "tool_call[0] is not an object"
        _ -> pure $ Left $ "tool_calls missing or wrong length: " ++ show (KM.lookup (Key.fromText "tool_calls") obj)
    _ -> pure $ Left "messageToJSON did not produce an object"

-- | A full conversation with assistant tool_calls followed by tool
--   results serializes in correct OpenAI order with matching IDs.
testOpenAIConversationToolCallRoundTrip :: Test
testOpenAIConversationToolCallRoundTrip = do
  let tcs = [ToolCall "call_x1" "list_files" (object ["dir" .= ("." :: T.Text)])]
      msgs =
        [ mkUserMessage "List the files in this repo"
        , Message Assistant "Let me check." Nothing (Just tcs)
        , Message User "src\napp\ntest\n" (Just "call_x1") Nothing
        ]
      vals = messagesToJSON msgs
  case vals of
    [userVal, asstVal, toolVal] -> do
      -- User message
      let userRoleOk = case userVal of
            Object o -> KM.lookup (Key.fromText "role") o == Just (String "user")
            _        -> False
      -- Assistant message must have tool_calls
      let asstOk = case asstVal of
            Object o -> do
              let roleOk = KM.lookup (Key.fromText "role") o == Just (String "assistant")
                  hasTcs = case KM.lookup (Key.fromText "tool_calls") o of
                             Just (Array arr) -> length arr == 1
                             _                -> False
              roleOk && hasTcs
            _ -> False
      -- Tool result must have role=tool and matching tool_call_id
      let toolOk = case toolVal of
            Object o -> do
              let roleOk   = KM.lookup (Key.fromText "role") o == Just (String "tool")
                  callIdOk = KM.lookup (Key.fromText "tool_call_id") o == Just (String "call_x1")
                  contentOk = KM.lookup (Key.fromText "content") o == Just (String "src\napp\ntest\n")
              roleOk && callIdOk && contentOk
            _ -> False
      if userRoleOk && asstOk && toolOk
        then pure $ Right ()
        else pure $ Left $ "user=" ++ show userRoleOk
                         ++ " asst=" ++ show asstOk
                         ++ " tool=" ++ show toolOk
    _ -> pure $ Left $ "Expected 3 messages, got " ++ show (length vals)

-- | The tool_call ID in the assistant message matches the tool_call_id
--   in the subsequent tool result message.
testOpenAIToolCallIdMatch :: Test
testOpenAIToolCallIdMatch = do
  let tcs = [ToolCall "call_match_42" "read_file" (object ["path" .= ("x.hs" :: T.Text)])]
      asstMsg = Message Assistant "" Nothing (Just tcs)
      toolMsg = Message User "file contents" (Just "call_match_42") Nothing
      asstVal = messageToJSON asstMsg
      toolVal = messageToJSON toolMsg
  -- Extract the tool_call id from assistant message
  let asstCallId = case asstVal of
        Object o -> case KM.lookup (Key.fromText "tool_calls") o of
          Just (Array arr) -> case arr V.! 0 of
            Object tc -> case KM.lookup (Key.fromText "id") tc of
              Just (String i) -> Just i
              _               -> Nothing
            _ -> Nothing
          _ -> Nothing
        _ -> Nothing
  -- Extract the tool_call_id from tool message
  let toolCallId = case toolVal of
        Object o -> case KM.lookup (Key.fromText "tool_call_id") o of
          Just (String i) -> Just i
          _               -> Nothing
        _ -> Nothing
  if asstCallId == Just "call_match_42" && toolCallId == Just "call_match_42" && asstCallId == toolCallId
    then pure $ Right ()
    else pure $ Left $ "ID mismatch: asst=" ++ show asstCallId ++ " tool=" ++ show toolCallId

-- ---------------------------------------------------------------------------

-- ---------------------------------------------------------------------------
-- SSE parsing tests (pure)
-- ---------------------------------------------------------------------------

-- | parseSSELine extracts content from a data line.
testParseSSELineData :: Test
testParseSSELineData =
  case parseSSELine (BS8.pack "data: {\"choices\":[{\"delta\":{\"content\":\"hi\"}}]}") of
    Just (Just _) -> pure $ Right ()
    other -> pure $ Left $ "Expected Just (Just _), got " ++ show other

-- | parseSSELine returns Nothing for [DONE].
testParseSSELineDone :: Test
testParseSSELineDone =
  case parseSSELine (BS8.pack "data: [DONE]") of
    Just Nothing -> pure $ Right ()
    other -> pure $ Left $ "Expected Just Nothing, got " ++ show other

-- | parseSSELine returns Nothing for empty lines.
testParseSSELineEmpty :: Test
testParseSSELineEmpty =
  case parseSSELine BS.empty of
    Nothing -> pure $ Right ()
    other -> pure $ Left $ "Expected Nothing, got " ++ show other

-- | parseSSELine returns Nothing for non-data lines.
testParseSSELineNonData :: Test
testParseSSELineNonData =
  case parseSSELine (BS8.pack "event: message") of
    Nothing -> pure $ Right ()
    other -> pure $ Left $ "Expected Nothing, got " ++ show other

-- | parseSSELine handles leading/trailing whitespace.
testParseSSELineWhitespace :: Test
testParseSSELineWhitespace =
  case parseSSELine (BS8.pack "  data: {\"test\":1}  ") of
    Just (Just _) -> pure $ Right ()
    other -> pure $ Left $ "Expected Just (Just _), got " ++ show other

-- | parseSSEEvent extracts content from a valid delta event.
testParseSSEEventContent :: Test
testParseSSEEventContent =
  let json = BS8.pack "{\"choices\":[{\"delta\":{\"content\":\"Hello\"}}]}"
  in case parseSSEEvent json of
       Right (Just "Hello") -> pure $ Right ()
       other -> pure $ Left $ "Expected Right (Just \"Hello\"), got " ++ show other

-- | parseSSEEvent returns Nothing for a role-only delta.
testParseSSEEventRoleDelta :: Test
testParseSSEEventRoleDelta =
  let json = BS8.pack "{\"choices\":[{\"delta\":{\"role\":\"assistant\"}}]}"
  in case parseSSEEvent json of
       Right Nothing -> pure $ Right ()
       other -> pure $ Left $ "Expected Right Nothing, got " ++ show other

-- | parseSSEEvent returns Left for malformed JSON.
testParseSSEEventMalformed :: Test
testParseSSEEventMalformed =
  case parseSSEEvent (BS8.pack "not json") of
    Left _ -> pure $ Right ()
    other -> pure $ Left $ "Expected Left _, got " ++ show other

-- | parseSSEEvent handles missing choices array.
testParseSSEEventNoChoices :: Test
testParseSSEEventNoChoices =
  let json = BS8.pack "{\"id\":\"chatcmpl-123\"}"
  in case parseSSEEvent json of
       Right Nothing -> pure $ Right ()
       other -> pure $ Left $ "Expected Right Nothing, got " ++ show other

-- | parseDeltaContent extracts text from a valid delta.
testParseDeltaContentValid :: Test
testParseDeltaContentValid =
  let val = object
        [ "choices" .=
            [ object [ "delta" .= object [ "content" .= ("world" :: T.Text) ] ] ]
        ]
  in case parseDeltaContent val of
       Just "world" -> pure $ Right ()
       other -> pure $ Left $ "Expected Just \"world\", got " ++ show other

-- | parseDeltaContent returns Nothing for null content.
testParseDeltaContentNull :: Test
testParseDeltaContentNull =
  let val = object
        [ "choices" .=
            [ object [ "delta" .= object [ "content" .= ("" :: T.Text) ] ] ]
        ]
  in case parseDeltaContent val of
       Nothing -> pure $ Right ()
       other -> pure $ Left $ "Expected Nothing, got " ++ show other

-- | parseDeltaContent returns Nothing for non-object input.
testParseDeltaContentNonObject :: Test
testParseDeltaContentNonObject =
  case parseDeltaContent (String "not an object") of
    Nothing -> pure $ Right ()
    other -> pure $ Left $ "Expected Nothing, got " ++ show other

-- | Multiple content deltas can be assembled into a full reply.
testParseMultipleDeltas :: Test
testParseMultipleDeltas = do
  ref <- Data.IORef.newIORef ("" :: T.Text)
  let handler = StreamHandler { onToken = \t -> Data.IORef.modifyIORef ref (<> t) }
      deltas :: [T.Text]
      deltas = ["Hello", " ", "world", "!"]
      processDelta d = case parseDeltaContent
            (object [ "choices" .= [ object [ "delta" .= object [ "content" .= d ] ] ] ]) of
        Just t -> onToken handler t
        Nothing -> pure ()
  mapM_ processDelta deltas
  result <- Data.IORef.readIORef ref
  if result == "Hello world!"
    then pure $ Right ()
    else pure $ Left $ "Expected \"Hello world!\", got " ++ T.unpack result

-- | parseSSELine with data line containing spaces after colon.
testParseSSELineExtraSpaces :: Test
testParseSSELineExtraSpaces =
  case parseSSELine (BS8.pack "data:   {\"test\":1}") of
    Just (Just payload) ->
      -- The payload should include the leading spaces (they're part of the JSON data)
      if payload == BS8.pack "  {\"test\":1}"
        then pure $ Right ()
        else pure $ Left $ "Unexpected payload: " ++ show payload
    other -> pure $ Left $ "Expected Just (Just _), got " ++ show other

-- | buildStreamingRequestBody includes stream:true.
testBuildStreamingRequestBodyIncludesStream :: Test
testBuildStreamingRequestBodyIncludesStream = do
  let reg = defaultRegistry
      body = buildStreamingRequestBody "max_tokens" [] "gpt-4o" 1024 reg
      decoded = eitherDecode body :: Either String Value
  case decoded of
    Left err -> pure $ Left $ "Failed to decode streaming body: " ++ err
    Right val -> case val of
      Object obj -> case KM.lookup (Key.fromText "stream") obj of
        Just (Bool True) -> pure $ Right ()
        other -> pure $ Left $ "Expected stream=true, got " ++ show other
      _ -> pure $ Left "Expected JSON object"

-- ---------------------------------------------------------------------------
-- Streamed tool-call assembly tests (pure)
-- ---------------------------------------------------------------------------

-- | A single complete tool call in one SSE event is assembled correctly.
testStreamToolCallSingleComplete :: Test
testStreamToolCallSingleComplete = do
  let val = object
        [ "choices" .= [object
            [ "delta" .= object
                [ "tool_calls" .= [object
                    [ "index"    .= (0 :: Int)
                    , "id"       .= ("call_001" :: T.Text)
                    , "type"     .= ("function" :: T.Text)
                    , "function" .= object
                        [ "name"      .= ("read_file" :: T.Text)
                        , "arguments" .= ("{\"path\":\"foo.hs\"}" :: T.Text)
                        ]
                    ]]
                ]]
            ]]
  case parseDeltaToolCalls val of
    Nothing -> pure $ Left "Expected tool-call deltas, got Nothing"
    Just frags -> do
      let stcs = foldl (\acc (i, f) -> Map.insert i f acc) Map.empty frags
      case assembleStreamToolCalls stcs of
        Left err -> pure $ Left $ "Assembly error: " ++ err
        Right [tc]
          | tcId tc == "call_001" && tcName tc == "read_file"
            && tcArgs tc == object ["path" .= ("foo.hs" :: T.Text)]
            -> pure $ Right ()
          | otherwise -> pure $ Left $ "Tool call mismatch: " ++ show tc
        Right tcs -> pure $ Left $ "Expected 1 tool call, got " ++ show (length tcs)

-- | One tool call split across multiple argument fragments assembles
--   the arguments in order.
testStreamToolCallFragmentedArgs :: Test
testStreamToolCallFragmentedArgs = do
  -- Three SSE events: first sends id+name+partial args, second sends more args, third sends rest
  let frag1 = object
        [ "choices" .= [object
            [ "delta" .= object
                [ "tool_calls" .= [object
                    [ "index"     .= (0 :: Int)
                    , "id"        .= ("call_frag" :: T.Text)
                    , "type"      .= ("function" :: T.Text)
                    , "function"  .= object
                        [ "name"      .= ("shell" :: T.Text)
                        , "arguments" .= ("{\"command\":" :: T.Text)
                        ]
                    ]]
                ]]
            ]]
      frag2 = object
        [ "choices" .= [object
            [ "delta" .= object
                [ "tool_calls" .= [object
                    [ "index"     .= (0 :: Int)
                    , "function"  .= object
                        [ "arguments" .= ("\"echo " :: T.Text)
                        ]
                    ]]
                ]]
            ]]
      frag3 = object
        [ "choices" .= [object
            [ "delta" .= object
                [ "tool_calls" .= [object
                    [ "index"     .= (0 :: Int)
                    , "function"  .= object
                        [ "arguments" .= ("hello\"}" :: T.Text)
                        ]
                    ]]
                ]]
            ]]
  let allFrags = concat
        [ case parseDeltaToolCalls frag1 of Just fs -> fs; Nothing -> []
        , case parseDeltaToolCalls frag2 of Just fs -> fs; Nothing -> []
        , case parseDeltaToolCalls frag3 of Just fs -> fs; Nothing -> []
        ]
  let stcs = foldl (\acc (i, f) ->
        case Map.lookup i acc of
          Nothing -> Map.insert i f acc
          Just existing -> Map.insert i
            (existing { stcArgs = stcArgs existing <> stcArgs f }) acc
        ) Map.empty allFrags
  case assembleStreamToolCalls stcs of
    Left err -> pure $ Left $ "Assembly error: " ++ err
    Right [tc]
      | tcId tc == "call_frag" && tcName tc == "shell"
        && tcArgs tc == object ["command" .= ("echo hello" :: T.Text)]
        -> pure $ Right ()
      | otherwise -> pure $ Left $ "Mismatch: " ++ show (tcId tc, tcName tc, tcArgs tc)
    Right tcs -> pure $ Left $ "Expected 1 tool call, got " ++ show (length tcs)

-- | Multiple tool calls distinguished by index are assembled correctly.
testStreamToolCallMultipleByIndex :: Test
testStreamToolCallMultipleByIndex = do
  let frag1 = object
        [ "choices" .= [object
            [ "delta" .= object
                [ "tool_calls" .= [object
                    [ "index" .= (0 :: Int)
                    , "id"    .= ("call_a" :: T.Text)
                    , "type"  .= ("function" :: T.Text)
                    , "function" .= object
                        [ "name"      .= ("read_file" :: T.Text)
                        , "arguments" .= ("{\"path\":\"a.hs\"}" :: T.Text)
                        ]
                    ]]
                ]]
            ]]
      frag2 = object
        [ "choices" .= [object
            [ "delta" .= object
                [ "tool_calls" .= [object
                    [ "index" .= (1 :: Int)
                    , "id"    .= ("call_b" :: T.Text)
                    , "type"  .= ("function" :: T.Text)
                    , "function" .= object
                        [ "name"      .= ("list_files" :: T.Text)
                        , "arguments" .= ("{\"dir\":\".\"}" :: T.Text)
                        ]
                    ]]
                ]]
            ]]
  let allFrags = concat
        [ case parseDeltaToolCalls frag1 of Just fs -> fs; Nothing -> []
        , case parseDeltaToolCalls frag2 of Just fs -> fs; Nothing -> []
        ]
  let stcs = foldl (\acc (i, f) ->
        case Map.lookup i acc of
          Nothing -> Map.insert i f acc
          Just existing -> Map.insert i
            (StreamingToolCall
              { stcIndex = i
              , stcId    = stcId f `mplus''` stcId existing
              , stcName  = stcName f `mplus''` stcName existing
              , stcArgs  = stcArgs existing <> stcArgs f
              }) acc
        ) Map.empty allFrags
  case assembleStreamToolCalls stcs of
    Left err -> pure $ Left $ "Assembly error: " ++ err
    Right [tc1, tc2]
      | tcName tc1 == "read_file" && tcName tc2 == "list_files"
        && tcId tc1 == "call_a" && tcId tc2 == "call_b"
        -> pure $ Right ()
      | otherwise -> pure $ Left $ "Mismatch: " ++ show (tcName tc1, tcName tc2)
    Right tcs -> pure $ Left $ "Expected 2 tool calls, got " ++ show (length tcs)
  where
    mplus'' :: Maybe a -> Maybe a -> Maybe a
    mplus'' (Just x) _ = Just x
    mplus'' Nothing  y = y

-- | Text-only streaming: parseDeltaToolCalls returns Nothing for
--   content-only events.
testStreamToolCallTextOnlyNoToolCalls :: Test
testStreamToolCallTextOnlyNoToolCalls =
  let val = object
        [ "choices" .= [object
            [ "delta" .= object [ "content" .= ("Hello world" :: T.Text) ]
            ]]
        ]
  in case parseDeltaToolCalls val of
       Nothing -> pure $ Right ()
       Just _  -> pure $ Left "Expected Nothing for text-only delta, got Just"

-- | Role-only or empty deltas produce no tool-call fragments.
testStreamToolCallRoleOnlyDelta :: Test
testStreamToolCallRoleOnlyDelta =
  let val = object
        [ "choices" .= [object
            [ "delta" .= object [ "role" .= ("assistant" :: T.Text) ]
            ]]
        ]
  in case parseDeltaToolCalls val of
       Nothing -> pure $ Right ()
       Just _  -> pure $ Left "Expected Nothing for role-only delta, got Just"

-- | Empty delta (no fields) produces no tool-call fragments.
testStreamToolCallEmptyDelta :: Test
testStreamToolCallEmptyDelta =
  let val = object
        [ "choices" .= [object
            [ "delta" .= object []
            ]]
        ]
  in case parseDeltaToolCalls val of
       Nothing -> pure $ Right ()
       Just _  -> pure $ Left "Expected Nothing for empty delta, got Just"

-- | Malformed tool-call arguments at assembly time produce a clear error.
testStreamToolCallMalformedArgs :: Test
testStreamToolCallMalformedArgs =
  let stcs = Map.fromList
        [ (0, StreamingToolCall
              { stcIndex = 0
              , stcId    = Just "call_bad"
              , stcName  = Just "read_file"
              , stcArgs  = "not valid json"
              })
        ]
  in case assembleStreamToolCalls stcs of
       Left err
         | "malformed" `T.isInfixOf` T.pack err -> pure $ Right ()
         | otherwise -> pure $ Left $ "Expected 'malformed' in error, got: " ++ err
       Right _ -> pure $ Left "Expected Left for malformed args, got Right"

-- | Missing id in assembled tool call produces a clear error.
testStreamToolCallMissingId :: Test
testStreamToolCallMissingId =
  let stcs = Map.fromList
        [ (0, StreamingToolCall
              { stcIndex = 0
              , stcId    = Nothing
              , stcName  = Just "read_file"
              , stcArgs  = "{}"
              })
        ]
  in case assembleStreamToolCalls stcs of
       Left err
         | "missing" `T.isInfixOf` T.pack err
           && "id" `T.isInfixOf` T.pack err -> pure $ Right ()
         | otherwise -> pure $ Left $ "Expected 'missing id' in error, got: " ++ err
       Right _ -> pure $ Left "Expected Left for missing id, got Right"

-- | Missing function.name in assembled tool call produces a clear error.
testStreamToolCallMissingName :: Test
testStreamToolCallMissingName =
  let stcs = Map.fromList
        [ (0, StreamingToolCall
              { stcIndex = 0
              , stcId    = Just "call_noname"
              , stcName  = Nothing
              , stcArgs  = "{}"
              })
        ]
  in case assembleStreamToolCalls stcs of
       Left err
         | "missing" `T.isInfixOf` T.pack err
           && "name" `T.isInfixOf` T.pack err -> pure $ Right ()
         | otherwise -> pure $ Left $ "Expected 'missing name' in error, got: " ++ err
       Right _ -> pure $ Left "Expected Left for missing name, got Right"

-- | Empty tool-call map produces an empty list (no tool calls).
testStreamToolCallEmptyAssembly :: Test
testStreamToolCallEmptyAssembly =
  case assembleStreamToolCalls Map.empty of
    Right [] -> pure $ Right ()
    Right tcs -> pure $ Left $ "Expected empty list, got " ++ show (length tcs)
    Left err -> pure $ Left $ "Unexpected error: " ++ err

-- | Final assembled CompletionResponse has crToolCalls = Just for tool-call streams.
testStreamToolCallCompletionResponse :: Test
testStreamToolCallCompletionResponse =
  let stcs = Map.fromList
        [ (0, StreamingToolCall
              { stcIndex = 0
              , stcId    = Just "call_resp"
              , stcName  = Just "list_files"
              , stcArgs  = "{\"dir\":\".\"}"
              })
        ]
  in case assembleStreamToolCalls stcs of
       Left err -> pure $ Left $ "Assembly error: " ++ err
       Right tcs -> do
         let resp = CompletionResponse
               { crReply     = Message Assistant "" Nothing Nothing
               , crToolCalls = Just tcs
               }
         case crToolCalls resp of
           Just [tc]
             | tcId tc == "call_resp" && tcName tc == "list_files"
               -> pure $ Right ()
             | otherwise -> pure $ Left $ "Response tool call mismatch: " ++ show tc
           Just tcs' -> pure $ Left $ "Expected 1 tool call in response, got " ++ show (length tcs')
           Nothing -> pure $ Left "Expected Just tool calls in response, got Nothing"


tests :: [Test]
tests =
  [ testOpenAIRequestShape
  , testOpenAIRequestTools
  , testOpenAIResponseText
  , testOpenAIResponseToolCall
  , testOpenAIResponseMultiToolCall
  , testOpenAIMalformedResponse
  , testOpenAIMissingChoices
  , testOpenAIParseToolCall
  , testOpenAIParseToolCallMissing
  , testOpenAIMessageToJSON
  , testOpenAIToolResultToJSON
  , testOpenAIRequestNoTools
  , testOpenAIToolsSchema
  , testOpenAIResponseNullContentToolCall
  , testBuildSystemPromptNoJsonInstruction
  , testBuildSystemPromptNativeTools
  , testPreviewPatchDescriptionPhrases
  , testApplyPatchDescriptionPhrases
  , testSearchDescriptionPhrases
  , testGlobDescriptionPhrases
  , testShellDescriptionPhrases
  , testSystemPromptToolPhrases
  , testToolSchemaPhrasesInWireFormat
  , testTokenLimitFieldOpenAI
  , testTokenLimitFieldOllama
  , testTokenLimitFieldOtherProviders
  , testOpenAIRequestMaxCompletionTokens
  , testOpenAIRequestMaxTokensOllama
  , testOpenAIAssistantToolCallsJSON
  , testOpenAIConversationToolCallRoundTrip
  , testOpenAIToolCallIdMatch
  , testParseSSELineData
  , testParseSSELineDone
  , testParseSSELineEmpty
  , testParseSSELineNonData
  , testParseSSELineWhitespace
  , testParseSSELineExtraSpaces
  , testParseSSEEventContent
  , testParseSSEEventRoleDelta
  , testParseSSEEventMalformed
  , testParseSSEEventNoChoices
  , testParseDeltaContentValid
  , testParseDeltaContentNull
  , testParseDeltaContentNonObject
  , testParseMultipleDeltas
  , testBuildStreamingRequestBodyIncludesStream
  , testStreamToolCallSingleComplete
  , testStreamToolCallFragmentedArgs
  , testStreamToolCallMultipleByIndex
  , testStreamToolCallTextOnlyNoToolCalls
  , testStreamToolCallRoleOnlyDelta
  , testStreamToolCallEmptyDelta
  , testStreamToolCallMalformedArgs
  , testStreamToolCallMissingId
  , testStreamToolCallMissingName
  , testStreamToolCallEmptyAssembly
  , testStreamToolCallCompletionResponse
  ]
