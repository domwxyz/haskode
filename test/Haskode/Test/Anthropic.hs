{-# LANGUAGE OverloadedStrings #-}

-- | Anthropic provider JSON conversion tests.
module Haskode.Test.Anthropic (tests) where

import Control.Exception (finally)
import Data.Aeson
    ( Value(..), decode, encode, object, (.=) )
import Data.Maybe (isJust)
import qualified Data.Aeson.Key as Key (fromText)
import qualified Data.Aeson.KeyMap as KM (lookup)
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T (Text, isInfixOf, pack)
import qualified Data.Vector as V (toList)
import System.Environment (lookupEnv, setEnv, unsetEnv)

import Haskode.Config
    ( Config(..), ProviderConfig(..), defaultConfig )
import Haskode.Core
    ( Message(..), Role(..), ToolCall(..), mkSystemMessage, mkUserMessage )
import Haskode.Provider
    ( CompletionResponse(..), Provider(..) )
import Haskode.Provider.Anthropic
    ( AnthropicError(..), AnthropicStreamEvent(..),
      AnthropicStreamState(..), applyStreamEvent, assembleStreamEvents,
      buildRequestBody, buildStreamingRequestBody, emptyAnthropicStreamState,
      messageToJSON, parseResponseBody, parseSSELine, parseStreamEvent,
      resolveAnthropicApiKey, systemMessagesToText, toolsToJSON,
      validateMessages, anthropicProvider )
import Haskode.Test.Util (Test)
import Haskode.Tools (defaultRegistry, toolNames)

-- | Temporarily set or clear ANTHROPIC_API_KEY for constructor tests.
withAnthropicApiKey :: Maybe String -> IO a -> IO a
withAnthropicApiKey value action = do
  old <- lookupEnv "ANTHROPIC_API_KEY"
  let setNew = case value of
        Nothing -> unsetEnv "ANTHROPIC_API_KEY"
        Just v  -> setEnv "ANTHROPIC_API_KEY" v
      restore = case old of
        Nothing -> unsetEnv "ANTHROPIC_API_KEY"
        Just v  -> setEnv "ANTHROPIC_API_KEY" v
  setNew
  action `finally` restore

anthropicConfig :: String -> Config
anthropicConfig key = defaultConfig
  { cfgProvider = ProviderConfig
      { pcProvider = "anthropic"
      , pcModel    = "claude-3-5-sonnet-latest"
      , pcBaseUrl  = ""
      , pcApiKey   = key
      }
  }

decodeRequestBody :: Either AnthropicError LBS.ByteString -> Either String Value
decodeRequestBody result = do
  body <- case result of
    Left err -> Left $ "buildRequestBody failed: " ++ show err
    Right b  -> Right b
  case decode body of
    Nothing  -> Left "buildRequestBody produced invalid JSON"
    Just val -> Right val

-- | buildRequestBody uses Anthropic's Messages shape and moves system
--   messages to the top-level system field.
testAnthropicRequestShape :: Test
testAnthropicRequestShape = do
  let msgs =
        [ mkSystemMessage "system instructions"
        , mkUserMessage "hello"
        ]
      body = buildRequestBody msgs "claude-test" 1024 mempty
  case decodeRequestBody body of
    Left err -> pure $ Left err
    Right (Object obj) -> do
      let hasModel  = KM.lookup (Key.fromText "model") obj == Just (String "claude-test")
          hasMaxTok = KM.lookup (Key.fromText "max_tokens") obj == Just (Number 1024)
          hasSystem = KM.lookup (Key.fromText "system") obj == Just (String "system instructions")
          messagesOk = case KM.lookup (Key.fromText "messages") obj of
            Just (Array arr) -> case V.toList arr of
              [Object msg] ->
                KM.lookup (Key.fromText "role") msg == Just (String "user")
                && KM.lookup (Key.fromText "content") msg == Just (String "hello")
              _ -> False
            _ -> False
      if hasModel && hasMaxTok && hasSystem && messagesOk
        then pure $ Right ()
        else pure $ Left $ "Request JSON missing expected fields: " ++ show obj
    Right other -> pure $ Left $ "Request JSON is not an object: " ++ show other

-- | The Anthropic request includes tools using input_schema when the
--   registry is non-empty.
testAnthropicRequestTools :: Test
testAnthropicRequestTools = do
  let body = buildRequestBody [mkUserMessage "hello"] "claude-test" 1024 defaultRegistry
  case decodeRequestBody body of
    Left err -> pure $ Left err
    Right (Object obj) -> do
      let hasTools = case KM.lookup (Key.fromText "tools") obj of
            Just (Array arr) -> length arr == length (toolNames defaultRegistry)
            _                -> False
          firstToolOk = case toolsToJSON defaultRegistry of
            (Object toolObj : _) ->
              case KM.lookup (Key.fromText "input_schema") toolObj of
                Just (Object _) -> True
                _               -> False
            _ -> False
      if hasTools && firstToolOk
        then pure $ Right ()
        else pure $ Left $ "tools=" ++ show hasTools
                        ++ " firstToolOk=" ++ show firstToolOk
    Right other -> pure $ Left $ "Request JSON is not an object: " ++ show other

-- | Streaming request bodies keep the normal Messages shape and add
--   stream:true.
testAnthropicStreamingRequestShape :: Test
testAnthropicStreamingRequestShape = do
  let body = buildStreamingRequestBody [mkUserMessage "hello"] "claude-test" 1024 mempty
  case decodeRequestBody body of
    Left err -> pure $ Left err
    Right (Object obj) -> do
      let hasStream = KM.lookup (Key.fromText "stream") obj == Just (Bool True)
          hasMessages = case KM.lookup (Key.fromText "messages") obj of
            Just (Array arr) -> length arr == 1
            _                -> False
      if hasStream && hasMessages
        then pure $ Right ()
        else pure $ Left $ "Streaming request mismatch: " ++ show obj
    Right other -> pure $ Left $ "Request JSON is not an object: " ++ show other

-- | Assistant tool calls map to Anthropic tool_use content blocks.
testAnthropicAssistantToolUseJSON :: Test
testAnthropicAssistantToolUseJSON = do
  let tc = ToolCall "toolu_1" "read_file" (object ["path" .= ("README.md" :: T.Text)])
      msg = Message Assistant "Reading." Nothing (Just [tc])
  case messageToJSON msg of
    Object obj -> case KM.lookup (Key.fromText "content") obj of
      Just (Array arr) -> case V.toList arr of
        [Object textBlock, Object toolBlock] -> do
          let textOk = KM.lookup (Key.fromText "type") textBlock == Just (String "text")
              toolOk = KM.lookup (Key.fromText "type") toolBlock == Just (String "tool_use")
                    && KM.lookup (Key.fromText "id") toolBlock == Just (String "toolu_1")
                    && KM.lookup (Key.fromText "name") toolBlock == Just (String "read_file")
          if textOk && toolOk
            then pure $ Right ()
            else pure $ Left $ "tool_use block mismatch: " ++ show toolBlock
        other -> pure $ Left $ "Expected text + tool_use blocks, got " ++ show other
      other -> pure $ Left $ "Expected content array, got " ++ show other
    other -> pure $ Left $ "messageToJSON did not produce object: " ++ show other

-- | Tool-result messages map to Anthropic tool_result content blocks.
testAnthropicToolResultJSON :: Test
testAnthropicToolResultJSON = do
  let msg = Message User "file contents" (Just "toolu_1") Nothing
  case messageToJSON msg of
    Object obj -> case KM.lookup (Key.fromText "content") obj of
      Just (Array arr) -> case V.toList arr of
        [Object block] -> do
          let roleOk = KM.lookup (Key.fromText "role") obj == Just (String "user")
              blockOk = KM.lookup (Key.fromText "type") block == Just (String "tool_result")
                     && KM.lookup (Key.fromText "tool_use_id") block == Just (String "toolu_1")
                     && KM.lookup (Key.fromText "content") block == Just (String "file contents")
          if roleOk && blockOk
            then pure $ Right ()
            else pure $ Left $ "tool_result block mismatch: " ++ show block
        other -> pure $ Left $ "Expected one tool_result block, got " ++ show other
      other -> pure $ Left $ "Expected content array, got " ++ show other
    other -> pure $ Left $ "messageToJSON did not produce object: " ++ show other

-- | systemMessagesToText joins only system messages.
testAnthropicSystemMessages :: Test
testAnthropicSystemMessages =
  let msgs =
        [ mkSystemMessage "first"
        , mkUserMessage "ignored"
        , mkSystemMessage "second"
        ]
  in if systemMessagesToText msgs == "first\n\nsecond"
       then pure $ Right ()
       else pure $ Left $ "Unexpected system text: " ++ show (systemMessagesToText msgs)

-- | Anthropic requests must include at least one non-system message.
testAnthropicRejectsSystemOnlyRequest :: Test
testAnthropicRejectsSystemOnlyRequest =
  case buildRequestBody [mkSystemMessage "system only"] "claude-test" 1024 mempty of
    Left (AnthropicRequestError msg)
      | "non-system" `T.isInfixOf` T.pack msg -> pure $ Right ()
      | otherwise -> pure $ Left $ "Expected non-system error, got: " ++ msg
    Left other -> pure $ Left $ "Expected AnthropicRequestError, got " ++ show other
    Right _ -> pure $ Left "Expected system-only request to fail"

-- | Non-assistant messages cannot contain tool_use blocks.
testAnthropicRejectsUserToolUse :: Test
testAnthropicRejectsUserToolUse =
  let tc = ToolCall "toolu_bad" "read_file" (object ["path" .= ("README.md" :: T.Text)])
      msg = Message User "" Nothing (Just [tc])
  in case validateMessages [msg] of
       Left (AnthropicRequestError msgText)
         | "assistant" `T.isInfixOf` T.pack msgText -> pure $ Right ()
         | otherwise -> pure $ Left $ "Expected assistant-only error, got: " ++ msgText
       Left other -> pure $ Left $ "Expected AnthropicRequestError, got " ++ show other
       Right _ -> pure $ Left "Expected user tool_use message to fail"

-- | Empty assistant tool-call lists should fail before producing
--   invalid empty Anthropic content arrays.
testAnthropicRejectsEmptyToolUse :: Test
testAnthropicRejectsEmptyToolUse =
  let msg = Message Assistant "" Nothing (Just [])
  in case validateMessages [msg] of
       Left (AnthropicRequestError msgText)
         | "at least one tool call" `T.isInfixOf` T.pack msgText -> pure $ Right ()
         | otherwise -> pure $ Left $ "Expected empty-tool-call error, got: " ++ msgText
       Left other -> pure $ Left $ "Expected AnthropicRequestError, got " ++ show other
       Right _ -> pure $ Left "Expected empty tool_use message to fail"

-- | Anthropic key resolution prefers pcApiKey, then ANTHROPIC_API_KEY.
--   The CLI applies --api-key by overriding pcApiKey before provider
--   construction, so this also covers the provider-visible CLI case.
testAnthropicApiKeyPrecedence :: Test
testAnthropicApiKeyPrecedence =
  let cliOrConfigWins = resolveAnthropicApiKey "explicit-key" (Just "env-key")
      envFallback     = resolveAnthropicApiKey "" (Just "env-key")
      missing         = resolveAnthropicApiKey "" Nothing
  in if cliOrConfigWins == Just "explicit-key"
        && envFallback == Just "env-key"
        && missing == Nothing
       then pure $ Right ()
       else pure $ Left $ "Unexpected key precedence: "
            ++ show (cliOrConfigWins, envFallback, missing)

sampleTextResponse :: Value
sampleTextResponse = object
  [ "id"      .= ("msg_1" :: T.Text)
  , "type"    .= ("message" :: T.Text)
  , "role"    .= ("assistant" :: T.Text)
  , "content" .= [object
      [ "type" .= ("text" :: T.Text)
      , "text" .= ("Hello from Claude." :: T.Text)
      ]]
  , "stop_reason" .= ("end_turn" :: T.Text)
  ]

sampleToolUseResponse :: Value
sampleToolUseResponse = object
  [ "id"      .= ("msg_2" :: T.Text)
  , "type"    .= ("message" :: T.Text)
  , "role"    .= ("assistant" :: T.Text)
  , "content" .=
      [ object
          [ "type" .= ("text" :: T.Text)
          , "text" .= ("I will read it." :: T.Text)
          ]
      , object
          [ "type"  .= ("tool_use" :: T.Text)
          , "id"    .= ("toolu_2" :: T.Text)
          , "name"  .= ("read_file" :: T.Text)
          , "input" .= object ["path" .= ("README.md" :: T.Text)]
          ]
      ]
  , "stop_reason" .= ("tool_use" :: T.Text)
  ]

-- | parseResponseBody handles a plain text Anthropic reply.
testAnthropicResponseText :: Test
testAnthropicResponseText =
  case parseResponseBody (encode sampleTextResponse) of
    Left err -> pure $ Left $ "Failed to parse text response: " ++ show err
    Right resp
      | msgContent (crReply resp) == "Hello from Claude."
        && crToolCalls resp == Nothing -> pure $ Right ()
      | otherwise -> pure $ Left $ "Unexpected response: " ++ show resp

-- | parseResponseBody maps Anthropic tool_use blocks to Haskode ToolCall.
testAnthropicResponseToolUse :: Test
testAnthropicResponseToolUse =
  case parseResponseBody (encode sampleToolUseResponse) of
    Left err -> pure $ Left $ "Failed to parse tool_use response: " ++ show err
    Right resp -> case crToolCalls resp of
      Just [tc]
        | msgContent (crReply resp) == "I will read it."
          && tcId tc == "toolu_2"
          && tcName tc == "read_file"
          && tcArgs tc == object ["path" .= ("README.md" :: T.Text)]
          -> pure $ Right ()
        | otherwise -> pure $ Left $ "Tool call mismatch: " ++ show tc
      Just tcs -> pure $ Left $ "Expected one tool call, got " ++ show (length tcs)
      Nothing -> pure $ Left "Expected tool call, got Nothing"

-- | parseSSELine extracts Anthropic JSON data payloads and ignores
--   other SSE fields.
testAnthropicParseSSELine :: Test
testAnthropicParseSSELine =
  case parseSSELine (BS8.pack "data: {\"type\":\"message_stop\"}") of
    Just payload
      | payload == BS8.pack "{\"type\":\"message_stop\"}"
          && parseSSELine (BS8.pack "event: content_block_delta") == Nothing ->
            pure $ Right ()
      | otherwise -> pure $ Left $ "Unexpected payload: " ++ show payload
    Nothing -> pure $ Left "Expected data payload"

-- | Anthropic text_delta events parse into text deltas.
testAnthropicParseTextDeltaEvent :: Test
testAnthropicParseTextDeltaEvent =
  let event = object
        [ "type"  .= ("content_block_delta" :: T.Text)
        , "index" .= (0 :: Int)
        , "delta" .= object
            [ "type" .= ("text_delta" :: T.Text)
            , "text" .= ("Hello" :: T.Text)
            ]
        ]
  in case parseStreamEvent (LBS.toStrict (encode event)) of
       Right (StreamTextDelta 0 "Hello") -> pure $ Right ()
       Right other -> pure $ Left $ "Unexpected event: " ++ show other
       Left err -> pure $ Left $ "Failed to parse text delta: " ++ show err

-- | Plain text streaming accumulates text deltas into a final assistant
--   response.
testAnthropicStreamTextAssembly :: Test
testAnthropicStreamTextAssembly =
  case assembleStreamEvents
        [ StreamTextDelta 0 "Hel"
        , StreamTextDelta 0 "lo"
        , StreamStopReason "end_turn"
        , StreamMessageStop
        ] of
    Left err -> pure $ Left $ "Failed to assemble text stream: " ++ show err
    Right resp
      | msgContent (crReply resp) == "Hello"
        && crToolCalls resp == Nothing -> pure $ Right ()
      | otherwise -> pure $ Left $ "Unexpected response: " ++ show resp

-- | Streamed tool_use events assemble into normal Haskode ToolCall
--   values.
testAnthropicStreamToolUseAssembly :: Test
testAnthropicStreamToolUseAssembly =
  case assembleStreamEvents
        [ StreamToolUseStart 0 "toolu_stream" "read_file"
        , StreamToolInputDelta 0 "{\"path\":\"README.md\"}"
        , StreamStopReason "tool_use"
        , StreamMessageStop
        ] of
    Left err -> pure $ Left $ "Failed to assemble tool stream: " ++ show err
    Right resp -> case crToolCalls resp of
      Just [tc]
        | tcId tc == "toolu_stream"
          && tcName tc == "read_file"
          && tcArgs tc == object ["path" .= ("README.md" :: T.Text)]
          -> pure $ Right ()
        | otherwise -> pure $ Left $ "Tool call mismatch: " ++ show tc
      other -> pure $ Left $ "Expected one tool call, got " ++ show other

-- | Fragmented input_json_delta chunks are concatenated before JSON
--   decoding.
testAnthropicStreamFragmentedToolInput :: Test
testAnthropicStreamFragmentedToolInput =
  case assembleStreamEvents
        [ StreamToolUseStart 0 "toolu_frag" "shell"
        , StreamToolInputDelta 0 "{\"command\":\"echo"
        , StreamToolInputDelta 0 " hello\"}"
        , StreamStopReason "tool_use"
        , StreamMessageStop
        ] of
    Left err -> pure $ Left $ "Failed to assemble fragmented input: " ++ show err
    Right resp -> case crToolCalls resp of
      Just [tc]
        | tcName tc == "shell"
          && tcArgs tc == object ["command" .= ("echo hello" :: T.Text)]
          -> pure $ Right ()
        | otherwise -> pure $ Left $ "Fragmented input mismatch: " ++ show tc
      other -> pure $ Left $ "Expected one tool call, got " ++ show other

-- | Multiple content blocks are ordered by Anthropic content-block index.
testAnthropicStreamMultipleContentBlocks :: Test
testAnthropicStreamMultipleContentBlocks =
  case assembleStreamEvents
        [ StreamTextDelta 0 "Before "
        , StreamToolUseStart 1 "toolu_multi" "list_files"
        , StreamToolInputDelta 1 "{\"directory\":\".\"}"
        , StreamTextDelta 2 "after."
        , StreamStopReason "tool_use"
        , StreamMessageStop
        ] of
    Left err -> pure $ Left $ "Failed to assemble multi-block stream: " ++ show err
    Right resp -> case crToolCalls resp of
      Just [tc]
        | msgContent (crReply resp) == "Before after."
          && tcId tc == "toolu_multi"
          && tcName tc == "list_files" -> pure $ Right ()
        | otherwise -> pure $ Left $ "Multi-block mismatch: " ++ show (resp, tc)
      other -> pure $ Left $ "Expected one tool call, got " ++ show other

-- | The final message_delta stop_reason is retained in the stream
--   accumulator.
testAnthropicStreamStopReason :: Test
testAnthropicStreamStopReason =
  case applyStreamEvent emptyAnthropicStreamState (StreamStopReason "tool_use") of
    Right st
      | assStopReason st == Just "tool_use" -> pure $ Right ()
      | otherwise -> pure $ Left $ "Unexpected stop reason: " ++ show (assStopReason st)
    Left err -> pure $ Left $ "Unexpected apply error: " ++ err

-- | Unsupported or malformed stream event shapes fail clearly.
testAnthropicStreamMalformedEvents :: Test
testAnthropicStreamMalformedEvents =
  let unsupported = object ["type" .= ("unknown_event" :: T.Text)]
      badDelta = object
        [ "type"  .= ("content_block_delta" :: T.Text)
        , "index" .= (0 :: Int)
        , "delta" .= object ["type" .= ("image_delta" :: T.Text)]
        ]
      malformedInput = assembleStreamEvents
        [ StreamToolUseStart 0 "toolu_bad" "read_file"
        , StreamToolInputDelta 0 "not json"
        , StreamMessageStop
        ]
      missingStop = assembleStreamEvents
        [ StreamTextDelta 0 "unfinished" ]
  in case ( parseStreamEvent (BS8.pack "{bad json")
          , parseStreamEvent (LBS.toStrict (encode unsupported))
          , parseStreamEvent (LBS.toStrict (encode badDelta))
          , malformedInput
          , missingStop
          ) of
       ( Left (AnthropicStreamError parseErr)
         , Left (AnthropicStreamError unsupportedErr)
         , Left (AnthropicStreamError deltaErr)
         , Left (AnthropicStreamError inputErr)
         , Left (AnthropicStreamError stopErr)
         )
         | "decode" `T.isInfixOf` T.pack parseErr
           && "unsupported" `T.isInfixOf` T.pack unsupportedErr
           && "unsupported" `T.isInfixOf` T.pack deltaErr
           && "malformed input JSON" `T.isInfixOf` T.pack inputErr
           && "message_stop" `T.isInfixOf` T.pack stopErr
           -> pure $ Right ()
       other -> pure $ Left $ "Expected clear stream errors, got " ++ show other

-- | malformed JSON returns AnthropicResponseParseError.
testAnthropicMalformedResponse :: Test
testAnthropicMalformedResponse =
  case parseResponseBody "{bad json" of
    Left (AnthropicResponseParseError _) -> pure $ Right ()
    Left other -> pure $ Left $ "Expected AnthropicResponseParseError, got " ++ show other
    Right _ -> pure $ Left "Expected parse error, got success"

-- | unsupported content block types fail clearly.
testAnthropicUnsupportedBlock :: Test
testAnthropicUnsupportedBlock =
  let resp = object
        [ "role" .= ("assistant" :: T.Text)
        , "content" .= [object ["type" .= ("image" :: T.Text)]]
        ]
  in case parseResponseBody (encode resp) of
       Left (AnthropicResponseParseError msg)
         | "unsupported" `T.isInfixOf` T.pack msg -> pure $ Right ()
         | otherwise -> pure $ Left $ "Error missing unsupported phrase: " ++ msg
       Left other -> pure $ Left $ "Expected AnthropicResponseParseError, got " ++ show other
       Right _ -> pure $ Left "Expected unsupported block error"

-- | anthropicProvider fails clearly when no Anthropic key is available.
--   The error message mentions all three key sources in precedence order.
testAnthropicProviderMissingKey :: Test
testAnthropicProviderMissingKey =
  withAnthropicApiKey Nothing $ do
    result <- anthropicProvider (anthropicConfig "") mempty
    case result of
      Left msg
        | not ("--api-key" `T.isInfixOf` T.pack msg) ->
            pure $ Left $ "Missing-key message should mention --api-key: " ++ msg
        | not ("ANTHROPIC_API_KEY" `T.isInfixOf` T.pack msg) ->
            pure $ Left $ "Missing-key message should mention ANTHROPIC_API_KEY: " ++ msg
        | not ("pcApiKey" `T.isInfixOf` T.pack msg) ->
            pure $ Left $ "Missing-key message should mention pcApiKey: " ++ msg
        | otherwise -> pure $ Right ()
      Right _ -> pure $ Left "Expected missing-key error, got provider"

-- | A config pcApiKey is enough to construct the provider, and the
--   provider exposes the Anthropic streaming path.
testAnthropicProviderConfigKeyStream :: Test
testAnthropicProviderConfigKeyStream =
  withAnthropicApiKey Nothing $ do
    result <- anthropicProvider (anthropicConfig "cfg-key") mempty
    case result of
      Left err -> pure $ Left $ "Expected provider, got error: " ++ err
      Right prov
        | providerName prov == "anthropic" && isJust (providerStream prov) ->
            pure $ Right ()
        | otherwise -> pure $ Left $ "Unexpected provider metadata: " ++ show (providerName prov)

-- | ANTHROPIC_API_KEY is the default key source when pcApiKey is empty.
testAnthropicProviderEnvKey :: Test
testAnthropicProviderEnvKey =
  withAnthropicApiKey (Just "env-key") $ do
    result <- anthropicProvider (anthropicConfig "") mempty
    case result of
      Left err -> pure $ Left $ "Expected env key provider, got error: " ++ err
      Right prov
        | providerName prov == "anthropic" -> pure $ Right ()
        | otherwise -> pure $ Left $ "Unexpected provider name: " ++ show (providerName prov)

tests :: [Test]
tests =
  [ testAnthropicRequestShape
  , testAnthropicRequestTools
  , testAnthropicStreamingRequestShape
  , testAnthropicAssistantToolUseJSON
  , testAnthropicToolResultJSON
  , testAnthropicSystemMessages
  , testAnthropicRejectsSystemOnlyRequest
  , testAnthropicRejectsUserToolUse
  , testAnthropicRejectsEmptyToolUse
  , testAnthropicApiKeyPrecedence
  , testAnthropicResponseText
  , testAnthropicResponseToolUse
  , testAnthropicParseSSELine
  , testAnthropicParseTextDeltaEvent
  , testAnthropicStreamTextAssembly
  , testAnthropicStreamToolUseAssembly
  , testAnthropicStreamFragmentedToolInput
  , testAnthropicStreamMultipleContentBlocks
  , testAnthropicStreamStopReason
  , testAnthropicStreamMalformedEvents
  , testAnthropicMalformedResponse
  , testAnthropicUnsupportedBlock
  , testAnthropicProviderMissingKey
  , testAnthropicProviderConfigKeyStream
  , testAnthropicProviderEnvKey
  ]
