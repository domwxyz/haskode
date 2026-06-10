{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | Anthropic Messages API provider.
--
-- This module implements Haskode's record-of-functions 'Provider'
-- interface for Anthropic's native @/v1/messages@ API.
--
-- = Scope
--
-- This is intentionally narrow:
--
--   * Non-streaming message completion is implemented.
--   * Streaming message completion is implemented through the same
--     optional provider hook used by OpenAI-compatible providers.
--   * Native Anthropic @tool_use@ and @tool_result@ content blocks are
--     mapped to Haskode's existing 'ToolCall' conversation shape.
--
-- No token counting, context-window management, resume/replay behavior,
-- typeclasses, or broad provider abstraction is added here.

module Haskode.Provider.Anthropic
  ( -- * Provider constructor
    anthropicProvider
    -- * Request building (exported for testing)
  , buildRequestBody
  , buildStreamingRequestBody
  , resolveAnthropicApiKey
  , validateMessages
  , messagesToJSON
  , messageToJSON
  , systemMessagesToText
  , toolsToJSON
    -- * Response parsing (exported for testing)
  , parseResponseBody
  , ParsedContentBlock (..)
  , parseContentBlock
    -- * SSE parsing and stream assembly (exported for testing)
  , parseSSELine
  , parseStreamEvent
  , AnthropicStreamEvent (..)
  , AnthropicStreamingToolUse (..)
  , AnthropicStreamState (..)
  , emptyAnthropicStreamState
  , applyStreamEvent
  , finalizeStreamState
  , assembleStreamEvents
    -- * Errors
  , AnthropicError (..)
  ) where

import Control.Exception        (Exception, throwIO)
import Control.Monad            (foldM, unless, when)
import Data.Aeson               (Value (..), (.=), object, encode,
                                 eitherDecode, decode)
import qualified Data.Aeson.Key    as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy  as LBS
import Data.IORef               (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text                (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector        as V
import Network.HTTP.Client      (BodyReader, Manager, RequestBody (..), httpLbs,
                                 method, newManager, parseRequest,
                                 requestBody, requestHeaders,
                                 responseBody, responseStatus, withResponse,
                                 brRead)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Network.HTTP.Types       (statusCode)
import System.Environment       (lookupEnv)

import Haskode.Config           (Config (..), ProviderConfig (..))
import Haskode.Core             (Message (..), Role (..), ToolCall (..))
import Haskode.Provider         (CompletionRequest (..),
                                 CompletionResponse (..), Provider (..),
                                 StreamHandler (..))
import Haskode.Tools            (Tool (..), ToolRegistry, lookupTool,
                                 toolNames)

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

-- | Errors specific to the Anthropic provider.
data AnthropicError
  = MissingAnthropicApiKey
    -- ^ Neither @--api-key@ / @pcApiKey@ nor @ANTHROPIC_API_KEY@
    --   contained a key.
  | AnthropicRequestError String
    -- ^ The conversation could not be represented as a valid
    --   Anthropic Messages request.
  | AnthropicHttpError Int LBS.ByteString
    -- ^ The API returned a non-2xx status code.
  | AnthropicResponseParseError String
    -- ^ The response body could not be decoded as the expected JSON.
  | AnthropicStreamError String
    -- ^ The streaming SSE response was malformed or could not be
    --   assembled into a final assistant message.
  deriving stock (Show, Eq)

instance Exception AnthropicError

-- ---------------------------------------------------------------------------
-- Provider constructor
-- ---------------------------------------------------------------------------

-- | Create an Anthropic provider from the app config.
--
-- API key resolution is provider-specific:
--
--   1. @--api-key@, applied by the CLI as @pcApiKey@.
--   2. @pcApiKey@ from config, when non-empty.
--   3. @ANTHROPIC_API_KEY@, when set and non-empty.
--
-- The base URL defaults to @https://api.anthropic.com@ when @pcBaseUrl@
-- is empty.  Otherwise @/v1/messages@ is appended to @pcBaseUrl@.
anthropicProvider :: Config -> ToolRegistry -> IO (Either String Provider)
anthropicProvider cfg reg = do
  mEnvKey <- lookupEnv "ANTHROPIC_API_KEY"
  let configKey = pcApiKey (cfgProvider cfg)
      mKey = resolveAnthropicApiKey configKey mEnvKey

  case mKey of
    Nothing -> pure $ Left
      "Anthropic API key not found. Set --api-key, pcApiKey in config, or ANTHROPIC_API_KEY."
    Just apiKey -> do
      mgr <- newManager tlsManagerSettings
      let baseRoot = case pcBaseUrl (cfgProvider cfg) of
            "" -> "https://api.anthropic.com"
            u  -> u
          url    = baseRoot ++ "/v1/messages"
          model' = T.pack (pcModel (cfgProvider cfg))
          maxTok = cfgMaxTokens cfg
      pure $ Right Provider
        { providerName = "anthropic"
        , providerComplete = \req -> do
            body <- either throwIO pure $
              buildRequestBody (crMessages req) model' maxTok reg
            respBody <- sendRequest mgr url (T.pack apiKey) body
            either throwIO pure (parseResponseBody respBody)
        , providerStream = Just $ \req handler -> do
            body <- either throwIO pure $
              buildStreamingRequestBody (crMessages req) model' maxTok reg
            sendRequestStreaming mgr url (T.pack apiKey) body handler
        }

-- | Resolve the Anthropic API key.
--
-- The CLI applies @--api-key@ by overwriting @pcApiKey@ before provider
-- construction, so a non-empty config key represents either the CLI
-- override or the config file value.  That value wins over the
-- provider-specific environment fallback.
resolveAnthropicApiKey :: String -> Maybe String -> Maybe String
resolveAnthropicApiKey configKey mEnvKey =
  case configKey of
    "" -> case mEnvKey of
            Just k | not (null k) -> Just k
            _                     -> Nothing
    k  -> Just k

-- ---------------------------------------------------------------------------
-- HTTP transport
-- ---------------------------------------------------------------------------

sendRequest :: Manager -> String -> Text -> LBS.ByteString -> IO LBS.ByteString
sendRequest mgr url apiKey body = do
  req <- parseRequest url
  let req' = req
        { method = BS8.pack "POST"
        , requestHeaders =
            [ ("Content-Type",       "application/json")
            , ("x-api-key",          TE.encodeUtf8 apiKey)
            , ("anthropic-version",  "2023-06-01")
            ]
        , requestBody = RequestBodyLBS body
        }
  resp <- httpLbs req' mgr
  let status = statusCode (responseStatus resp)
  if status >= 200 && status < 300
    then pure (responseBody resp)
    else throwIO $ AnthropicHttpError status (responseBody resp)

sendRequestStreaming :: Manager -> String -> Text -> LBS.ByteString
                     -> StreamHandler -> IO CompletionResponse
sendRequestStreaming mgr url apiKey body handler = do
  req <- parseRequest url
  let req' = req
        { method = BS8.pack "POST"
        , requestHeaders =
            [ ("Content-Type",       "application/json")
            , ("Accept",             "text/event-stream")
            , ("x-api-key",          TE.encodeUtf8 apiKey)
            , ("anthropic-version",  "2023-06-01")
            ]
        , requestBody = RequestBodyLBS body
        }
  withResponse req' mgr $ \resp -> do
    let status = statusCode (responseStatus resp)
    unless (status >= 200 && status < 300) $ do
      errBody <- brRead (responseBody resp)
      throwIO $ AnthropicHttpError status (LBS.fromStrict errBody)
    processSSEStream (responseBody resp) handler

processSSEStream :: BodyReader -> StreamHandler -> IO CompletionResponse
processSSEStream reader handler = do
  stateRef <- newIORef emptyAnthropicStreamState
  bufRef <- newIORef BS.empty
  doneRef <- newIORef False

  let loop = do
        done <- readIORef doneRef
        unless done $ do
          chunk <- brRead reader
          if BS.null chunk
            then do
              leftover <- readIORef bufRef
              unless (BS.null leftover) $
                processOneLine stateRef handler doneRef leftover
              writeIORef bufRef BS.empty
            else do
              buffered <- readIORef bufRef
              let (complete, remainder) = splitLines (buffered <> chunk)
              writeIORef bufRef remainder
              mapM_ (processOneLine stateRef handler doneRef) complete
              loop

  loop
  st <- readIORef stateRef
  unless (assMessageStopped st) $
    throwIO $ AnthropicStreamError "Anthropic stream ended before message_stop"
  either throwIO pure (finalizeStreamState st)

splitLines :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
splitLines bs = go bs []
  where
    go remaining acc =
      case BS.break (== fromIntegral (fromEnum '\n')) remaining of
        (line, rest)
          | BS.null rest -> (reverse acc, line)
          | otherwise    -> go (BS.drop 1 rest) (line : acc)

processOneLine :: IORef AnthropicStreamState -> StreamHandler
               -> IORef Bool -> BS.ByteString -> IO ()
processOneLine stateRef handler doneRef line =
  case parseSSELine line of
    Nothing -> pure ()
    Just payload -> do
      event <- either throwIO pure (parseStreamEvent payload)
      case event of
        StreamTextDelta _ text
          | T.null text -> pure ()
          | otherwise -> onToken handler text
        _ -> pure ()
      st <- readIORef stateRef
      st' <- case applyStreamEvent st event of
        Right val -> pure val
        Left err  -> throwIO $ AnthropicStreamError err
      writeIORef stateRef st'
      when (event == StreamMessageStop) $
        writeIORef doneRef True

-- ---------------------------------------------------------------------------
-- Request building
-- ---------------------------------------------------------------------------

-- | Build the full JSON request body for Anthropic's Messages API.
--
-- Haskode keeps system messages in the conversation.  Anthropic expects
-- them in a top-level @system@ field, so 'systemMessagesToText' extracts
-- and joins them while 'messagesToJSON' omits them from @messages@.
buildRequestBody :: [Message] -> Text -> Int -> ToolRegistry
                 -> Either AnthropicError LBS.ByteString
buildRequestBody msgs model' maxTok reg = do
  validateMessages msgs
  Right $ encode $ object $
      [ "model"      .= model'
      , "max_tokens" .= maxTok
      , "messages"   .= messagesToJSON msgs
      ] ++ systemField ++ toolsField
  where
    systemField = case systemMessagesToText msgs of
      ""  -> []
      sys -> [ "system" .= sys ]

    toolsField
      | null (toolNames reg) = []
      | otherwise            = [ "tools" .= toolsToJSON reg ]

-- | Build a streaming Anthropic request body.
--
-- This is the same Messages API shape as 'buildRequestBody' with
-- @"stream": true@ added.
buildStreamingRequestBody :: [Message] -> Text -> Int -> ToolRegistry
                          -> Either AnthropicError LBS.ByteString
buildStreamingRequestBody msgs model' maxTok reg = do
  validateMessages msgs
  Right $ encode $ object $
      [ "model"      .= model'
      , "max_tokens" .= maxTok
      , "messages"   .= messagesToJSON msgs
      , "stream"     .= True
      ] ++ systemField ++ toolsField
  where
    systemField = case systemMessagesToText msgs of
      ""  -> []
      sys -> [ "system" .= sys ]

    toolsField
      | null (toolNames reg) = []
      | otherwise            = [ "tools" .= toolsToJSON reg ]

-- | Validate Haskode messages before encoding an Anthropic request.
--
-- This catches impossible internal shapes early with a provider-specific
-- error instead of sending malformed Anthropic JSON.
validateMessages :: [Message] -> Either AnthropicError ()
validateMessages msgs
  | null nonSystem =
      Left $ AnthropicRequestError
        "Anthropic request must contain at least one non-system message"
  | otherwise = mapM_ validateMessage msgs
  where
    nonSystem = filter ((/= System) . msgRole) msgs

validateMessage :: Message -> Either AnthropicError ()
validateMessage m
  | msgRole m == System && msgCallId m /= Nothing =
      Left $ AnthropicRequestError
        "system messages cannot be tool_result messages"
  | msgRole m == System && msgToolCalls m /= Nothing =
      Left $ AnthropicRequestError
        "system messages cannot contain tool_use blocks"
  | msgCallId m /= Nothing && msgToolCalls m /= Nothing =
      Left $ AnthropicRequestError
        "a message cannot be both a tool_result and a tool_use request"
  | Just "" <- msgCallId m =
      Left $ AnthropicRequestError
        "tool_result messages must have a non-empty tool_use_id"
  | Just _ <- msgToolCalls m
  , msgRole m /= Assistant =
      Left $ AnthropicRequestError
        "only assistant messages can contain tool_use blocks"
  | Just [] <- msgToolCalls m =
      Left $ AnthropicRequestError
        "assistant tool_use messages must contain at least one tool call"
  | otherwise = Right ()

-- | Extract top-level Anthropic system text from Haskode system messages.
systemMessagesToText :: [Message] -> Text
systemMessagesToText msgs =
  T.intercalate "\n\n"
    [ msgContent m
    | m <- msgs
    , msgRole m == System
    , not (T.null (msgContent m))
    ]

-- | Convert non-system Haskode messages to Anthropic message JSON.
messagesToJSON :: [Message] -> [Value]
messagesToJSON =
  map messageToJSON . filter ((/= System) . msgRole)

-- | Convert a single non-system message to Anthropic wire format.
--
-- Tool-result messages become user messages with @tool_result@ content
-- blocks.  Assistant messages with tool calls become assistant messages
-- with @tool_use@ content blocks.  Plain messages use a text content
-- string, which is the compact Messages API form.
messageToJSON :: Message -> Value
messageToJSON m =
  case msgCallId m of
    Just callId -> object
      [ "role"    .= ("user" :: Text)
      , "content" .= [ object
          [ "type"        .= ("tool_result" :: Text)
          , "tool_use_id" .= callId
          , "content"     .= msgContent m
          ]
        ]
      ]
    Nothing ->
      case msgToolCalls m of
        Just tcs -> object
          [ "role"    .= roleToText (msgRole m)
          , "content" .= assistantContentBlocks (msgContent m) tcs
          ]
        Nothing -> object
          [ "role"    .= roleToText (msgRole m)
          , "content" .= msgContent m
          ]

assistantContentBlocks :: Text -> [ToolCall] -> [Value]
assistantContentBlocks content tcs =
  textBlock ++ map toolCallToContentBlock tcs
  where
    textBlock
      | T.null content = []
      | otherwise      =
          [ object
              [ "type" .= ("text" :: Text)
              , "text" .= content
              ]
          ]

toolCallToContentBlock :: ToolCall -> Value
toolCallToContentBlock tc = object
  [ "type"  .= ("tool_use" :: Text)
  , "id"    .= tcId tc
  , "name"  .= tcName tc
  , "input" .= tcArgs tc
  ]

roleToText :: Role -> Text
roleToText System    = "system"
roleToText User      = "user"
roleToText Assistant = "assistant"

-- | Convert a tool registry to Anthropic's @tools@ array.
toolsToJSON :: ToolRegistry -> [Value]
toolsToJSON reg =
  [ object
      [ "name"         .= toolName t
      , "description"  .= toolDescription t
      , "input_schema" .= toolSchema t
      ]
  | name' <- toolNames reg
  , Just t <- [lookupTool name' reg]
  ]

-- ---------------------------------------------------------------------------
-- Streaming event parsing and assembly
-- ---------------------------------------------------------------------------

-- | Parsed Anthropic Messages streaming events that affect Haskode's
-- final response.  Provider metadata events and pings become
-- 'StreamNoop'.
data AnthropicStreamEvent
  = StreamTextDelta !Int !Text
  | StreamToolUseStart !Int !Text !Text
  | StreamToolInputDelta !Int !Text
  | StreamStopReason !Text
  | StreamMessageStop
  | StreamNoop
  deriving stock (Show, Eq)

-- | Mutable-in-spirit state for a streamed Anthropic @tool_use@ block.
data AnthropicStreamingToolUse = AnthropicStreamingToolUse
  { astIndex :: !Int
  , astId    :: !(Maybe Text)
  , astName  :: !(Maybe Text)
  , astInput :: !Text
  } deriving stock (Show, Eq)

-- | Pure accumulator for an Anthropic streaming response.
data AnthropicStreamState = AnthropicStreamState
  { assTextBlocks     :: !(Map.Map Int Text)
  , assToolUses       :: !(Map.Map Int AnthropicStreamingToolUse)
  , assStopReason     :: !(Maybe Text)
  , assMessageStopped :: !Bool
  } deriving stock (Show, Eq)

emptyAnthropicStreamState :: AnthropicStreamState
emptyAnthropicStreamState = AnthropicStreamState
  { assTextBlocks     = Map.empty
  , assToolUses       = Map.empty
  , assStopReason     = Nothing
  , assMessageStopped = False
  }

-- | Parse a single SSE line and return the JSON payload from @data:@.
--
-- Non-data lines such as @event:@, comments, and empty lines are ignored.
parseSSELine :: BS.ByteString -> Maybe BS.ByteString
parseSSELine line =
  case BS.stripPrefix (BS8.pack "data:") stripped of
    Just payload -> Just (BS8.dropWhile (== ' ') payload)
    Nothing      -> Nothing
  where
    stripped = BS8.strip line

-- | Parse one Anthropic streaming event payload.
parseStreamEvent :: BS.ByteString -> Either AnthropicError AnthropicStreamEvent
parseStreamEvent raw =
  case eitherDecode (LBS.fromStrict raw) of
    Left err -> Left $ AnthropicStreamError
      ("Failed to decode Anthropic stream event: " ++ err)
    Right val -> parseStreamEventValue val

parseStreamEventValue :: Value -> Either AnthropicError AnthropicStreamEvent
parseStreamEventValue (Object root) = do
  typ <- requiredText "type" root
  case typ of
    "message_start"       -> Right StreamNoop
    "content_block_start" -> parseContentBlockStart root
    "content_block_delta" -> parseContentBlockDelta root
    "content_block_stop"  -> Right StreamNoop
    "message_delta"       -> parseMessageDelta root
    "message_stop"        -> Right StreamMessageStop
    "ping"                -> Right StreamNoop
    "error"               -> Left $ AnthropicStreamError (parseStreamError root)
    other                 -> Left $ AnthropicStreamError
      ("unsupported Anthropic stream event type: " ++ T.unpack other)
parseStreamEventValue _ =
  Left $ AnthropicStreamError "Anthropic stream event is not a JSON object"

parseContentBlockStart :: KM.KeyMap Value
                       -> Either AnthropicError AnthropicStreamEvent
parseContentBlockStart root = do
  idx <- requiredIndex root
  block <- requiredObject "content_block" root
  typ <- requiredText "type" block
  case typ of
    "text" -> Right StreamNoop
    "tool_use" -> do
      ident <- requiredText "id" block
      name <- requiredText "name" block
      Right $ StreamToolUseStart idx ident name
    other -> Left $ AnthropicStreamError
      ("unsupported Anthropic content_block_start type: " ++ T.unpack other)

parseContentBlockDelta :: KM.KeyMap Value
                       -> Either AnthropicError AnthropicStreamEvent
parseContentBlockDelta root = do
  idx <- requiredIndex root
  delta <- requiredObject "delta" root
  typ <- requiredText "type" delta
  case typ of
    "text_delta" -> StreamTextDelta idx <$> requiredText "text" delta
    "input_json_delta" ->
      StreamToolInputDelta idx <$> requiredText "partial_json" delta
    other -> Left $ AnthropicStreamError
      ("unsupported Anthropic content_block_delta type: " ++ T.unpack other)

parseMessageDelta :: KM.KeyMap Value
                  -> Either AnthropicError AnthropicStreamEvent
parseMessageDelta root = do
  delta <- requiredObject "delta" root
  case KM.lookup (Key.fromText "stop_reason") delta of
    Just (String reason) -> Right $ StreamStopReason reason
    Just Null            -> Right StreamNoop
    Nothing              -> Right StreamNoop
    _ -> Left $ AnthropicStreamError
      "message_delta.stop_reason must be a string or null"

parseStreamError :: KM.KeyMap Value -> String
parseStreamError root =
  case KM.lookup (Key.fromText "error") root of
    Just (Object errObj) ->
      let errType = case KM.lookup (Key.fromText "type") errObj of
            Just (String t) -> T.unpack t
            _               -> "error"
          errMsg = case KM.lookup (Key.fromText "message") errObj of
            Just (String t) -> T.unpack t
            _               -> "Anthropic stream error"
      in errType ++ ": " ++ errMsg
    _ -> "Anthropic stream error event missing error object"

requiredText :: Text -> KM.KeyMap Value -> Either AnthropicError Text
requiredText field obj =
  case KM.lookup (Key.fromText field) obj of
    Just (String t) -> Right t
    _ -> Left $ AnthropicStreamError
      ("Anthropic stream event missing string field '" ++ T.unpack field ++ "'")

requiredObject :: Text -> KM.KeyMap Value -> Either AnthropicError (KM.KeyMap Value)
requiredObject field obj =
  case KM.lookup (Key.fromText field) obj of
    Just (Object nested) -> Right nested
    _ -> Left $ AnthropicStreamError
      ("Anthropic stream event missing object field '" ++ T.unpack field ++ "'")

requiredIndex :: KM.KeyMap Value -> Either AnthropicError Int
requiredIndex obj =
  case KM.lookup (Key.fromText "index") obj of
    Just (Number n) -> Right (round n)
    _ -> Left $ AnthropicStreamError
      "Anthropic stream event missing numeric field 'index'"

-- | Apply one parsed stream event to the pure accumulator.
applyStreamEvent :: AnthropicStreamState -> AnthropicStreamEvent
                 -> Either String AnthropicStreamState
applyStreamEvent st event =
  case event of
    StreamTextDelta idx text ->
      Right st
        { assTextBlocks =
            Map.insert idx
              (case Map.lookup idx (assTextBlocks st) of
                 Nothing  -> text
                 Just old -> old <> text)
              (assTextBlocks st)
        }
    StreamToolUseStart idx ident name ->
      Right st
        { assToolUses =
            Map.insert idx
              (case Map.lookup idx (assToolUses st) of
                 Nothing -> AnthropicStreamingToolUse idx (Just ident) (Just name) ""
                 Just old -> old { astId = Just ident, astName = Just name })
              (assToolUses st)
        }
    StreamToolInputDelta idx fragment ->
      Right st
        { assToolUses =
            Map.insert idx
              (case Map.lookup idx (assToolUses st) of
                 Nothing -> AnthropicStreamingToolUse idx Nothing Nothing fragment
                 Just old -> old { astInput = astInput old <> fragment })
              (assToolUses st)
        }
    StreamStopReason reason ->
      Right st { assStopReason = Just reason }
    StreamMessageStop ->
      Right st { assMessageStopped = True }
    StreamNoop ->
      Right st

-- | Convert a finished stream accumulator into Haskode's normal
-- 'CompletionResponse'.
finalizeStreamState :: AnthropicStreamState
                    -> Either AnthropicError CompletionResponse
finalizeStreamState st
  | not (assMessageStopped st) =
      Left $ AnthropicStreamError "Anthropic stream missing message_stop"
  | otherwise = do
      calls <- assembleStreamToolUses (assToolUses st)
      let text = T.concat (Map.elems (assTextBlocks st))
      Right CompletionResponse
        { crReply     = Message Assistant text Nothing Nothing
        , crToolCalls = if null calls then Nothing else Just calls
        }

-- | Parse and assemble a complete list of stream events.
assembleStreamEvents :: [AnthropicStreamEvent]
                     -> Either AnthropicError CompletionResponse
assembleStreamEvents events = do
  st <- case foldM applyStreamEvent emptyAnthropicStreamState events of
    Left err  -> Left $ AnthropicStreamError err
    Right val -> Right val
  finalizeStreamState st

assembleStreamToolUses :: Map.Map Int AnthropicStreamingToolUse
                       -> Either AnthropicError [ToolCall]
assembleStreamToolUses toolUses =
  mapM assemble (Map.toAscList toolUses)
  where
    assemble (_idx, tu) = do
      ident <- case astId tu of
        Just i  -> Right i
        Nothing -> Left $ AnthropicStreamError
          ("streamed tool_use missing 'id' at index " ++ show (astIndex tu))
      name <- case astName tu of
        Just n  -> Right n
        Nothing -> Left $ AnthropicStreamError
          ("streamed tool_use missing 'name' at index " ++ show (astIndex tu))
      let inputText
            | T.null (astInput tu) = "{}"
            | otherwise            = astInput tu
      input <- case decode (LBS.fromStrict (TE.encodeUtf8 inputText)) of
        Just v  -> Right v
        Nothing -> Left $ AnthropicStreamError
          ("streamed tool_use has malformed input JSON at index "
            ++ show (astIndex tu) ++ ": " ++ T.unpack inputText)
      Right ToolCall
        { tcId   = ident
        , tcName = name
        , tcArgs = input
        }

-- ---------------------------------------------------------------------------
-- Response parsing
-- ---------------------------------------------------------------------------

-- | Parse a raw Anthropic Messages API response body.
parseResponseBody :: LBS.ByteString -> Either AnthropicError CompletionResponse
parseResponseBody raw =
  case eitherDecode raw of
    Left err -> Left $ AnthropicResponseParseError
      ("Failed to decode API response: " ++ err
       ++ "\nRaw body: " ++ take 500 (BS8.unpack (LBS.toStrict raw)))
    Right val -> parseAPIResponse val

parseAPIResponse :: Value -> Either AnthropicError CompletionResponse
parseAPIResponse (Object root) = do
  let roleText = case KM.lookup (Key.fromText "role") root of
                   Just (String r) -> r
                   _               -> "assistant"
  blocks <- case KM.lookup (Key.fromText "content") root of
              Just (Array arr) -> Right (V.toList arr)
              _ -> Left $ AnthropicResponseParseError
                     "missing or malformed 'content' array"
  parsed <- mapM parseContentBlock blocks
  let texts = [ t | ParsedText t <- parsed ]
      calls = [ tc | ParsedToolUse tc <- parsed ]
      reply = Message (textToRole roleText) (T.concat texts) Nothing Nothing
  pure CompletionResponse
    { crReply     = reply
    , crToolCalls = if null calls then Nothing else Just calls
    }
parseAPIResponse _ =
  Left $ AnthropicResponseParseError "response is not a JSON object"

data ParsedContentBlock
  = ParsedText Text
  | ParsedToolUse ToolCall
  deriving stock (Show, Eq)

-- | Parse an Anthropic response content block.
parseContentBlock :: Value -> Either AnthropicError ParsedContentBlock
parseContentBlock (Object block) =
  case KM.lookup (Key.fromText "type") block of
    Just (String "text") -> do
      text <- case KM.lookup (Key.fromText "text") block of
                Just (String t) -> Right t
                _ -> Left $ AnthropicResponseParseError
                       "text content block missing 'text'"
      Right (ParsedText text)
    Just (String "tool_use") -> do
      ident <- case KM.lookup (Key.fromText "id") block of
                 Just (String i) -> Right i
                 _ -> Left $ AnthropicResponseParseError
                        "tool_use content block missing 'id'"
      name <- case KM.lookup (Key.fromText "name") block of
                Just (String n) -> Right n
                _ -> Left $ AnthropicResponseParseError
                       "tool_use content block missing 'name'"
      input <- case KM.lookup (Key.fromText "input") block of
                 Just v  -> Right v
                 Nothing -> Left $ AnthropicResponseParseError
                              "tool_use content block missing 'input'"
      Right $ ParsedToolUse ToolCall
        { tcId   = ident
        , tcName = name
        , tcArgs = input
        }
    Just (String other) -> Left $ AnthropicResponseParseError
      ("unsupported Anthropic content block type: " ++ T.unpack other)
    _ -> Left $ AnthropicResponseParseError
      "content block missing 'type'"
parseContentBlock _ =
  Left $ AnthropicResponseParseError "content block is not an object"

textToRole :: Text -> Role
textToRole "user"      = User
textToRole "assistant" = Assistant
textToRole "system"    = System
textToRole _           = Assistant
