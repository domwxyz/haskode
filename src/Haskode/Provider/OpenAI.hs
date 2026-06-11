{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings  #-}

-- | OpenAI-compatible chat-completions provider.
--
-- This module implements the @Provider@ interface for any LLM backend
-- that speaks the OpenAI @/v1/chat/completions@ wire format.  That
-- includes OpenAI itself, Ollama (with @\/v1@ proxy), vLLM, LiteLLM,
-- and many others.
--
-- = Architecture
--
-- The module is split into two layers:
--
--   * __Pure JSON conversion__ — functions like 'buildRequestBody',
--     'parseResponseBody', and 'messageToJSON' translate between
--     Haskode's internal types and the OpenAI JSON wire format.
--     These are exported so they can be tested with fixture JSON
--     without any network I\/O.
--
--   * __HTTP transport__ — 'openaiProvider' wires the pure
--     conversion into actual HTTP requests using @http-client-tls@.
--
-- = Streaming
--
-- The provider supports Server-Sent Events (SSE) streaming for
-- assistant replies.  When streaming is enabled, the provider sends
-- @"stream": true@ in the request body and parses the SSE response
-- incrementally, calling the 'StreamHandler' callback for each
-- content delta.
--
-- Streaming now supports both text content deltas and tool-call
-- deltas.  Text deltas are forwarded to the 'StreamHandler' callback
-- and accumulated.  Tool-call deltas (@choices[0].delta.tool_calls@)
-- are assembled by index: each streamed fragment contributes an
-- optional @id@, @function.name@, and @function.arguments@ chunk.
-- Arguments fragments are concatenated in order.  At @[DONE]@, the
-- assembled state is converted into @[ToolCall]@ and returned in
-- the final 'CompletionResponse'.
--
-- = Configuration
--
-- The API key is resolved in this order:
--
--   1. An explicit CLI override, when passed through by the caller
--   2. The @OPENAI_API_KEY@ environment variable (if set and non-empty)
--   3. The @pcApiKey@ field from 'ProviderConfig'
--
-- @ollama@ and @vllm@ do not require an API key by default.  Hosted
-- and proxy providers fail fast with a helpful error when no key is
-- available.
--
-- = Limitations
--
--   * No token counting in this provider adapter. The agent owns its
--     character-based context guard.
--   * No provider-specific APIs beyond the OpenAI-compatible chat
--     completions shape.

module Haskode.Provider.OpenAI
  ( -- * Provider constructor
    openaiProvider
  , openaiProviderWithApiKeyOverride
  , resolveOpenAICompatibleApiKey
  , openAICompatibleRequiresApiKey
    -- * Request building (exported for testing)
  , buildRequestBody
  , buildStreamingRequestBody
  , messagesToJSON
  , messageToJSON
  , toolsToJSON
    -- * Response parsing (exported for testing)
  , parseResponseBody
  , parseToolCall
    -- * SSE parsing (exported for testing)
  , parseSSELine
  , parseSSEEvent
  , parseDeltaContent
  , parseDeltaToolCalls
    -- * Stream tool-call assembly (exported for testing)
  , StreamingToolCall (..)
  , assembleStreamToolCalls
    -- * Errors
  , OpenAIError (..)
  ) where

import Control.Exception        (Exception, throwIO)
import Control.Monad            (unless)
import Data.Aeson               (Value (..), (.=), object, encode,
                                 eitherDecode, decode)
import qualified Data.Aeson.Key    as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy  as LBS
import Data.IORef               (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import qualified Data.Map.Strict  as Map
import Data.Text                (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector        as V
import Network.HTTP.Client      (BodyReader, Manager, RequestBody (..),
                                 httpLbs, method, newManager,
                                 parseRequest, requestBody,
                                 requestHeaders, responseBody,
                                 responseStatus, withResponse,
                                 brRead)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Network.HTTP.Types       (RequestHeaders, statusCode)
import System.Environment       (lookupEnv)

import Haskode.Config           (Config (..), ProviderConfig (..),
                                 tokenLimitFieldName)
import Haskode.Core             (Message (..), Role (..), ToolCall (..))
import Haskode.Provider         (CompletionRequest (..),
                                 CompletionResponse (..), Provider (..),
                                 StreamHandler (..), ToolMode (..))
import Haskode.Provider.Resolve (localOpenAICompatibleProviders)
import Haskode.Tools            (Tool (..), ToolRegistry, toolNames, lookupTool)

-- ---------------------------------------------------------------------------
-- Errors
-- ---------------------------------------------------------------------------

-- | Errors specific to the OpenAI provider.
--
--   These are thrown as IO exceptions via 'throwIO'.  Callers that
--   need to catch them can use @Control.Exception.try@.
data OpenAIError
  = MissingApiKey
    -- ^ A provider that requires an API key had no CLI override,
    --   @OPENAI_API_KEY@, or config @pcApiKey@ value.
  | HttpError Int LBS.ByteString
    -- ^ The API returned a non-2xx status code.  Fields are
    --   @(statusCode, responseBody)@.
  | ResponseParseError String
    -- ^ The response body could not be decoded as the expected JSON.
  | StreamError String
    -- ^ An error occurred during SSE streaming (network, parse, or
    --   unexpected content such as tool-call deltas).
  deriving stock (Show, Eq)

instance Exception OpenAIError

-- ---------------------------------------------------------------------------
-- Provider constructor
-- ---------------------------------------------------------------------------

-- | Create an OpenAI-compatible provider from the app config.
--
--   Returns @Left errorMessage@ if the API key cannot be resolved.
--   On success the provider is ready to use — it holds an internal
--   HTTP connection manager that is reused across requests.
--
--   The 'ToolRegistry' is stored inside the provider so that each
--   request includes the correct @tools@ array.  Pass
--   'Haskode.Tools.defaultRegistry' (or any custom registry) here.
--
-- === Example
--
-- @
--   eitherProv <- openaiProvider cfg defaultRegistry
--   case eitherProv of
--     Left  err  -> putStrLn err
--     Right prov -> -- use prov with the agent loop
-- @
openaiProvider :: Config -> ToolRegistry -> IO (Either String Provider)
openaiProvider = openaiProviderWithApiKeyOverride Nothing

-- | Create an OpenAI-compatible provider with an optional explicit API
-- key override from the CLI.  The override is separate from @pcApiKey@
-- so @--api-key@ can honestly outrank @OPENAI_API_KEY@.
openaiProviderWithApiKeyOverride :: Maybe String -> Config -> ToolRegistry -> IO (Either String Provider)
openaiProviderWithApiKeyOverride mCliKey cfg reg = do
  mEnvKey <- lookupEnv "OPENAI_API_KEY"
  let pc         = cfgProvider cfg
      provider   = pcProvider pc
      configKey  = pcApiKey pc
      keyResult  = resolveOpenAICompatibleApiKey provider mCliKey mEnvKey configKey

  case keyResult of
    Left err -> pure $ Left err
    Right apiKey -> do
      -- Create a TLS-capable HTTP manager.  This is reused across
      -- requests so TCP connections can be kept alive.
      mgr <- newManager tlsManagerSettings

      -- Build the base URL once.  The endpoint path is appended in
      -- each request.
      let baseUrl    = pcBaseUrl pc ++ "/v1/chat/completions"
          model'     = T.pack (pcModel pc)
          maxTok     = cfgMaxTokens cfg
          tokenField = tokenLimitFieldName pc

      -- Use an IORef to hold the latest ToolRegistry so the provider
      -- can include tool definitions in each request.  It is
      -- initialised with the registry passed at construction time.
      regRef <- newIORef reg

      pure $ Right Provider
        { providerName = T.pack provider
        , providerComplete = \req -> do
            reg' <- readIORef regRef
            let body = buildRequestBody tokenField (crMessages req) model' maxTok reg' (crToolMode req)
            respBody <- sendRequest mgr baseUrl (T.pack apiKey) body
            either throwIO pure (parseResponseBody respBody)
        , providerStream = Just $ \req handler -> do
            reg' <- readIORef regRef
            let body = buildStreamingRequestBody tokenField (crMessages req) model' maxTok reg' (crToolMode req)
            sendRequestStreaming mgr baseUrl (T.pack apiKey) body handler
        }

-- | Whether an OpenAI-compatible provider requires a key before startup.
--
-- Local development servers commonly run without auth; hosted services and
-- proxies should fail clearly rather than sending anonymous requests.
openAICompatibleRequiresApiKey :: String -> Bool
openAICompatibleRequiresApiKey provider =
  provider `notElem` localOpenAICompatibleProviders

-- | Resolve an OpenAI-compatible API key.
--
-- Precedence:
--
--   1. explicit CLI key
--   2. @OPENAI_API_KEY@
--   3. config @pcApiKey@
--
-- Providers that do not require auth return an empty key when no source is
-- available; the HTTP transport then omits the Authorization header.
resolveOpenAICompatibleApiKey :: String -> Maybe String -> Maybe String -> String
                              -> Either String String
resolveOpenAICompatibleApiKey provider mCliKey mEnvKey configKey =
  case firstNonEmpty [mCliKey, mEnvKey, Just configKey] of
    Just key -> Right key
    Nothing
      | openAICompatibleRequiresApiKey provider ->
          Left $ provider ++ " API key not found. Set OPENAI_API_KEY, pcApiKey in config, or --api-key."
      | otherwise -> Right ""
  where
    firstNonEmpty :: [Maybe String] -> Maybe String
    firstNonEmpty [] = Nothing
    firstNonEmpty (Nothing : rest) = firstNonEmpty rest
    firstNonEmpty (Just "" : rest) = firstNonEmpty rest
    firstNonEmpty (Just key : _) = Just key

-- ---------------------------------------------------------------------------
-- HTTP transport
-- ---------------------------------------------------------------------------

-- | Send the chat-completion request and return the raw response body.
--
--   This is the only function that touches the network.  It:
--
--   1. Parses the URL (cached from 'openaiProvider')
--   2. Sets the @Authorization: Bearer …@ header when an API key exists
--   3. POSTs the JSON body
--   4. Checks for HTTP errors (non-2xx status codes)
sendRequest :: Manager -> String -> Text -> LBS.ByteString -> IO LBS.ByteString
sendRequest mgr url apiKey body = do
  req <- parseRequest url
  let req' = req
        { method = BS8.pack "POST"
        , requestHeaders =
            authHeaders apiKey
        , requestBody = RequestBodyLBS body
        }
  resp <- httpLbs req' mgr
  let status = statusCode (responseStatus resp)
  if status >= 200 && status < 300
    then pure (responseBody resp)
    else throwIO $ HttpError status (responseBody resp)

-- | Send a streaming chat-completion request.
--
--   Uses 'withResponse' to stream the HTTP response body as chunks.
--   Parses Server-Sent Events (SSE) lines, extracts content deltas,
--   calls the 'StreamHandler' for each delta, and returns the final
--   assembled 'CompletionResponse'.
--
--   Throws 'StreamError' if the HTTP status is non-2xx, if SSE
--   parsing fails, or if streamed tool-call assembly fails (e.g.
--   missing id or malformed arguments).
sendRequestStreaming :: Manager -> String -> Text -> LBS.ByteString
                     -> StreamHandler -> IO CompletionResponse
sendRequestStreaming mgr url apiKey body handler = do
  req <- parseRequest url
  let req' = req
        { method = BS8.pack "POST"
        , requestHeaders =
            authHeaders apiKey
        , requestBody = RequestBodyLBS body
        }
  withResponse req' mgr $ \resp -> do
    let status = statusCode (responseStatus resp)
    unless (status >= 200 && status < 300) $ do
      -- Read the full error body for the error message
      errBody <- brRead (responseBody resp)
      throwIO $ HttpError status (LBS.fromStrict errBody)
    processSSEStream (responseBody resp) handler

authHeaders :: Text -> RequestHeaders
authHeaders apiKey =
  [ ("Content-Type", "application/json") ]
  ++ if T.null apiKey
       then []
       else [("Authorization", TE.encodeUtf8 ("Bearer " <> apiKey))]

-- | Process an SSE stream from the response body.
--
--   Reads chunks, buffers incomplete lines, and processes complete
--   SSE events.  Returns the final assembled 'CompletionResponse'.
processSSEStream :: BodyReader -> StreamHandler -> IO CompletionResponse
processSSEStream resp handler = do
  accRef <- newIORef ""        -- accumulated content deltas
  bufRef <- newIORef BS.empty  -- buffered incomplete bytes
  doneRef <- newIORef False    -- whether we've seen [DONE]
  tcRef <- newIORef Map.empty  -- accumulated tool-call state (Map Int StreamingToolCall)

  let loop = do
        done <- readIORef doneRef
        if done
          then pure ()
          else do
            chunk <- brRead resp
            if BS.null chunk
              then do
                -- Stream ended without [DONE]; process any remaining buffer
                buf <- readIORef bufRef
                unless (BS.null buf) $
                  processLineChunk buf accRef handler doneRef bufRef tcRef
              else do
                buf <- readIORef bufRef
                let combined = buf <> chunk
                processLineChunk combined accRef handler doneRef bufRef tcRef
                loop

  loop

  -- Build the final CompletionResponse from accumulated content and tool calls
  acc <- readIORef accRef
  stcs <- readIORef tcRef
  case assembleStreamToolCalls stcs of
    Left err -> throwIO $ StreamError err
    Right [] ->
      pure CompletionResponse
        { crReply     = Message Assistant acc Nothing Nothing
        , crToolCalls = Nothing
        }
    Right tcs ->
      pure CompletionResponse
        { crReply     = Message Assistant acc Nothing Nothing
        , crToolCalls = Just tcs
        }

-- | Process a chunk of bytes that may contain multiple SSE lines.
--
--   Splits on newlines, processes each complete line, and buffers
--   any trailing incomplete bytes.
processLineChunk :: BS.ByteString -> IORef Text -> StreamHandler
                 -> IORef Bool -> IORef BS.ByteString
                 -> IORef (Map.Map Int StreamingToolCall) -> IO ()
processLineChunk chunk accRef handler doneRef bufRef tcRef = do
  let (lines', remainder) = splitLines chunk
  writeIORef bufRef remainder
  mapM_ (processOneLine accRef handler doneRef tcRef) lines'

-- | Split a strict ByteString on newline boundaries.
--
--   Returns @(completeLines, trailingBytes)@.  The trailing bytes
--   do not contain a newline and should be buffered for the next
--   chunk.
splitLines :: BS.ByteString -> ([BS.ByteString], BS.ByteString)
splitLines bs = go bs []
  where
    go acc rest =
      case BS.break (== fromIntegral (fromEnum '\n')) acc of
        (line, rest')
          | BS.null rest' -> (reverse rest, line)  -- no newline found
          | otherwise     -- found newline; skip it
            -> go (BS.drop 1 rest') (line : rest)

-- | Process a single complete SSE line.
--
--   Updates the content accumulator, tool-call state, and done flag
--   as needed.
processOneLine :: IORef Text -> StreamHandler -> IORef Bool
               -> IORef (Map.Map Int StreamingToolCall) -> BS.ByteString -> IO ()
processOneLine accRef handler doneRef tcRef line = do
  case parseSSELine line of
    Nothing -> pure ()  -- not a data line, skip
    Just Nothing -> do
      -- [DONE] signal
      writeIORef doneRef True
    Just (Just content) -> do
      case parseSSEEvent content of
        Left _err -> pure ()  -- skip unparseable events
        Right Nothing -> pure ()  -- no content delta
        Right (Just delta) -> do
          -- Call the handler and accumulate text content
          onToken handler delta
          modifyIORef' accRef (<> delta)
      -- Also check for tool-call deltas (may coexist with content)
      case eitherDecode (LBS.fromStrict content) of
        Left _ -> pure ()  -- skip unparseable
        Right val -> case parseDeltaToolCalls val of
          Nothing -> pure ()  -- no tool-call deltas
          Just fragments -> do
            -- Merge each fragment into the accumulated tool-call state
            mapM_ (mergeToolCallFragment tcRef) fragments

-- | Merge a single tool-call fragment into the accumulated state.
--
--   If the index already exists, updates the existing entry:
--   * @id@ is set if the fragment provides it (first fragment)
--   * @function.name@ is set if the fragment provides it
--   * @function.arguments@ text is appended
--
--   If the index is new, creates a new 'StreamingToolCall'.
mergeToolCallFragment :: IORef (Map.Map Int StreamingToolCall)
                      -> (Int, StreamingToolCall) -> IO ()
mergeToolCallFragment tcRef (idx, frag) = do
  stcs <- readIORef tcRef
  let updated = case Map.lookup idx stcs of
        Nothing ->
          -- New tool call
          Map.insert idx frag stcs
        Just existing ->
          -- Merge into existing
          let merged = StreamingToolCall
                { stcIndex = idx
                , stcId    = stcId frag `mplus'` stcId existing
                , stcName  = stcName frag `mplus'` stcName existing
                , stcArgs  = stcArgs existing <> stcArgs frag
                }
          in Map.insert idx merged stcs
  writeIORef tcRef updated
  where
    -- Left-biased merge (prefer first non-Nothing value)
    mplus' :: Maybe a -> Maybe a -> Maybe a
    mplus' (Just x) _ = Just x
    mplus' Nothing  y = y

-- ---------------------------------------------------------------------------
-- Request building (pure, testable)
-- ---------------------------------------------------------------------------

-- | Build the full JSON request body for the OpenAI API.
--
--   The shape matches the OpenAI spec:
--
--   > { "model": "gpt-4o"
--   > , "messages": [ ... ]
--   > , "max_completion_tokens": 4096   -- or "max_tokens" for local providers
--   > , "tools": [ ... ]               -- only when tools are registered and mode is AdvertiseTools
--   > }
--
--   The token-limit field name is passed explicitly so that callers
--   can choose between @"max_completion_tokens"@ (OpenAI) and
--   @"max_tokens"@ (local/proxy providers).  See
--   'tokenLimitFieldName'.
buildRequestBody :: Text -> [Message] -> Text -> Int -> ToolRegistry -> ToolMode -> LBS.ByteString
buildRequestBody tokenField msgs model' maxTok reg mode =
  encode $ object $
    [ "model"      .= model'
    , "messages"   .= messagesToJSON msgs
    , Key.fromText tokenField .= maxTok
    ] ++ toolsField reg mode
  where
    -- The "tools" field is omitted entirely when the registry is
    -- empty or when tool mode is NoTools.  Some providers reject
    -- requests with an empty tools array.  When tools are present
    -- and mode is AdvertiseTools we also set tool_choice to "auto"
    -- so the model knows it may call them.
    toolsField _ NoTools        = []
    toolsField r AdvertiseTools
      | null (toolNames r) = []
      | otherwise          = [ "tools"       .= toolsToJSON r
                             , "tool_choice" .= ("auto" :: Text)
                             ]

-- | Build a streaming JSON request body.
--
--   Identical to 'buildRequestBody' but adds @"stream": true@ so
--   the provider returns Server-Sent Events instead of a single
--   JSON response.
buildStreamingRequestBody :: Text -> [Message] -> Text -> Int -> ToolRegistry -> ToolMode -> LBS.ByteString
buildStreamingRequestBody tokenField msgs model' maxTok reg mode =
  encode $ object $
    [ "model"      .= model'
    , "messages"   .= messagesToJSON msgs
    , Key.fromText tokenField .= maxTok
    , "stream"     .= True
    ] ++ toolsField reg mode
  where
    toolsField _ NoTools        = []
    toolsField r AdvertiseTools
      | null (toolNames r) = []
      | otherwise          = [ "tools"       .= toolsToJSON r
                             , "tool_choice" .= ("auto" :: Text)
                             ]

-- | Convert a list of Haskode messages to the OpenAI JSON format.
--
--   Each message becomes @{"role": "...", "content": "..."}@ with
--   an optional @tool_call_id@ for tool-result messages.
messagesToJSON :: [Message] -> [Value]
messagesToJSON = map messageToJSON

-- | Convert a single message to the OpenAI wire format.
--
--   Tool-result messages (those with a 'msgCallId') are serialized
--   with role @"tool"@ so the API can match them to the original
--   tool call.  Assistant messages that contain 'msgToolCalls' are
--   serialized with the @"tool_calls"@ array so the API can match
--   subsequent @"tool"@ results.  All other messages use their
--   natural role.
messageToJSON :: Message -> Value
messageToJSON m = object $
  [ "role"    .= wireRole
  , "content" .= msgContent m
  ] <> maybe [] (\cid -> ["tool_call_id" .= cid]) (msgCallId m)
    <> maybe [] (\tcs -> ["tool_calls"   .= map toolCallToJSON tcs]) (msgToolCalls m)
  where
    wireRole = case msgCallId m of
      Just _  -> ("tool" :: Text)
      Nothing -> roleToText (msgRole m)

-- | Serialize a 'ToolCall' into the OpenAI wire format for embedding
--   inside an assistant message's @tool_calls@ array.
--
--   > { "id": "call_abc123"
--   > , "type": "function"
--   > , "function":
--   >     { "name": "read_file"
--   >     , "arguments": "{\"path\": \"foo.hs\"}"
--   >     }
--   > }
toolCallToJSON :: ToolCall -> Value
toolCallToJSON tc = object
  [ "id"       .= tcId tc
  , "type"     .= ("function" :: Text)
  , "function" .= object
      [ "name"      .= tcName tc
      , "arguments" .= encodeArgs (tcArgs tc)
      ]
  ]
  where
    -- OpenAI expects arguments as a JSON *string*, not a JSON object.
    encodeArgs :: Value -> Text
    encodeArgs = TE.decodeUtf8 . LBS.toStrict . encode

-- | Map our 'Role' to the lowercase text the OpenAI API expects.
roleToText :: Role -> Text
roleToText System    = "system"
roleToText User      = "user"
roleToText Assistant = "assistant"

-- | Convert a tool registry to the OpenAI @tools@ JSON array.
--
--   Each tool becomes:
--
--   > { "type": "function"
--   > , "function":
--   >     { "name": "read_file"
--   >     , "description": "Read the contents of a file"
--   >     , "parameters": { ... }  -- the tool's JSON schema
--   >     }
--   > }
toolsToJSON :: ToolRegistry -> [Value]
toolsToJSON reg =
  [ object
      [ "type"     .= ("function" :: Text)
      , "function" .= object
          [ "name"        .= toolName t
          , "description" .= toolDescription t
          , "parameters"  .= toolSchema t
          ]
      ]
  | name' <- toolNames reg
  , Just t <- [lookupTool name' reg]
  ]

-- ---------------------------------------------------------------------------
-- SSE parsing (pure, testable)
-- ---------------------------------------------------------------------------

-- | Mutable state for a single tool call being assembled from
--   streamed deltas.  Fields are updated incrementally as fragments
--   arrive.
data StreamingToolCall = StreamingToolCall
  { stcIndex     :: !Int    -- ^ The tool-call index from the delta
  , stcId        :: !(Maybe Text)  -- ^ Set when the first fragment with id arrives
  , stcName      :: !(Maybe Text)  -- ^ Set when function.name arrives
  , stcArgs      :: !Text          -- ^ Accumulated argument fragments
  } deriving stock (Show, Eq)

-- | Parse a single SSE line.
--
--   Returns:
--
--   * @Nothing@ — the line is not a data line (empty, comment, or
--     other SSE field like @event:@ or @id:@).
--   * @Just Nothing@ — the line is @data: [DONE]@, signalling end
--     of stream.
--   * @Just (Just content)@ — the line is @data: {...}@ containing
--     a JSON payload.  The content is the strict 'BS.ByteString'
--     after the @data: @ prefix.
parseSSELine :: BS.ByteString -> Maybe (Maybe BS.ByteString)
parseSSELine line
  | BS.null stripped = Nothing  -- empty line
  | otherwise =
      case BS.stripPrefix (BS8.pack "data: ") stripped of
        Just payload
          | payload == BS8.pack "[DONE]" -> Just Nothing
          | otherwise                    -> Just (Just payload)
        Nothing -> Nothing  -- not a data line
  where
    stripped = BS8.strip line

-- | Parse an SSE event JSON payload and extract the content delta.
--
--   The OpenAI streaming response shape is:
--
--   > { "choices":
--   >     [ { "delta":
--   >           { "content": "Some text" }
--   >       }
--   >     ]
--   > }
--
--   Returns:
--
--   * @Right Nothing@ — the event has no content delta (e.g. role
--     delta, empty delta, or tool-call delta).
--   * @Right (Just text)@ — the event contains a text content delta.
--   * @Left err@ — the JSON could not be parsed.
parseSSEEvent :: BS.ByteString -> Either String (Maybe Text)
parseSSEEvent raw =
  case eitherDecode (LBS.fromStrict raw) of
    Left err -> Left $ "SSE JSON parse error: " ++ err
    Right val -> Right (parseDeltaContent val)

-- | Extract content text from a streaming delta JSON value.
--
--   Looks for @choices[0].delta.content@.  Returns 'Nothing' if:
--
--   * The @choices@ array is missing or empty.
--   * The @delta@ object is missing.
--   * The @content@ field is missing or null (e.g. role-only deltas).
--   * Tool-call deltas are present (handled separately by 'parseDeltaToolCalls').
parseDeltaContent :: Value -> Maybe Text
parseDeltaContent (Object root) =
  case KM.lookup (Key.fromText "choices") root of
    Just (Array choices) | not (null choices) ->
      case choices V.! 0 of
        Object choiceObj ->
          case KM.lookup (Key.fromText "delta") choiceObj of
            Just (Object deltaObj) ->
              case KM.lookup (Key.fromText "content") deltaObj of
                Just (String t) | not (T.null t) -> Just t
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
parseDeltaContent _ = Nothing

-- | Extract tool-call deltas from a streaming event JSON value.
--
--   Looks for @choices[0].delta.tool_calls@, which is an array of
--   partial tool-call objects.  Each element may contain:
--
--   * @index@ (required) — identifies which tool call this fragment
--     belongs to.
--   * @id@ (optional) — the tool-call identifier, sent once.
--   * @function.name@ (optional) — the function name, sent once.
--   * @function.arguments@ (optional) — a chunk of the arguments
--     JSON string, may arrive across multiple fragments.
--
--   Returns a list of @(index, StreamingToolCall)@ pairs for each
--   fragment found, or @Nothing@ if no tool-call deltas are present.
parseDeltaToolCalls :: Value -> Maybe [(Int, StreamingToolCall)]
parseDeltaToolCalls (Object root) =
  case KM.lookup (Key.fromText "choices") root of
    Just (Array choices) | not (null choices) ->
      case choices V.! 0 of
        Object choiceObj ->
          case KM.lookup (Key.fromText "delta") choiceObj of
            Just (Object deltaObj) ->
              case KM.lookup (Key.fromText "tool_calls") deltaObj of
                Just (Array tcs) -> Just (concatMap extractTCFragment (V.toList tcs))
                _ -> Nothing
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing
  where
    extractTCFragment :: Value -> [(Int, StreamingToolCall)]
    extractTCFragment (Object tcObj) =
      case KM.lookup (Key.fromText "index") tcObj of
        Just (Number idx) ->
          let i = round idx
              mId = case KM.lookup (Key.fromText "id") tcObj of
                      Just (String s) -> Just s
                      _               -> Nothing
              mName = case KM.lookup (Key.fromText "function") tcObj of
                        Just (Object fn) -> case KM.lookup (Key.fromText "name") fn of
                                              Just (String n) -> Just n
                                              _               -> Nothing
                        _                -> Nothing
              mArgs = case KM.lookup (Key.fromText "function") tcObj of
                        Just (Object fn) -> case KM.lookup (Key.fromText "arguments") fn of
                                              Just (String a) -> a
                                              _               -> ""
                        _                -> ""
          in [(i, StreamingToolCall i mId mName mArgs)]
        _ -> []
    extractTCFragment _ = []
parseDeltaToolCalls _ = Nothing

-- | Assemble accumulated 'StreamingToolCall' state into final
--   'ToolCall' values.
--
--   For each entry in the map (sorted by index):
--
--   * If @id@ or @function.name@ is missing, raises 'StreamError'.
--   * The concatenated @function.arguments@ text is parsed as JSON.
--   * If JSON parsing fails, raises 'StreamError'.
--
--   Returns the assembled tool calls in index order.
assembleStreamToolCalls :: Map.Map Int StreamingToolCall -> Either String [ToolCall]
assembleStreamToolCalls stcs =
  mapM assemble (Map.toAscList stcs)
  where
    assemble (_idx, stc) = do
      tcId' <- case stcId stc of
                 Just i  -> Right i
                 Nothing -> Left $ "Streamed tool call missing 'id' at index "
                                   ++ show (stcIndex stc)
      name <- case stcName stc of
                Just n  -> Right n
                Nothing -> Left $ "Streamed tool call missing 'function.name' at index "
                                  ++ show (stcIndex stc)
      let argsText = stcArgs stc
      args <- case decode (LBS.fromStrict (TE.encodeUtf8 argsText)) of
                Just v  -> Right v
                Nothing -> Left $ "Streamed tool call has malformed arguments JSON at index "
                                  ++ show (stcIndex stc) ++ ": " ++ T.unpack argsText
      Right ToolCall
        { tcId   = tcId'
        , tcName = name
        , tcArgs = args
        }

-- ---------------------------------------------------------------------------
-- Response parsing (pure, testable)
-- ---------------------------------------------------------------------------

-- | Parse a raw API response body into our 'CompletionResponse'.
--
--   This handles:
--
--   * A plain text reply (no tool calls)
--   * A reply with one or more tool calls
--   * Error responses from the API
--   * Malformed JSON (returns 'ResponseParseError')
--
--   Returns @Left 'OpenAIError'@ on failure so the caller can
--   decide whether to throw or recover.
parseResponseBody :: LBS.ByteString -> Either OpenAIError CompletionResponse
parseResponseBody raw =
  case eitherDecode raw of
    Left err -> Left $ ResponseParseError
      ("Failed to decode API response: " ++ err
       ++ "\nRaw body: " ++ take 500 (BS8.unpack (LBS.toStrict raw)))
    Right val -> parseAPIResponse val

-- | Internal: parse the top-level JSON object.
--
--   The OpenAI response shape is:
--
--   > { "choices":
--   >     [ { "message":
--   >           { "role": "assistant"
--   >           , "content": "Some text"     -- may be null
--   >           , "tool_calls": [ ... ]      -- optional
--   >           }
--   >       }
--   >     ]
--   > }
parseAPIResponse :: Value -> Either OpenAIError CompletionResponse
parseAPIResponse (Object root) =
  case KM.lookup (Key.fromText "choices") root of
    Just (Array choices) | not (null choices) ->
      -- We only look at the first choice (index 0).
      case choices V.! 0 of
        Object choiceObj ->
          case KM.lookup (Key.fromText "message") choiceObj of
            Just (Object msgObj) -> parseChoiceMessage msgObj
            _ -> Left $ ResponseParseError "missing 'message' in choice"
        _ -> Left $ ResponseParseError "choice is not an object"
    _ -> Left $ ResponseParseError "missing or empty 'choices' array"
parseAPIResponse _ = Left $ ResponseParseError "response is not a JSON object"

-- | Parse the @message@ object inside a choice.
--
--   We handle three cases:
--
--   1. __Text only__ — @content@ is a string, no @tool_calls@.
--   2. __Tool calls__ — @tool_calls@ is present.  @content@ may be
--      @null@ or a string.
--   3. __Empty content__ — @content@ is @null@ and no tool calls.
--      We treat this as an empty text reply.
parseChoiceMessage :: KM.KeyMap Value -> Either OpenAIError CompletionResponse
parseChoiceMessage msgObj = do
  -- Extract role (defaults to "assistant" if missing).
  let roleText = case KM.lookup (Key.fromText "role") msgObj of
                   Just (String r) -> r
                   _               -> "assistant"

  -- Extract content.  OpenAI returns null when the model only
  -- issues tool calls, so we default to empty text.
  let content = case KM.lookup (Key.fromText "content") msgObj of
                  Just (String c) -> c
                  _               -> ""

  -- Extract tool calls (optional).
  let mToolCalls = case KM.lookup (Key.fromText "tool_calls") msgObj of
                     Just (Array arr) -> Just (V.toList arr)
                     _                -> Nothing

  case mToolCalls of
    Nothing -> do
      -- Plain text reply.
      let role = textToRole roleText
      pure CompletionResponse
        { crReply     = Message role content Nothing Nothing
        , crToolCalls = Nothing
        }
    Just tcValues -> do
      -- Parse each tool call JSON into our ToolCall type.
      toolCalls <- mapM parseToolCall tcValues
      let role = textToRole roleText
      pure CompletionResponse
        { crReply     = Message role content Nothing Nothing
        , crToolCalls = Just toolCalls
        }

-- | Parse a single tool-call JSON object.
--
--   The OpenAI shape is:
--
--   > { "id": "call_abc123"
--   > , "type": "function"
--   > , "function":
--   >     { "name": "read_file"
--   >     , "arguments": "{\"path\": \"foo.hs\"}"
--   >     }
--   > }
--
--   Note that @function.arguments@ is a JSON *string* that itself
--   contains JSON.  We parse it into a 'Value' so the tool executor
--   can work with it directly.
parseToolCall :: Value -> Either OpenAIError ToolCall
parseToolCall (Object tcObj) = do
  tcId' <- case KM.lookup (Key.fromText "id") tcObj of
             Just (String i) -> Right i
             _ -> Left $ ResponseParseError "tool_call missing 'id'"

  funcObj <- case KM.lookup (Key.fromText "function") tcObj of
               Just (Object f) -> Right f
               _ -> Left $ ResponseParseError "tool_call missing 'function'"

  name <- case KM.lookup (Key.fromText "name") funcObj of
            Just (String n) -> Right n
            _ -> Left $ ResponseParseError "tool_call.function missing 'name'"

  argsText <- case KM.lookup (Key.fromText "arguments") funcObj of
                Just (String a) -> Right a
                _ -> Left $ ResponseParseError "tool_call.function missing 'arguments'"

  -- The arguments field is a JSON string containing JSON.
  -- We need to parse it into an actual Value.
  args <- case decode (LBS.fromStrict (TE.encodeUtf8 argsText)) of
            Just v  -> Right v
            Nothing -> Left $ ResponseParseError $
              "Failed to parse tool_call arguments as JSON: "
              ++ T.unpack argsText

  Right ToolCall
    { tcId   = tcId'
    , tcName = name
    , tcArgs = args
    }
parseToolCall _ = Left $ ResponseParseError "tool_call is not a JSON object"

-- | Map the OpenAI role text back to our 'Role' type.
--
--   The API may return @"tool"@ for tool-result messages, but we
--   don't have a separate constructor for that.  We map it to
--   'User' since tool results are user-side content in our model.
textToRole :: Text -> Role
textToRole "system"    = System
textToRole "user"      = User
textToRole "assistant" = Assistant
textToRole "tool"      = User     -- tool results map to User in our model
textToRole _           = Assistant -- safe default
