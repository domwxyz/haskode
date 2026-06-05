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
-- = Configuration
--
-- The API key is resolved in this order:
--
--   1. The @OPENAI_API_KEY@ environment variable (if set and non-empty)
--   2. The @pcApiKey@ field from 'ProviderConfig'
--
-- If neither is available, 'openaiProvider' returns @Left@ with a
-- helpful error message.
--
-- = Limitations (Phase 1)
--
--   * Non-streaming only — the entire response is collected before
--     returning.
--   * No token counting or context-window management.
--   * No Anthropic or Ollama-specific extensions.

module Haskode.Provider.OpenAI
  ( -- * Provider constructor
    openaiProvider
    -- * Request building (exported for testing)
  , buildRequestBody
  , messagesToJSON
  , messageToJSON
  , toolsToJSON
    -- * Response parsing (exported for testing)
  , parseResponseBody
  , parseToolCall
    -- * Errors
  , OpenAIError (..)
  ) where

import Control.Exception        (Exception, throwIO)
import Data.Aeson               (Value (..), (.=), object, encode,
                                 eitherDecode, decode)
import qualified Data.Aeson.Key    as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy  as LBS
import Data.IORef               (newIORef, readIORef)
import Data.Text                (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector        as V
import Network.HTTP.Client      (Manager, RequestBody (..),
                                 httpLbs, method, newManager,
                                 parseRequest, requestBody,
                                 requestHeaders, responseBody,
                                 responseStatus)
import Network.HTTP.Client.TLS  (tlsManagerSettings)
import Network.HTTP.Types       (statusCode)
import System.Environment       (lookupEnv)

import Haskode.Config           (Config (..), ProviderConfig (..),
                                 tokenLimitFieldName)
import Haskode.Core             (Message (..), Role (..), ToolCall (..))
import Haskode.Provider         (CompletionRequest (..),
                                 CompletionResponse (..), Provider (..))
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
    -- ^ Neither the @OPENAI_API_KEY@ env var nor the config's
    --   @pcApiKey@ field contained a key.
  | HttpError Int LBS.ByteString
    -- ^ The API returned a non-2xx status code.  Fields are
    --   @(statusCode, responseBody)@.
  | ResponseParseError String
    -- ^ The response body could not be decoded as the expected JSON.
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
openaiProvider cfg reg = do
  -- Resolve the API key: env var takes precedence over config file.
  -- This is resolved once at construction time so the provider fails
  -- fast if the key is missing, rather than on the first request.
  mEnvKey <- lookupEnv "OPENAI_API_KEY"
  let configKey = pcApiKey (cfgProvider cfg)
      mKey      = case mEnvKey of
                    Just k  | not (null k) -> Just k
                    _                      -> case configKey of
                                               "" -> Nothing
                                               k  -> Just k

  case mKey of
    Nothing -> pure $ Left
      "OpenAI API key not found. Set OPENAI_API_KEY or pcApiKey in config."
    Just apiKey -> do
      -- Create a TLS-capable HTTP manager.  This is reused across
      -- requests so TCP connections can be kept alive.
      mgr <- newManager tlsManagerSettings

      -- Build the base URL once.  The endpoint path is appended in
      -- each request.
      let baseUrl    = pcBaseUrl (cfgProvider cfg) ++ "/v1/chat/completions"
          model'     = T.pack (pcModel (cfgProvider cfg))
          maxTok     = cfgMaxTokens cfg
          tokenField = tokenLimitFieldName (cfgProvider cfg)

      -- Use an IORef to hold the latest ToolRegistry so the provider
      -- can include tool definitions in each request.  It is
      -- initialised with the registry passed at construction time.
      regRef <- newIORef reg

      pure $ Right Provider
        { providerName = "openai"
        , providerComplete = \req -> do
            reg' <- readIORef regRef
            let body = buildRequestBody tokenField (crMessages req) model' maxTok reg'
            respBody <- sendRequest mgr baseUrl (T.pack apiKey) body
            either throwIO pure (parseResponseBody respBody)
        }

-- ---------------------------------------------------------------------------
-- HTTP transport
-- ---------------------------------------------------------------------------

-- | Send the chat-completion request and return the raw response body.
--
--   This is the only function that touches the network.  It:
--
--   1. Parses the URL (cached from 'openaiProvider')
--   2. Sets the @Authorization: Bearer …@ header
--   3. POSTs the JSON body
--   4. Checks for HTTP errors (non-2xx status codes)
sendRequest :: Manager -> String -> Text -> LBS.ByteString -> IO LBS.ByteString
sendRequest mgr url apiKey body = do
  req <- parseRequest url
  let req' = req
        { method = BS8.pack "POST"
        , requestHeaders =
            [ ("Content-Type",  "application/json")
            , ("Authorization", TE.encodeUtf8 ("Bearer " <> apiKey))
            ]
        , requestBody = RequestBodyLBS body
        }
  resp <- httpLbs req' mgr
  let status = statusCode (responseStatus resp)
  if status >= 200 && status < 300
    then pure (responseBody resp)
    else throwIO $ HttpError status (responseBody resp)

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
--   > , "tools": [ ... ]               -- only when tools are registered
--   > }
--
--   The token-limit field name is passed explicitly so that callers
--   can choose between @"max_completion_tokens"@ (OpenAI) and
--   @"max_tokens"@ (local/proxy providers).  See
--   'tokenLimitFieldName'.
buildRequestBody :: Text -> [Message] -> Text -> Int -> ToolRegistry -> LBS.ByteString
buildRequestBody tokenField msgs model' maxTok reg =
  encode $ object $
    [ "model"      .= model'
    , "messages"   .= messagesToJSON msgs
    , Key.fromText tokenField .= maxTok
    ] ++ toolsField reg
  where
    -- The "tools" field is omitted entirely when the registry is
    -- empty.  Some providers reject requests with an empty tools
    -- array.  When tools are present we also set tool_choice to
    -- "auto" so the model knows it may call them.
    toolsField r
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
