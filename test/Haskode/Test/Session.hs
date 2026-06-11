{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

-- | Session event, persistence, rotation, summary, and lifecycle tests.
module Haskode.Test.Session (tests) where

import Control.Exception ( try, IOException, throwIO )
import Data.Aeson
    ( Value(Object, String), object, encode, decode, KeyValue((.=)) )
import Data.Time.Clock ( getCurrentTime )
import Haskode.Agent
    ( AgentState(asSession, asConversation),
      autoApprove,
      autoReject,
      initState,
      recordConversationReset,
      recordSessionEnd,
      recordSessionStart,
      recordSessionStartWithResume,
      runAgent )
import Haskode.Commands ( resetConversation )
import Haskode.Config ( defaultConfig )
import Haskode.Core
    ( ToolResult(trOutput),
      mkAssistantMessage,
      Message(msgCallId, msgRole, msgToolCalls, msgContent),
      Role(Assistant, User, System),
      ToolCall(ToolCall, tcId) )
import Haskode.Policy ( defaultPolicy )
import Haskode.Provider
    ( scriptedProvider,
      stubProvider,
      CompletionResponse(crToolCalls, CompletionResponse, crReply) )
import Haskode.Session
    ( emptyLog,
      logEvent,
      events,
      flushLog,
      flushLogOnException,
      Event(..),
      EventType(..),
      SessionSummary(..),
      summarizeSession,
      formatSessionSummary,
      isMeaningfulSession,
      ResumeResult(..),
      loadResumeEvents,
      resumeContextEventsToConversation,
      formatResumeSummary )
import Haskode.Test.Util ( cleanup, withTestDir, Test )
import Haskode.Tools
    ( defaultRegistry, shellTool, Tool(toolExecute) )
import System.Directory
    ( createDirectory,
      doesFileExist,
      getCurrentDirectory,
      getTemporaryDirectory,
      removeDirectoryRecursive,
      removeFile,
      renameFile,
      setCurrentDirectory )
import System.FilePath ( (</>) )
import System.IO ( hClose, hFileSize, openFile, IOMode(ReadMode) )
import System.Info ( os )
import qualified Data.Aeson.KeyMap as KM ( lookup, member )
import qualified Data.Aeson.Key as Key ( fromText )
import qualified Data.ByteString.Lazy as LBS
    ( appendFile, null, readFile, split )
import qualified Data.Map.Strict as Map ( findWithDefault )
import qualified Data.Text as T
    ( Text, isInfixOf, replicate, take, pack, unpack )
-- ---------------------------------------------------------------------------

-- | When the assistant requests multiple tool calls, the tool results
--   appear in the conversation in the same order as the tool calls.
--   This is critical for the OpenAI wire format: tool results must
--   follow the assistant message that requested them.
testMultiToolResultOrder :: Test
testMultiToolResultOrder = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Let me check both."
        , crToolCalls = Just
            [ ToolCall "tc-o1" "read_file" (object ["path" .= ("a.hs" :: T.Text)])
            , ToolCall "tc-o2" "read_file" (object ["path" .= ("b.hs" :: T.Text)])
            , ToolCall "tc-o3" "list_files" (object ["dir" .= ("." :: T.Text)])
            ]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let cfg   = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove False
  state' <- runAgent state "check files"
  let conv = asConversation state'
      -- Find tool-result messages (those with msgCallId set)
      toolResults = filter (\m -> msgCallId m /= Nothing) conv
      callIds     = map (maybe "" id . msgCallId) toolResults
  -- The tool results should appear in the same order as the tool calls
  if callIds == ["tc-o1", "tc-o2", "tc-o3"]
    then pure $ Right ()
    else pure $ Left $ "Tool result order: " ++ show callIds

-- | Assistant message with tool_calls preserves them in conversation.
testMultiToolAssistantPreservesCalls :: Test
testMultiToolAssistantPreservesCalls = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Checking."
        , crToolCalls = Just
            [ ToolCall "tc-p1" "read_file" (object ["path" .= ("x.hs" :: T.Text)])
            , ToolCall "tc-p2" "list_files" (object ["dir" .= ("." :: T.Text)])
            ]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "All done."
        , crToolCalls = Nothing
        }
    ]
  let cfg   = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove False
  state' <- runAgent state "check"
  let conv = asConversation state'
      -- Find assistant messages with tool_calls
      asstWithCalls = filter
        (\m -> msgRole m == Assistant && msgToolCalls m /= Nothing) conv
  case asstWithCalls of
    [m] -> case msgToolCalls m of
      Just tcs | length tcs == 2 -> pure $ Right ()
      other -> pure $ Left $ "Expected 2 tool calls, got: " ++ show other
    _ -> pure $ Left $ "Expected 1 assistant msg with calls, got: "
                     ++ show (length asstWithCalls)

-- | Multi-tool conversation: tool results reference the correct call IDs.
testMultiToolCallIdCorrespondence :: Test
testMultiToolCallIdCorrespondence = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Let me check."
        , crToolCalls = Just
            [ ToolCall "tc-c1" "read_file" (object ["path" .= ("a.hs" :: T.Text)])
            , ToolCall "tc-c2" "read_file" (object ["path" .= ("b.hs" :: T.Text)])
            ]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let cfg   = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove False
  state' <- runAgent state "check"
  let conv = asConversation state'
      -- Get tool_calls from the assistant message
      asstMsgs = filter
        (\m -> msgRole m == Assistant && msgToolCalls m /= Nothing) conv
      toolCallIds = case asstMsgs of
        (m:_) -> case msgToolCalls m of
          Just tcs -> map tcId tcs
          Nothing  -> []
        _ -> []
      -- Get call IDs from tool-result messages
      toolResults = filter (\m -> msgCallId m /= Nothing) conv
      resultIds   = map (maybe "" id . msgCallId) toolResults
  -- Every tool call ID should have a matching tool result
  if toolCallIds == resultIds && not (null toolCallIds)
    then pure $ Right ()
    else pure $ Left $ "callIds=" ++ show toolCallIds ++ " resultIds=" ++ show resultIds

-- | Helper: generate a command that produces very long output (cross-platform).
veryLongOutputCmd :: String
veryLongOutputCmd
  | os == "mingw32" = "powershell -c \"1..2000 | % { 'abcdefghijabcdefghijabcdefghij' }\""
  | otherwise       = "seq 1 2000 | while read i; do printf 'abcdefghij%.0s' $(seq 1 3); done"

-- | Shell truncation metadata is present in tool output when output is long.
testShellTruncationMetaNewFormat :: Test
testShellTruncationMetaNewFormat = do
  -- Generate output that exceeds shellOutputLimit (4096 chars)
  let args = object ["command" .= (T.pack veryLongOutputCmd :: T.Text)]
  result <- toolExecute shellTool args
  let out = trOutput result
  -- Check for the new metadata format
  let hasTruncMeta = T.isInfixOf "[truncated:" out
      hasReturned  = T.isInfixOf "returned" out
      hasDropped   = T.isInfixOf "dropped" out
  if hasTruncMeta && hasReturned && hasDropped
    then pure $ Right ()
    else pure $ Left $ "truncMeta=" ++ show hasTruncMeta
                     ++ " returned=" ++ show hasReturned
                     ++ " dropped=" ++ show hasDropped

-- | Shell with short output has no truncation metadata.
testShellShortOutputNoTruncMeta :: Test
testShellShortOutputNoTruncMeta = do
  let args = object ["command" .= ("echo short-output-test" :: T.Text)]
  result <- toolExecute shellTool args
  let out = trOutput result
  if not (T.isInfixOf "[truncated" out) && T.isInfixOf "short-output-test" out
    then pure $ Right ()
    else pure $ Left $ "Unexpected truncation metadata in short output: " ++ T.unpack out

-- ---------------------------------------------------------------------------
-- Session event data content tests
-- ---------------------------------------------------------------------------

-- | EAssistantReply event data includes tool-call summary when the
--   assistant response contains tool calls.  The summary should list
--   each call ID and tool name.
testSessionEventAssistantReplyWithToolCalls :: Test
testSessionEventAssistantReplyWithToolCalls = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Let me check."
        , crToolCalls = Just
            [ ToolCall "tc-ar1" "read_file" (object ["path" .= ("x.hs" :: T.Text)])
            , ToolCall "tc-ar2" "list_files" (object ["dir" .= ("." :: T.Text)])
            ]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove False
  state' <- runAgent state "check"
  let evts = events (asSession state')
      -- Find the first EAssistantReply (the one with tool calls)
      asstEvts = filter (\e -> evType e == EAssistantReply) evts
  case asstEvts of
    (firstAsst:_) -> do
      let d = evData firstAsst
      if T.isInfixOf "tc-ar1=read_file" d && T.isInfixOf "tc-ar2=list_files" d
        then pure $ Right ()
        else pure $ Left $ "Missing tool-call summary in assistant event: " ++ T.unpack d
    _ -> pure $ Left "No EAssistantReply events found"

-- | EAssistantReply event data is plain text when no tool calls.
testSessionEventAssistantReplyTextOnly :: Test
testSessionEventAssistantReplyTextOnly = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Hello there."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove False
  state' <- runAgent state "hi"
  let evts = events (asSession state')
      asstEvts = filter (\e -> evType e == EAssistantReply) evts
  case asstEvts of
    (firstAsst:_) ->
      if evData firstAsst == "Hello there."
        then pure $ Right ()
        else pure $ Left $ "Expected plain text, got: " ++ T.unpack (evData firstAsst)
    _ -> pure $ Left "No EAssistantReply events found"

-- | EToolCall event data contains the call ID and tool name.
testSessionEventToolCallData :: Test
testSessionEventToolCallData = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Reading."
        , crToolCalls = Just [ToolCall "tc-tc1" "read_file"
                                (object ["path" .= ("foo.hs" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove False
  state' <- runAgent state "read foo"
  let evts = events (asSession state')
      toolCallEvts = filter (\e -> evType e == EToolCall) evts
  case toolCallEvts of
    (tcEvt:_) -> do
      let d = evData tcEvt
      if T.isInfixOf "tc-tc1" d && T.isInfixOf "read_file" d
        then pure $ Right ()
        else pure $ Left $ "EToolCall missing call ID or name: " ++ T.unpack d
    _ -> pure $ Left "No EToolCall events found"

-- | EToolResult event data contains the call ID and output.
testSessionEventToolResultData :: Test
testSessionEventToolResultData = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-session-tr-test"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "tr-test.txt") "session test content"
  origDir <- getCurrentDirectory
  setCurrentDirectory root
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Reading."
        , crToolCalls = Just [ToolCall "tc-tr1" "read_file"
                                 (object ["path" .= ("tr-test.txt" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove False
  state' <- runAgent state "read file"
  setCurrentDirectory origDir
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  let evts = events (asSession state')
      trEvts = filter (\e -> evType e == EToolResult) evts
  case trEvts of
    (trEvt:_) -> do
      let d = evData trEvt
      if T.isInfixOf "tc-tr1" d && T.isInfixOf "session test content" d
        then pure $ Right ()
        else pure $ Left $ "EToolResult missing call ID or output: " ++ T.unpack d
    _ -> pure $ Left "No EToolResult events found"

-- | Session events are in chronological order (type sequence matches
--   the expected flow for a simple tool-call round trip).
--   Note: even Allow decisions generate an EPolicyDecision event.
testSessionEventChronologicalOrder :: Test
testSessionEventChronologicalOrder = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Let me read."
        , crToolCalls = Just [ToolCall "tc-ord1" "read_file"
                                (object ["path" .= ("x.hs" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done reading."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove False
  state' <- runAgent state "read x"
  let types = map evType (events (asSession state'))
  -- Expected order: UserMessage, AssistantReply, PolicyDecision (Allow),
  --                 ToolCall, ToolResult, AssistantReply
  if types == [EUserMessage, EAssistantReply, EPolicyDecision,
               EToolCall, EToolResult, EAssistantReply]
    then pure $ Right ()
    else pure $ Left $ "Event order: " ++ show types

-- | Multi-tool + policy-approved shell: session events are complete
--   and in correct order.  This is the key integration test that
--   exercises the full audit trail: user message, assistant reply
--   (with tool_calls summary), multiple tool calls + results,
--   another assistant reply requesting shell, policy decision (AskUser),
--   approval, shell execution, and final assistant reply.
testMultiToolThenShellSessionEvents :: Test
testMultiToolThenShellSessionEvents = do
  prov <- scriptedProvider
    [ -- First response: request two read_file calls
      CompletionResponse
        { crReply     = mkAssistantMessage "Let me read both files."
        , crToolCalls = Just
            [ ToolCall "tc-ms1" "read_file" (object ["path" .= ("a.hs" :: T.Text)])
            , ToolCall "tc-ms2" "read_file" (object ["path" .= ("b.hs" :: T.Text)])
            ]
        }
    , -- Second response: now request a shell command
      CompletionResponse
        { crReply     = mkAssistantMessage "Now let me run the tests."
        , crToolCalls = Just [ToolCall "tc-ms3" "shell"
                                (object ["command" .= ("echo shell-ok" :: T.Text)])]
        }
    , -- Third response: final summary
      CompletionResponse
        { crReply     = mkAssistantMessage "All tasks complete."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove False
  state' <- runAgent state "read files and run tests"
  let evts = events (asSession state')
      types = map evType evts

  -- Verify event type sequence:
  --   UserMessage, AssistantReply (with tool_calls),
  --   PolicyDecision (Allow), ToolCall, ToolResult,
  --   PolicyDecision (Allow), ToolCall, ToolResult,
  --   AssistantReply (with tool_calls),
  --   PolicyDecision (AskUser), PolicyDecision (approved),
  --   ToolCall, ToolResult,
  --   AssistantReply (final)
  let expectedTypes =
        [ EUserMessage
        , EAssistantReply    -- "Let me read both files." + tool_calls
        , EPolicyDecision    -- Allow for read_file (tc-ms1)
        , EToolCall          -- tc-ms1 read_file
        , EToolResult        -- tc-ms1 result
        , EPolicyDecision    -- Allow for read_file (tc-ms2)
        , EToolCall          -- tc-ms2 read_file
        , EToolResult        -- tc-ms2 result
        , EAssistantReply    -- "Now let me run the tests." + tool_calls
        , EPolicyDecision    -- AskUser for shell
        , EPolicyDecision    -- approved by user
        , EToolCall          -- tc-ms3 shell
        , EToolResult        -- tc-ms3 result
        , EAssistantReply    -- "All tasks complete."
        ]

  -- Check that key event types are present
  let hasUser    = EUserMessage `elem` types
      hasAsst    = EAssistantReply `elem` types
      hasTCall   = EToolCall `elem` types
      hasTResult = EToolResult `elem` types
      hasPolicy  = EPolicyDecision `elem` types

  -- Check that the first assistant reply includes tool-call summary
      asstEvts = filter (\e -> evType e == EAssistantReply) evts
      firstAsstOk = case asstEvts of
        (a:_) -> T.isInfixOf "tc-ms1=read_file" (evData a)
        _     -> False

  -- Check that policy decision events include approval
      policyEvts = filter (\e -> evType e == EPolicyDecision) evts
      hasApproval = any (T.isInfixOf "approved by user" . evData) policyEvts

  -- Check that shell tool result contains exit code
      trEvts = filter (\e -> evType e == EToolResult) evts
      shellResultOk = any (\e -> T.isInfixOf "tc-ms3" (evData e)
                              && T.isInfixOf "[exit]" (evData e)) trEvts

  -- Check the exact order matches expected
      orderOk = types == expectedTypes

  if hasUser && hasAsst && hasTCall && hasTResult && hasPolicy
     && firstAsstOk && hasApproval && shellResultOk && orderOk
    then pure $ Right ()
    else pure $ Left $
      "order=" ++ show (types == expectedTypes)
      ++ " types=" ++ show types
      ++ " firstAsstToolCalls=" ++ show firstAsstOk
      ++ " approval=" ++ show hasApproval
      ++ " shellResult=" ++ show shellResultOk

-- | Session event for a Deny decision contains the denial reason.
testSessionEventDenyData :: Test
testSessionEventDenyData = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Let me delete."
        , crToolCalls = Just [ToolCall "tc-deny1" "shell"
                                (object ["command" .= ("rm -rf /" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "OK."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove False
  state' <- runAgent state "delete"
  let evts = events (asSession state')
      policyEvts = filter (\e -> evType e == EPolicyDecision) evts
      denyEvts = filter (T.isInfixOf "Deny" . evData) policyEvts
  case denyEvts of
    (d:_) ->
      if T.isInfixOf "dangerous" (evData d)
        then pure $ Right ()
        else pure $ Left $ "Deny event missing reason: " ++ T.unpack (evData d)
    _ -> pure $ Left "No Deny policy events found"

-- | Session event for a rejected tool call contains "denied by user".
testSessionEventRejectData :: Test
testSessionEventRejectData = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Let me run that."
        , crToolCalls = Just [ToolCall "tc-rej1" "shell"
                                (object ["command" .= ("ls" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "OK, skipping."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoReject False
  state' <- runAgent state "list"
  let evts = events (asSession state')
      trEvts = filter (\e -> evType e == EToolResult) evts
      deniedEvts = filter (T.isInfixOf "denied by user" . evData) trEvts
  case deniedEvts of
    (_:_) -> pure $ Right ()
    _     -> pure $ Left $ "No 'denied by user' in tool results: "
                          ++ show (map evData trEvts)

-- ---------------------------------------------------------------------------
-- flushLog persistence tests
-- ---------------------------------------------------------------------------

-- | Helper: read a JSONL file and return each line as a decoded Value.
--   Filters out empty lines (e.g. trailing newline).
readJsonlFile :: FilePath -> IO [Value]
readJsonlFile path = do
  bytes <- LBS.readFile path
  let lines' = LBS.split (fromIntegral (fromEnum '\n')) bytes
  pure [ v | line <- lines', not (LBS.null line), Just v <- [decode line] ]

-- | flushLog writes events as valid JSONL in chronological order.
testFlushLogWritesJsonl :: Test
testFlushLogWritesJsonl = do
  tmpDir <- getTemporaryDirectory
  now    <- getCurrentTime
  -- Clean up any stale session.jsonl from prior tests (flushLog appends).
  let path = tmpDir </> "session.jsonl"
  _ <- try (removeFile path) :: IO (Either IOException ())
  let log' = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EUserMessage "hello"
        , Event now EAssistantReply "hi there"
        , Event now EToolCall "tc-1 read_file"
        , Event now EToolResult "tc-1 file contents"
        ]
  flushLog tmpDir 0 log'
  exists <- doesFileExist path
  if not exists
    then pure $ Left "session.jsonl was not created"
    else do
      vals <- readJsonlFile path
      -- Each line should be a JSON object with "time", "type", "data"
      let isValidEvent v = case v of
            Object o ->
              KM.member (Key.fromText "time") o
              && KM.member (Key.fromText "type") o
              && KM.member (Key.fromText "data") o
            _ -> False
      if length vals == 4 && all isValidEvent vals
        then do
          -- Verify chronological order by checking "type" fields
          let types = map (\v -> case v of
                Object o -> KM.lookup (Key.fromText "type") o
                _        -> Nothing) vals
          if types == [ Just (String "user_message")
                      , Just (String "assistant_reply")
                      , Just (String "tool_call")
                      , Just (String "tool_result")
                      ]
            then do
              cleanup path
              pure $ Right ()
            else do
              cleanup path
              pure $ Left $ "Event type order: " ++ show types
        else do
          cleanup path
          pure $ Left $ "Expected 4 valid events, got " ++ show (length vals)

-- | flushLog on an empty log does not create a file (no events to write).
testFlushLogEmpty :: Test
testFlushLogEmpty = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir </> "session.jsonl"
  -- Clean up any leftover file from prior tests
  _ <- try (removeFile path) :: IO (Either IOException ())
  flushLog tmpDir 0 emptyLog
  exists <- doesFileExist path
  if not exists
    then pure $ Right ()
    else do
      cleanup path
      pure $ Left "Expected no file for empty log, but file exists"

-- | flushLog appends on subsequent calls (does not overwrite).
testFlushLogAppends :: Test
testFlushLogAppends = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir </> "session.jsonl"
  -- Clean up any leftover file
  exists <- doesFileExist path
  if exists then removeFile path else pure ()
  now <- getCurrentTime
  -- First flush: 2 events
  let log1 = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EUserMessage "first"
        , Event now EAssistantReply "reply1"
        ]
  flushLog tmpDir 0 log1
  -- Second flush: 1 event (appends to the same file)
  let log2 = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EUserMessage "second"
        ]
  flushLog tmpDir 0 log2
  vals <- readJsonlFile path
  cleanup path
  if length vals == 3
    then pure $ Right ()
    else pure $ Left $ "Expected 3 events after 2 flushes, got " ++ show (length vals)

-- | Agent run + flushLog: a full agent session flushes events that
--   decode back to the expected event types.  Uses scriptedProvider
--   so no network access is needed.
testFlushLogAgentIntegration :: Test
testFlushLogAgentIntegration = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Let me read."
        , crToolCalls = Just [ToolCall "tc-fl1" "read_file"
                                (object ["path" .= ("foo.hs" :: T.Text)])]
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Done reading."
        , crToolCalls = Nothing
        }
    ]
  let state = initState defaultConfig prov defaultPolicy defaultRegistry autoApprove False
  state' <- runAgent state "read foo"
  tmpDir <- getTemporaryDirectory
  let flushPath = tmpDir </> "session.jsonl"
  -- Clean up any leftover file
  _ <- try (removeFile flushPath) :: IO (Either IOException ())
  flushLog tmpDir 0 (asSession state')
  exists <- doesFileExist flushPath
  if not exists
    then pure $ Left "session.jsonl not created after agent run"
    else do
      vals <- readJsonlFile flushPath
      cleanup flushPath
      let types = map (\v -> case v of
            Object o -> KM.lookup (Key.fromText "type") o
            _        -> Nothing) vals
      -- Expected: user_message, assistant_reply, policy_decision,
      --           tool_call, tool_result, assistant_reply
      if types == [ Just (String "user_message")
                  , Just (String "assistant_reply")
                  , Just (String "policy_decision")
                  , Just (String "tool_call")
                  , Just (String "tool_result")
                  , Just (String "assistant_reply")
                  ]
        then pure $ Right ()
        else pure $ Left $ "Flushed event types: " ++ show types

-- ---------------------------------------------------------------------------
-- flushLogOnException tests
-- ---------------------------------------------------------------------------

-- | flushLogOnException does NOT flush when the action succeeds.
testFlushLogOnExceptionNoFlushOnSuccess :: Test
testFlushLogOnExceptionNoFlushOnSuccess = do
  tmpDir <- getTemporaryDirectory
  now    <- getCurrentTime
  let log' = logEvent (Event now EUserMessage "test") emptyLog
      path = tmpDir </> "session.jsonl"
  _ <- try (removeFile path) :: IO (Either IOException ())
  val <- flushLogOnException tmpDir 0 log' (pure 42 :: IO Int)
  exists <- doesFileExist path
  cleanup path
  if val == 42 && not exists
    then pure $ Right ()
    else pure $ Left $ "val=" ++ show val ++ " fileExists=" ++ show exists

-- | flushLogOnException flushes the session log when the action throws.
testFlushLogOnExceptionFlushesOnThrow :: Test
testFlushLogOnExceptionFlushesOnThrow = do
  tmpDir <- getTemporaryDirectory
  now    <- getCurrentTime
  let log' = logEvent (Event now EUserMessage "before-exception") emptyLog
      path = tmpDir </> "session.jsonl"
  _ <- try (removeFile path) :: IO (Either IOException ())
  let badAction = throwIO (userError "simulated error") :: IO ()
  result <- try (flushLogOnException tmpDir 0 log' badAction) :: IO (Either IOException ())
  exists <- doesFileExist path
  case (result, exists) of
    (Left _, True) -> do
      vals <- readJsonlFile path
      cleanup path
      if length vals == 1
        then pure $ Right ()
        else pure $ Left $ "Expected 1 event, got " ++ show (length vals)
    (Left _, False) -> do
      cleanup path
      pure $ Left "Exception was thrown but session.jsonl was not created"
    (Right _, _) -> do
      cleanup path
      pure $ Left "Expected exception but action succeeded"

-- | flushLogOnException does not create a file for an empty log on exception.
testFlushLogOnExceptionEmptyLog :: Test
testFlushLogOnExceptionEmptyLog = do
  tmpDir <- getTemporaryDirectory
  let path = tmpDir </> "session.jsonl"
  _ <- try (removeFile path) :: IO (Either IOException ())
  let badAction = throwIO (userError "simulated") :: IO ()
  _ <- try (flushLogOnException tmpDir 0 emptyLog badAction) :: IO (Either IOException ())
  exists <- doesFileExist path
  cleanup path
  if not exists
    then pure $ Right ()
    else pure $ Left "Expected no file for empty log, but file exists"

-- ---------------------------------------------------------------------------
-- Session log rotation tests
-- ---------------------------------------------------------------------------

-- | Helper: read file size, returning 0 if the file does not exist.
fileSizeIfExists :: FilePath -> IO Integer
fileSizeIfExists path = do
  exists <- doesFileExist path
  if exists
    then do
      h <- openFile path ReadMode
      s <- hFileSize h
      hClose h
      pure s
    else pure 0

-- | Normal flush still appends when existing log is under the limit.
testFlushLogRotationUnderLimit :: Test
testFlushLogRotationUnderLimit = do
  tmpDir <- getTemporaryDirectory
  let path   = tmpDir </> "session.jsonl"
      backup = tmpDir </> "session.jsonl.1"
  -- Clean slate
  _ <- try (removeFile path) :: IO (Either IOException ())
  _ <- try (removeFile backup) :: IO (Either IOException ())
  now <- getCurrentTime
  -- Write an initial small log.
  let log1 = logEvent (Event now EUserMessage "first") emptyLog
  flushLog tmpDir 0 log1
  -- Flush again with a high limit — should NOT rotate.
  let log2 = logEvent (Event now EAssistantReply "second") emptyLog
  flushLog tmpDir 1000000 log2
  -- Both events should be in the main file.
  vals <- readJsonlFile path
  backupExists <- doesFileExist backup
  cleanup path
  cleanup backup
  if length vals == 2 && not backupExists
    then pure $ Right ()
    else pure $ Left $ "Under-limit: events=" ++ show (length vals)
                     ++ " backupExists=" ++ show backupExists

-- | Oversized existing session.jsonl rotates to .1 before new events.
testFlushLogRotationOverLimit :: Test
testFlushLogRotationOverLimit = do
  tmpDir <- getTemporaryDirectory
  let path   = tmpDir </> "session.jsonl"
      backup = tmpDir </> "session.jsonl.1"
  -- Clean slate
  _ <- try (removeFile path) :: IO (Either IOException ())
  _ <- try (removeFile backup) :: IO (Either IOException ())
  now <- getCurrentTime
  -- Write enough events to exceed a tiny limit.
  let bigLog = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EUserMessage (T.replicate 200 "x")
        , Event now EAssistantReply (T.replicate 200 "y")
        ]
  flushLog tmpDir 0 bigLog
  sizeBefore <- fileSizeIfExists path
  -- Now flush with a limit smaller than the existing file.
  let newLog = logEvent (Event now EToolCall "new event") emptyLog
  flushLog tmpDir 100 newLog
  -- The old content should be in backup, new file should have only the new event.
  newVals <- readJsonlFile path
  backupExists <- doesFileExist backup
  backupVals <- if backupExists then readJsonlFile backup else pure []
  cleanup path
  cleanup backup
  if sizeBefore > 100
     && backupExists
     && length backupVals == 2
     && length newVals == 1
    then pure $ Right ()
    else pure $ Left $ "Over-limit: sizeBefore=" ++ show sizeBefore
                     ++ " backupExists=" ++ show backupExists
                     ++ " backupEvents=" ++ show (length backupVals)
                     ++ " newEvents=" ++ show (length newVals)

-- | Existing .1 backup is replaced deterministically on re-rotation.
testFlushLogRotationBackupReplaced :: Test
testFlushLogRotationBackupReplaced = do
  tmpDir <- getTemporaryDirectory
  let path   = tmpDir </> "session.jsonl"
      backup = tmpDir </> "session.jsonl.1"
  -- Clean slate
  _ <- try (removeFile path) :: IO (Either IOException ())
  _ <- try (removeFile backup) :: IO (Either IOException ())
  now <- getCurrentTime
  -- Create an old backup with known content.
  let oldBackup = logEvent (Event now ESessionStart "old backup") emptyLog
  flushLog tmpDir 0 oldBackup
  renameFile path backup
  -- Create an oversized main log.
  let bigLog = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EUserMessage (T.replicate 200 "a")
        , Event now EAssistantReply (T.replicate 200 "b")
        ]
  flushLog tmpDir 0 bigLog
  -- Rotate with a small limit.
  let newLog = logEvent (Event now EToolCall "replacement") emptyLog
  flushLog tmpDir 100 newLog
  -- The backup should now contain the bigLog events (2), not the old backup (1).
  backupVals <- readJsonlFile backup
  newVals <- readJsonlFile path
  cleanup path
  cleanup backup
  if length backupVals == 2 && length newVals == 1
    then pure $ Right ()
    else pure $ Left $ "Backup replaced: backupEvents=" ++ show (length backupVals)
                     ++ " newEvents=" ++ show (length newVals)

-- | Empty session still does not create files, even with rotation enabled.
testFlushLogRotationEmptySession :: Test
testFlushLogRotationEmptySession = do
  tmpDir <- getTemporaryDirectory
  let path   = tmpDir </> "session.jsonl"
      backup = tmpDir </> "session.jsonl.1"
  -- Clean slate
  _ <- try (removeFile path) :: IO (Either IOException ())
  _ <- try (removeFile backup) :: IO (Either IOException ())
  -- Flush empty log with rotation enabled.
  flushLog tmpDir 100 emptyLog
  exists <- doesFileExist path
  backupExists <- doesFileExist backup
  cleanup path
  cleanup backup
  if not exists && not backupExists
    then pure $ Right ()
    else pure $ Left $ "Empty rotation: fileExists=" ++ show exists
                     ++ " backupExists=" ++ show backupExists

-- | After rotation the new JSONL file contains valid JSON events.
testFlushLogRotationValidJsonl :: Test
testFlushLogRotationValidJsonl = do
  tmpDir <- getTemporaryDirectory
  let path   = tmpDir </> "session.jsonl"
      backup = tmpDir </> "session.jsonl.1"
  -- Clean slate
  _ <- try (removeFile path) :: IO (Either IOException ())
  _ <- try (removeFile backup) :: IO (Either IOException ())
  now <- getCurrentTime
  -- Create an oversized log.
  let bigLog = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EUserMessage (T.replicate 200 "c")
        , Event now EAssistantReply (T.replicate 200 "d")
        ]
  flushLog tmpDir 0 bigLog
  -- Rotate and write new events.
  let newLog = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EToolCall "tc-1 read_file"
        , Event now EToolResult "tc-1 contents"
        ]
  flushLog tmpDir 100 newLog
  -- Read and validate the new file.
  vals <- readJsonlFile path
  let isValidEvent v = case v of
        Object o ->
          KM.member (Key.fromText "time") o
          && KM.member (Key.fromText "type") o
          && KM.member (Key.fromText "data") o
        _ -> False
  cleanup path
  cleanup backup
  if length vals == 2 && all isValidEvent vals
    then pure $ Right ()
    else pure $ Left $ "Valid JSONL after rotation: events=" ++ show (length vals)

-- Session summary tests (--show-session groundwork)
-- ---------------------------------------------------------------------------

-- | summarizeSession returns zero events for a missing file
--   and reports the log path with no backup.
testSummarizeSessionMissingFile :: Test
testSummarizeSessionMissingFile = withTestDir "haskode-summarize-missing" $ \root -> do
  ss <- summarizeSession root
  let pathOk = ssLogPath ss == root </> "session.jsonl"
      backupOk = not (ssBackupExists ss)
  if ssTotalEvents ss == 0 && ssMalformedLines ss == 0 && pathOk && backupOk
    then pure $ Right ()
    else pure $ Left $ "Missing file: total=" ++ show (ssTotalEvents ss)
                     ++ " malformed=" ++ show (ssMalformedLines ss)
                     ++ " pathOk=" ++ show pathOk
                     ++ " backupOk=" ++ show backupOk

-- | summarizeSession counts events by type from valid JSONL.
testSummarizeSessionValidJsonl :: Test
testSummarizeSessionValidJsonl = withTestDir "haskode-summarize-valid" $ \root -> do
  now <- getCurrentTime
  let log' = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EUserMessage "hello"
        , Event now EAssistantReply "hi"
        , Event now EToolCall "tc-1 read_file"
        , Event now EToolResult "tc-1 contents"
        , Event now EUserMessage "second"
        ]
  flushLog root 0 log'
  ss <- summarizeSession root
  let counts = ssTypeCounts ss
      ok = ssTotalEvents ss == 5
           && Map.findWithDefault 0 EUserMessage counts == 2
           && Map.findWithDefault 0 EAssistantReply counts == 1
           && Map.findWithDefault 0 EToolCall counts == 1
           && Map.findWithDefault 0 EToolResult counts == 1
           && ssMalformedLines ss == 0
           && ssFirstTime ss /= Nothing
           && ssLastTime ss /= Nothing
  if ok
    then pure $ Right ()
    else pure $ Left $ "Valid JSONL: total=" ++ show (ssTotalEvents ss)
                     ++ " counts=" ++ show counts
                     ++ " malformed=" ++ show (ssMalformedLines ss)

-- | summarizeSession counts malformed lines without crashing.
testSummarizeSessionMalformedLines :: Test
testSummarizeSessionMalformedLines = withTestDir "haskode-summarize-malformed" $ \root -> do
  now <- getCurrentTime
  let validLog = logEvent (Event now EUserMessage "ok") emptyLog
  flushLog root 0 validLog
  let path = root </> "session.jsonl"
  LBS.appendFile path "not valid json at all\n"
  LBS.appendFile path "{\"incomplete\": true}\n"
  ss <- summarizeSession root
  if ssTotalEvents ss == 1 && ssMalformedLines ss == 2
    then pure $ Right ()
    else pure $ Left $ "Malformed: total=" ++ show (ssTotalEvents ss)
                     ++ " malformed=" ++ show (ssMalformedLines ss)

-- | summarizeSession returns zero events for an empty file.
testSummarizeSessionEmptyFile :: Test
testSummarizeSessionEmptyFile = withTestDir "haskode-summarize-empty" $ \root -> do
  let path = root </> "session.jsonl"
  writeFile path ""
  ss <- summarizeSession root
  if ssTotalEvents ss == 0 && ssMalformedLines ss == 0
    then pure $ Right ()
    else pure $ Left $ "Empty file: total=" ++ show (ssTotalEvents ss)
                     ++ " malformed=" ++ show (ssMalformedLines ss)

-- | summarizeSession ignores rotated session.jsonl.1 backup.
testSummarizeSessionIgnoresBackup :: Test
testSummarizeSessionIgnoresBackup = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-summarize-backup"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  now <- getCurrentTime
  -- Write events to main file.
  let log1 = logEvent (Event now EUserMessage "main") emptyLog
  flushLog root 0 log1
  -- Write different events to backup.
  let log2 = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EAssistantReply "backup1"
        , Event now EToolCall "backup2"
        ]
  flushLog root 0 log2  -- writes to main (appends)
  -- Now move main to .1 and write a fresh main with 1 event.
  let path   = root </> "session.jsonl"
      backup = root </> "session.jsonl.1"
  renameFile path backup
  let log3 = logEvent (Event now EUserMessage "fresh") emptyLog
  flushLog root 0 log3
  ss <- summarizeSession root
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  -- Should count only the fresh main file (1 event), not the backup (3 events).
  if ssTotalEvents ss == 1
    then pure $ Right ()
    else pure $ Left $ "Backup ignored: total=" ++ show (ssTotalEvents ss)

-- | Normal agent execution is unchanged when --show-session is not used.
--   This is a smoke test verifying the agent loop still works after
--   the Session.hs changes (FromJSON instances, etc.).
testAgentUnchangedWithSessionSummary :: Test
testAgentUnchangedWithSessionSummary = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Still works."
        , crToolCalls = Nothing
        }
    ]
  let cfg   = defaultConfig
      state = initState cfg prov defaultPolicy defaultRegistry autoApprove False
  state' <- runAgent state "hello"
  let evts = events (asSession state')
      types = map evType evts
  if EUserMessage `elem` types && EAssistantReply `elem` types
    then pure $ Right ()
    else pure $ Left $ "Agent unchanged: events=" ++ show types

-- | formatSessionSummary includes key sections.
testFormatSessionSummaryContainsSections :: Test
testFormatSessionSummaryContainsSections = withTestDir "haskode-summarize-format" $ \root -> do
  now <- getCurrentTime
  let log' = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EUserMessage "hello"
        , Event now EAssistantReply "hi"
        ]
  flushLog root 0 log'
  ss <- summarizeSession root
  let out = formatSessionSummary ss
  if T.isInfixOf "Session summary" out
     && T.isInfixOf "Log:" out
     && T.isInfixOf "Total events:" out
     && T.isInfixOf "Malformed:" out
     && T.isInfixOf "Backup:" out
     && T.isInfixOf "user_message:" out
     && T.isInfixOf "assistant_reply:" out
    then pure $ Right ()
    else pure $ Left $ "formatSessionSummary missing sections: " ++ T.unpack (T.take 300 out)

-- ---------------------------------------------------------------------------
-- Lifecycle event tests
-- ---------------------------------------------------------------------------

-- | EConversationReset JSON roundtrip.
testConversationResetJSON :: Test
testConversationResetJSON = do
  now <- getCurrentTime
  let ev = Event now EConversationReset "conversation reset by /new"
  case decode (encode ev) :: Maybe Event of
    Nothing  -> pure $ Left "EConversationReset JSON roundtrip failed"
    Just ev'
      | evType ev' == EConversationReset && evData ev' == "conversation reset by /new" ->
          pure $ Right ()
      | otherwise -> pure $ Left $ "EConversationReset roundtrip mismatch: " ++ show ev'

-- | summarizeSession counts conversation_reset events.
testSummarizeSessionConversationReset :: Test
testSummarizeSessionConversationReset = withTestDir "haskode-summarize-reset" $ \root -> do
  now <- getCurrentTime
  let log' = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EUserMessage "hello"
        , Event now EAssistantReply "hi"
        , Event now EConversationReset "conversation reset by /new"
        , Event now EUserMessage "second"
        ]
  flushLog root 0 log'
  ss <- summarizeSession root
  let counts = ssTypeCounts ss
      ok = ssTotalEvents ss == 4
           && Map.findWithDefault 0 EConversationReset counts == 1
           && Map.findWithDefault 0 EUserMessage counts == 2
  if ok
    then pure $ Right ()
    else pure $ Left $ "conversation_reset count: total=" ++ show (ssTotalEvents ss)
                     ++ " counts=" ++ show counts

-- | formatSessionSummary includes conversation_reset line.
testFormatSessionSummaryConversationReset :: Test
testFormatSessionSummaryConversationReset = withTestDir "haskode-summarize-format-reset" $ \root -> do
  now <- getCurrentTime
  let log' = logEvent (Event now EConversationReset "reset") emptyLog
  flushLog root 0 log'
  ss <- summarizeSession root
  let out = formatSessionSummary ss
  if T.isInfixOf "conversation_reset:" out
    then pure $ Right ()
    else pure $ Left $ "formatSessionSummary missing conversation_reset: " ++ T.unpack (T.take 300 out)

-- | EConversationCompacted JSON roundtrip.
testConversationCompactedJSON :: Test
testConversationCompactedJSON = do
  now <- getCurrentTime
  let ev = Event now EConversationCompacted "compact memory"
  case decode (encode ev) :: Maybe Event of
    Nothing -> pure $ Left "EConversationCompacted JSON roundtrip failed"
    Just ev'
      | evType ev' == EConversationCompacted && evData ev' == "compact memory" ->
          pure $ Right ()
      | otherwise -> pure $ Left $ "EConversationCompacted roundtrip mismatch: " ++ show ev'

-- | summarizeSession counts accepted compaction events.
testSummarizeSessionConversationCompacted :: Test
testSummarizeSessionConversationCompacted = withTestDir "haskode-summarize-compacted" $ \root -> do
  now <- getCurrentTime
  let log' = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EUserMessage "hello"
        , Event now EAssistantReply "hi"
        , Event now EConversationCompacted "compact memory"
        ]
  flushLog root 0 log'
  ss <- summarizeSession root
  let counts = ssTypeCounts ss
      ok = ssTotalEvents ss == 3
           && Map.findWithDefault 0 EConversationCompacted counts == 1
  if ok
    then pure $ Right ()
    else pure $ Left $ "conversation_compacted count: total=" ++ show (ssTotalEvents ss)
                     ++ " counts=" ++ show counts

-- | formatSessionSummary includes conversation_compacted line.
testFormatSessionSummaryConversationCompacted :: Test
testFormatSessionSummaryConversationCompacted = withTestDir "haskode-summarize-format-compacted" $ \root -> do
  now <- getCurrentTime
  let log' = logEvent (Event now EConversationCompacted "compact memory") emptyLog
  flushLog root 0 log'
  ss <- summarizeSession root
  let out = formatSessionSummary ss
  if T.isInfixOf "conversation_compacted:" out
    then pure $ Right ()
    else pure $ Left $ "formatSessionSummary missing conversation_compacted: " ++ T.unpack (T.take 300 out)

-- | recordSessionStart produces an ESessionStart event.
testRecordSessionStartEvent :: Test
testRecordSessionStartEvent = do
  let cfg = defaultConfig
      state0 = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove False
  state1 <- recordSessionStart state0
  let evts = events (asSession state1)
      types = map evType evts
  if types == [ESessionStart]
    then pure $ Right ()
    else pure $ Left $ "recordSessionStart events: " ++ show types

-- | recordSessionStartWithResume produces an ESessionStart event with resume info.
testRecordSessionStartWithResumeEvent :: Test
testRecordSessionStartWithResumeEvent = do
  let cfg = defaultConfig
      state0 = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove True
  state1 <- recordSessionStartWithResume state0 4
  let evts = events (asSession state1)
  case evts of
    [ev] ->
      if evType ev == ESessionStart
         && T.isInfixOf "resumed" (evData ev)
         && T.isInfixOf "4 messages" (evData ev)
        then pure $ Right ()
        else pure $ Left $ "Unexpected event data: " ++ T.unpack (evData ev)
    _ -> pure $ Left $ "Expected 1 event, got " ++ show (length evts)

-- | recordSessionEnd produces an ESessionEnd event.
testRecordSessionEndEvent :: Test
testRecordSessionEndEvent = do
  let cfg = defaultConfig
      state0 = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove False
  state1 <- recordSessionEnd state0
  let evts = events (asSession state1)
      types = map evType evts
  if types == [ESessionEnd]
    then pure $ Right ()
    else pure $ Left $ "recordSessionEnd events: " ++ show types

-- | recordConversationReset produces an EConversationReset event.
testRecordConversationResetEvent :: Test
testRecordConversationResetEvent = do
  let cfg = defaultConfig
      state0 = initState cfg stubProvider defaultPolicy defaultRegistry autoApprove False
  state1 <- recordConversationReset state0
  let evts = events (asSession state1)
      types = map evType evts
  if types == [EConversationReset]
    then pure $ Right ()
    else pure $ Left $ "recordConversationReset events: " ++ show types

-- | /new command preserves existing session events and appends
--   a conversation_reset event.  Tests the pure layer:
--   resetConversation clears messages; recordConversationReset appends event.
testNewPreservesSessionAndAppendsReset :: Test
testNewPreservesSessionAndAppendsReset = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "Hello."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state0 = initState cfg prov defaultPolicy defaultRegistry autoApprove False
  -- Run an agent turn to accumulate session events
  state1 <- runAgent state0 "hi"
  let evtsBefore = events (asSession state1)
      countBefore = length evtsBefore
  -- Simulate /new: reset conversation, record reset event
  let state2 = resetConversation state1
  state3 <- recordConversationReset state2
  let evtsAfter = events (asSession state3)
      countAfter = length evtsAfter
      hasReset = EConversationReset `elem` map evType evtsAfter
      convCleared = null (asConversation state2)
  if countAfter == countBefore + 1 && hasReset && convCleared
    then pure $ Right ()
    else pure $ Left $ "before=" ++ show countBefore
                     ++ " after=" ++ show countAfter
                     ++ " hasReset=" ++ show hasReset
                     ++ " convCleared=" ++ show convCleared

-- | Full lifecycle: start, agent turn, reset, agent turn, end.
--   Verifies the complete event sequence.
testFullLifecycleEventSequence :: Test
testFullLifecycleEventSequence = do
  prov <- scriptedProvider
    [ CompletionResponse
        { crReply     = mkAssistantMessage "First reply."
        , crToolCalls = Nothing
        }
    , CompletionResponse
        { crReply     = mkAssistantMessage "Second reply."
        , crToolCalls = Nothing
        }
    ]
  let cfg = defaultConfig
      state0 = initState cfg prov defaultPolicy defaultRegistry autoApprove False
  -- session_start
  state1 <- recordSessionStart state0
  -- first turn
  state2 <- runAgent state1 "first"
  -- /new
  let state3 = resetConversation state2
  state4 <- recordConversationReset state3
  -- second turn
  state5 <- runAgent state4 "second"
  -- session_end
  state6 <- recordSessionEnd state5
  let evts = events (asSession state6)
      types = map evType evts
  -- Expected: session_start, user_message, assistant_reply,
  --           conversation_reset, user_message, assistant_reply, session_end
  let expected = [ ESessionStart, EUserMessage, EAssistantReply
                 , EConversationReset, EUserMessage, EAssistantReply, ESessionEnd]
  if types == expected
    then pure $ Right ()
    else pure $ Left $ "lifecycle events: " ++ show types
                     ++ " expected: " ++ show expected

-- ---------------------------------------------------------------------------
-- isMeaningfulSession tests
-- ---------------------------------------------------------------------------

-- | Empty log is not meaningful.
testIsMeaningfulEmpty :: Test
testIsMeaningfulEmpty =
  if not (isMeaningfulSession emptyLog)
    then pure $ Right ()
    else pure $ Left "empty log should not be meaningful"

-- | Lifecycle-only log ([session_start, session_end]) is not meaningful.
testIsMeaningfulLifecycleOnly :: Test
testIsMeaningfulLifecycleOnly = do
  now <- getCurrentTime
  let log' = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now ESessionStart "session started"
        , Event now ESessionEnd "session ended"
        ]
  if not (isMeaningfulSession log')
    then pure $ Right ()
    else pure $ Left "lifecycle-only log should not be meaningful"

-- | Lifecycle with conversation_reset is not meaningful.
testIsMeaningfulWithReset :: Test
testIsMeaningfulWithReset = do
  now <- getCurrentTime
  let log' = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now ESessionStart "session started"
        , Event now EConversationReset "conversation reset by /new"
        , Event now ESessionEnd "session ended"
        ]
  if not (isMeaningfulSession log')
    then pure $ Right ()
    else pure $ Left "lifecycle+reset log should not be meaningful"

-- | Accepted conversation compaction is meaningful and should be flushed.
testIsMeaningfulWithCompaction :: Test
testIsMeaningfulWithCompaction = do
  now <- getCurrentTime
  let log' = logEvent (Event now EConversationCompacted "compact memory") emptyLog
  if isMeaningfulSession log'
    then pure $ Right ()
    else pure $ Left "conversation_compacted log should be meaningful"

-- | Log with user_message is meaningful.
testIsMeaningfulWithUserMessage :: Test
testIsMeaningfulWithUserMessage = do
  now <- getCurrentTime
  let log' = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now ESessionStart "session started"
        , Event now EUserMessage "hello"
        , Event now ESessionEnd "session ended"
        ]
  if isMeaningfulSession log'
    then pure $ Right ()
    else pure $ Left "log with user_message should be meaningful"

-- | Log with assistant_reply is meaningful.
testIsMeaningfulWithAssistantReply :: Test
testIsMeaningfulWithAssistantReply = do
  now <- getCurrentTime
  let log' = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now ESessionStart "session started"
        , Event now EAssistantReply "hi"
        , Event now ESessionEnd "session ended"
        ]
  if isMeaningfulSession log'
    then pure $ Right ()
    else pure $ Left "log with assistant_reply should be meaningful"

-- | Log with tool_call is meaningful.
testIsMeaningfulWithToolCall :: Test
testIsMeaningfulWithToolCall = do
  now <- getCurrentTime
  let log' = logEvent (Event now EToolCall "tc-1 read_file") emptyLog
  if isMeaningfulSession log'
    then pure $ Right ()
    else pure $ Left "log with tool_call should be meaningful"

-- | Log with tool_result is meaningful.
testIsMeaningfulWithToolResult :: Test
testIsMeaningfulWithToolResult = do
  now <- getCurrentTime
  let log' = logEvent (Event now EToolResult "tc-1 contents") emptyLog
  if isMeaningfulSession log'
    then pure $ Right ()
    else pure $ Left "log with tool_result should be meaningful"

-- | Log with policy_decision is meaningful.
testIsMeaningfulWithPolicyDecision :: Test
testIsMeaningfulWithPolicyDecision = do
  now <- getCurrentTime
  let log' = logEvent (Event now EPolicyDecision "read_file: Allow") emptyLog
  if isMeaningfulSession log'
    then pure $ Right ()
    else pure $ Left "log with policy_decision should be meaningful"

-- | Full lifecycle with real content is meaningful.
testIsMeaningfulFullLifecycle :: Test
testIsMeaningfulFullLifecycle = do
  now <- getCurrentTime
  let log' = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now ESessionStart "session started"
        , Event now EUserMessage "hello"
        , Event now EAssistantReply "hi"
        , Event now EConversationReset "conversation reset by /new"
        , Event now EUserMessage "second"
        , Event now EAssistantReply "hey"
        , Event now ESessionEnd "session ended"
        ]
  if isMeaningfulSession log'
    then pure $ Right ()
    else pure $ Left "full lifecycle with content should be meaningful"

-- | flushLogOnException does NOT flush a lifecycle-only session on exception.
testFlushLogOnExceptionLifecycleOnly :: Test
testFlushLogOnExceptionLifecycleOnly = do
  tmpDir <- getTemporaryDirectory
  now    <- getCurrentTime
  let log' = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now ESessionStart "session started"
        , Event now ESessionEnd "session ended"
        ]
      path = tmpDir </> "session.jsonl"
  _ <- try (removeFile path) :: IO (Either IOException ())
  let badAction = throwIO (userError "simulated") :: IO ()
  _ <- try (flushLogOnException tmpDir 0 log' badAction) :: IO (Either IOException ())
  exists <- doesFileExist path
  cleanup path
  if not exists
    then pure $ Right ()
    else pure $ Left "lifecycle-only session should not create file on exception"

-- | formatSessionSummary for missing log includes Log: and no backup.
testFormatSessionSummaryMissingLog :: Test
testFormatSessionSummaryMissingLog = withTestDir "haskode-summarize-fmt-missing" $ \root -> do
  ss <- summarizeSession root
  let out = formatSessionSummary ss
  if T.isInfixOf "Log:" out
     && T.isInfixOf "Total events:  0" out
     && T.isInfixOf "Malformed:     0" out
     && T.isInfixOf "Backup:        (none)" out
     && T.isInfixOf "Time:          (none)" out
    then pure $ Right ()
    else pure $ Left $ "Missing log format: " ++ T.unpack (T.take 300 out)

-- | formatSessionSummary shows malformed count even when zero.
testFormatSessionSummaryMalformedZero :: Test
testFormatSessionSummaryMalformedZero = withTestDir "haskode-summarize-fmt-malformed0" $ \root -> do
  now <- getCurrentTime
  let log' = logEvent (Event now EUserMessage "ok") emptyLog
  flushLog root 0 log'
  ss <- summarizeSession root
  let out = formatSessionSummary ss
  if T.isInfixOf "Malformed:     0" out
    then pure $ Right ()
    else pure $ Left $ "Malformed zero not shown: " ++ T.unpack (T.take 300 out)

-- | formatSessionSummary shows malformed count when > 0.
testFormatSessionSummaryMalformedNonZero :: Test
testFormatSessionSummaryMalformedNonZero = withTestDir "haskode-summarize-fmt-malformed1" $ \root -> do
  now <- getCurrentTime
  let validLog = logEvent (Event now EUserMessage "ok") emptyLog
  flushLog root 0 validLog
  let path = root </> "session.jsonl"
  LBS.appendFile path "bad json line\n"
  ss <- summarizeSession root
  let out = formatSessionSummary ss
  if T.isInfixOf "Malformed:     1" out
    then pure $ Right ()
    else pure $ Left $ "Malformed non-zero not shown: " ++ T.unpack (T.take 300 out)

-- | summarizeSession detects backup file when present.
testSummarizeSessionBackupExists :: Test
testSummarizeSessionBackupExists = withTestDir "haskode-summarize-backup-exists" $ \root -> do
  let backup = root </> "session.jsonl.1"
  now <- getCurrentTime
  let log1 = logEvent (Event now EUserMessage "main") emptyLog
  flushLog root 0 log1
  writeFile backup "backup content"
  ss <- summarizeSession root
  if ssBackupExists ss
    then pure $ Right ()
    else pure $ Left "Expected ssBackupExists to be True"

-- | summarizeSession reports no backup when file is absent.
testSummarizeSessionBackupAbsent :: Test
testSummarizeSessionBackupAbsent = withTestDir "haskode-summarize-backup-absent" $ \root -> do
  let backup = root </> "session.jsonl.1"
  _ <- try (removeFile backup) :: IO (Either IOException ())
  now <- getCurrentTime
  let log1 = logEvent (Event now EUserMessage "main") emptyLog
  flushLog root 0 log1
  ss <- summarizeSession root
  if not (ssBackupExists ss)
    then pure $ Right ()
    else pure $ Left "Expected ssBackupExists to be False"

-- | formatSessionSummary shows backup line when backup exists.
testFormatSessionSummaryBackupExists :: Test
testFormatSessionSummaryBackupExists = withTestDir "haskode-summarize-fmt-backup" $ \root -> do
  let backup = root </> "session.jsonl.1"
  writeFile backup "backup content"
  now <- getCurrentTime
  let log1 = logEvent (Event now EUserMessage "main") emptyLog
  flushLog root 0 log1
  ss <- summarizeSession root
  let out = formatSessionSummary ss
  if T.isInfixOf "Backup:        session.jsonl.1 exists" out
    then pure $ Right ()
    else pure $ Left $ "Backup exists line missing: " ++ T.unpack (T.take 300 out)

-- ---------------------------------------------------------------------------
-- Resume tests
-- ---------------------------------------------------------------------------

-- | Helper: write a JSONL file and run a test, cleaning up afterward.
withJsonlFile :: FilePath -> [Event] -> (FilePath -> IO (Either String ())) -> IO (Either String ())
withJsonlFile dir evts action = do
  _ <- try (removeDirectoryRecursive dir) :: IO (Either IOException ())
  createDirectory dir
  flushLog dir 0 (foldl (\acc e -> logEvent e acc) emptyLog evts)
  result <- action dir
  _ <- try (removeDirectoryRecursive dir) :: IO (Either IOException ())
  pure result

-- | resumeContextEventsToConversation includes user messages.
testResumeIncludesUserMessages :: Test
testResumeIncludesUserMessages = do
  now <- getCurrentTime
  let evts = [ Event now EUserMessage "hello"
             , Event now EAssistantReply "hi there"
             , Event now EUserMessage "how are you"
             ]
      conv = resumeContextEventsToConversation evts
      contents = map msgContent conv
  if contents == ["hello", "hi there", "how are you"]
    then pure $ Right ()
    else pure $ Left $ "User messages: " ++ show contents

-- | resumeContextEventsToConversation includes assistant replies.
testResumeIncludesAssistantReplies :: Test
testResumeIncludesAssistantReplies = do
  now <- getCurrentTime
  let evts = [ Event now EUserMessage "question"
             , Event now EAssistantReply "answer"
             ]
      conv = resumeContextEventsToConversation evts
      roles = map msgRole conv
  if roles == [User, Assistant]
    then pure $ Right ()
    else pure $ Left $ "Roles: " ++ show roles

-- | resumeContextEventsToConversation ignores lifecycle-only events.
testResumeIgnoresLifecycleEvents :: Test
testResumeIgnoresLifecycleEvents = do
  now <- getCurrentTime
  let evts = [ Event now ESessionStart "session started"
             , Event now EUserMessage "hello"
             , Event now EAssistantReply "hi"
             , Event now ESessionEnd "session ended"
             ]
      conv = resumeContextEventsToConversation evts
  if length conv == 2
    then pure $ Right ()
    else pure $ Left $ "Expected 2 messages, got " ++ show (length conv)

-- | resumeContextEventsToConversation ignores policy decisions.
testResumeIgnoresPolicyDecisions :: Test
testResumeIgnoresPolicyDecisions = do
  now <- getCurrentTime
  let evts = [ Event now EUserMessage "do something"
             , Event now EAssistantReply "let me try"
             , Event now EPolicyDecision "read_file: Allow"
             , Event now EToolCall "tc-1 read_file {}"
             , Event now EToolResult "tc-1 file contents"
             , Event now EAssistantReply "done"
             ]
      conv = resumeContextEventsToConversation evts
      roles = map msgRole conv
  -- Only user_message and assistant_reply events are included
  if roles == [User, Assistant, Assistant]
    then pure $ Right ()
    else pure $ Left $ "Roles: " ++ show roles

-- | resumeContextEventsToConversation does NOT reconstruct tool_call as executable pending calls.
testResumeDoesNotReconstructToolCalls :: Test
testResumeDoesNotReconstructToolCalls = do
  now <- getCurrentTime
  let evts = [ Event now EUserMessage "read file"
             , Event now EAssistantReply "let me read | tool_calls: tc-1=read_file"
             , Event now EToolCall "tc-1 read_file {\"path\":\"x.hs\"}"
             , Event now EToolResult "tc-1 file contents"
             , Event now EAssistantReply "done"
             ]
      conv = resumeContextEventsToConversation evts
      -- No message should have tool_calls set
      hasToolCalls = any (\m -> msgToolCalls m /= Nothing) conv
  if not hasToolCalls
    then pure $ Right ()
    else pure $ Left "Tool calls should not be reconstructed"

-- | loadResumeEvents returns Nothing for a missing file.
testResumeLoadMissingFile :: Test
testResumeLoadMissingFile = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-resume-missing"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  result <- loadResumeEvents root
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Nothing -> pure $ Right ()
    Just _  -> pure $ Left "Expected Nothing for missing file"

-- | loadResumeEvents returns Nothing for an empty file.
testResumeLoadEmptyFile :: Test
testResumeLoadEmptyFile = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-resume-empty"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  writeFile (root </> "session.jsonl") ""
  result <- loadResumeEvents root
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Nothing -> pure $ Right ()
    Just _  -> pure $ Left "Expected Nothing for empty file"

-- | loadResumeEvents skips malformed lines without crashing.
testResumeSkipsMalformedLines :: Test
testResumeSkipsMalformedLines = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-resume-malformed"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  now <- getCurrentTime
  -- Write one valid event and two malformed lines
  let validLog = logEvent (Event now EUserMessage "ok") emptyLog
  flushLog root 0 validLog
  let path = root </> "session.jsonl"
  LBS.appendFile path "not valid json\n"
  LBS.appendFile path "{\"incomplete\": true}\n"
  result <- loadResumeEvents root
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Nothing -> pure $ Left "Expected Just for file with valid event"
    Just rr ->
      if rrMessageEventCount rr == 1 && rrMalformed rr == 2
        then pure $ Right ()
        else pure $ Left $ "eventCount=" ++ show (rrMessageEventCount rr)
                         ++ " malformed=" ++ show (rrMalformed rr)

-- | loadResumeEvents counts only safe text message events.
testResumeCountsOnlySafeTextMessages :: Test
testResumeCountsOnlySafeTextMessages = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-resume-count"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  now <- getCurrentTime
  let evts = [ Event now ESessionStart "started"
             , Event now EUserMessage "hello"
             , Event now EAssistantReply "hi"
             , Event now EPolicyDecision "Allow"
             , Event now EToolCall "tc-1 read_file"
             , Event now EToolResult "tc-1 contents"
             , Event now EConversationReset "reset"
             , Event now EUserMessage "after reset"
             , Event now EAssistantReply "reply after reset"
             , Event now ESessionEnd "ended"
             ]
  flushLog root 0 (foldl (\acc e -> logEvent e acc) emptyLog evts)
  result <- loadResumeEvents root
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Nothing -> pure $ Left "Expected Just"
    Just rr ->
      if rrMessageEventCount rr == 4
        then pure $ Right ()
        else pure $ Left $ "Expected 4 safe text message events, got " ++ show (rrMessageEventCount rr)

-- | loadResumeEvents reconstructs safe text context, not executable replay state.
testResumeLoadsOnlySafeTextContext :: Test
testResumeLoadsOnlySafeTextContext = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-resume-safe-text-context"
  now <- getCurrentTime
  let evts = [ Event now ESessionStart "started"
             , Event now EUserMessage "read the file"
             , Event now EAssistantReply "let me read | tool_calls: tc-1=read_file"
             , Event now EPolicyDecision "read_file: Allow"
             , Event now EToolCall "tc-1 read_file {\"path\":\"x.hs\"}"
             , Event now EToolResult "tc-1 file contents"
             , Event now EAssistantReply "done"
             , Event now ESessionEnd "ended"
             ]
  withJsonlFile root evts $ \dir -> do
    result <- loadResumeEvents dir
    case result of
      Nothing -> pure (Left "Expected Just")
      Just rr -> do
        let conv = rrResumeContext rr
            contents = map msgContent conv
            hasExecutableToolCalls = any (\m -> msgToolCalls m /= Nothing) conv
            hasToolResultMessages = any (\m -> msgCallId m /= Nothing) conv
        if contents == ["read the file", "let me read | tool_calls: tc-1=read_file", "done"]
           && not hasExecutableToolCalls
           && not hasToolResultMessages
           && rrSkipped rr == 5
          then pure (Right ())
          else pure (Left $ "contents=" ++ show contents
                         ++ " executableToolCalls=" ++ show hasExecutableToolCalls
                         ++ " toolResultMessages=" ++ show hasToolResultMessages
                         ++ " skipped=" ++ show (rrSkipped rr))

-- | resumeContextEventsToConversation respects EConversationReset boundary.
testResumeRespectsConversationReset :: Test
testResumeRespectsConversationReset = do
  now <- getCurrentTime
  let evts = [ Event now EUserMessage "before reset"
             , Event now EAssistantReply "reply before"
             , Event now EConversationReset "reset"
             , Event now EUserMessage "after reset"
             , Event now EAssistantReply "reply after"
             ]
      conv = resumeContextEventsToConversation evts
      contents = map msgContent conv
  if contents == ["after reset", "reply after"]
    then pure $ Right ()
    else pure $ Left $ "Contents after reset: " ++ show contents

-- | resumeContextEventsToConversation treats compaction as a safe memory boundary.
testResumeRespectsConversationCompaction :: Test
testResumeRespectsConversationCompaction = do
  now <- getCurrentTime
  let evts = [ Event now EUserMessage "before compact"
             , Event now EAssistantReply "reply before"
             , Event now EToolCall "tc-1 read_file"
             , Event now EToolResult "tc-1 contents"
             , Event now EConversationCompacted "compact memory"
             ]
      conv = resumeContextEventsToConversation evts
  case conv of
    [msg]
      | msgRole msg == System
        && "compact memory" `T.isInfixOf` msgContent msg
        && msgCallId msg == Nothing
        && msgToolCalls msg == Nothing ->
          pure $ Right ()
    _ -> pure $ Left $ "Compaction resume context: " ++ show conv

-- | loadResumeEvents reports and applies accepted compaction boundaries.
testResumeLoadConversationCompactionBoundary :: Test
testResumeLoadConversationCompactionBoundary = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-resume-compaction-boundary"
  now <- getCurrentTime
  let evts = [ Event now EUserMessage "before compact"
             , Event now EAssistantReply "reply before"
             , Event now EConversationCompacted "compact memory"
             , Event now EUserMessage "after compact"
             , Event now EAssistantReply "reply after"
             ]
  withJsonlFile root evts $ \dir -> do
    result <- loadResumeEvents dir
    case result of
      Nothing -> pure (Left "Expected Just")
      Just rr -> do
        let conv = rrResumeContext rr
            roles = map msgRole conv
            contents = map msgContent conv
        case contents of
          memory : rest
            | rrUsedCompactionBoundary rr
              && roles == [System, User, Assistant]
              && length conv == 3
              && "compact memory" `T.isInfixOf` memory
              && rest == ["after compact", "reply after"] ->
                pure (Right ())
          _ -> pure (Left $ "compaction boundary resume: roles=" ++ show roles
                          ++ " contents=" ++ show contents
                          ++ " used=" ++ show (rrUsedCompactionBoundary rr))

-- | loadResumeEvents produces correct safe text context from a real JSONL file.
testResumeEndToEnd :: Test
testResumeEndToEnd = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-resume-e2e"
  now <- getCurrentTime
  let evts = [ Event now ESessionStart "started"
             , Event now EUserMessage "what is 2+2"
             , Event now EAssistantReply "The answer is 4."
             , Event now EUserMessage "thanks"
             , Event now EAssistantReply "You're welcome!"
             , Event now ESessionEnd "ended"
             ]
  withJsonlFile root evts $ \dir -> do
    result <- loadResumeEvents dir
    case result of
      Nothing -> pure (Left "Expected Just for valid file")
      Just rr -> do
        let conv = rrResumeContext rr
            contents = map msgContent conv
            roles = map msgRole conv
        if contents == ["what is 2+2", "The answer is 4.", "thanks", "You're welcome!"]
           && roles == [User, Assistant, User, Assistant]
           && rrMessageEventCount rr == 4
          then pure (Right ())
          else pure (Left ("e2e: contents=" ++ show contents
                           ++ " roles=" ++ show roles
                           ++ " count=" ++ show (rrMessageEventCount rr)))

-- ---------------------------------------------------------------------------
-- Resume accounting tests (phase zero polish)
-- ---------------------------------------------------------------------------

-- | ResumeResult counts parsed valid events (all event types).
testResumeParsedValidCount :: Test
testResumeParsedValidCount = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-resume-parsed-valid"
  now <- getCurrentTime
  let evts = [ Event now ESessionStart "started"
             , Event now EUserMessage "hello"
             , Event now EAssistantReply "hi"
             , Event now EPolicyDecision "Allow"
             , Event now EToolCall "tc-1 read_file"
             , Event now EToolResult "tc-1 contents"
             , Event now ESessionEnd "ended"
             ]
  withJsonlFile root evts $ \dir -> do
    result <- loadResumeEvents dir
    case result of
      Nothing -> pure (Left "Expected Just")
      Just rr ->
        if rrParsedValid rr == 7
          then pure (Right ())
          else pure (Left $ "Expected 7 parsed valid events, got " ++ show (rrParsedValid rr))

-- | ResumeResult counts skipped events (valid events outside safe text context).
testResumeSkippedEventCount :: Test
testResumeSkippedEventCount = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-resume-skipped"
  now <- getCurrentTime
  let evts = [ Event now ESessionStart "started"
             , Event now EUserMessage "hello"
             , Event now EAssistantReply "hi"
             , Event now EPolicyDecision "Allow"
             , Event now EToolCall "tc-1 read_file"
             , Event now EToolResult "tc-1 contents"
             , Event now EConversationReset "reset"
             , Event now EUserMessage "after"
             , Event now EAssistantReply "reply"
             , Event now ESessionEnd "ended"
             ]
  withJsonlFile root evts $ \dir -> do
    result <- loadResumeEvents dir
    case result of
      Nothing -> pure (Left "Expected Just")
      Just rr ->
        -- 10 valid events total
        -- Message events: 2 user + 2 assistant = 4
        -- Skipped: session_start + policy_decision + tool_call + tool_result + session_end = 5
        if rrParsedValid rr == 10 && rrSkipped rr == 5 && rrMessageEventCount rr == 4
          then pure (Right ())
          else pure (Left $ "parsedValid=" ++ show (rrParsedValid rr)
                           ++ " skipped=" ++ show (rrSkipped rr)
                           ++ " messageEvents=" ++ show (rrMessageEventCount rr))

-- | ResumeResult detects conversation_reset boundary.
testResumeResetBoundaryDetected :: Test
testResumeResetBoundaryDetected = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-resume-reset-boundary"
  now <- getCurrentTime
  let evts = [ Event now EUserMessage "before"
             , Event now EAssistantReply "reply before"
             , Event now EConversationReset "reset"
             , Event now EUserMessage "after"
             , Event now EAssistantReply "reply after"
             ]
  withJsonlFile root evts $ \dir -> do
    result <- loadResumeEvents dir
    case result of
      Nothing -> pure (Left "Expected Just")
      Just rr ->
        if rrUsedResetBoundary rr
          then pure (Right ())
          else pure (Left "Expected reset boundary to be detected")

-- | ResumeResult reports no reset boundary when none exists.
testResumeNoResetBoundary :: Test
testResumeNoResetBoundary = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-resume-no-reset"
  now <- getCurrentTime
  let evts = [ Event now EUserMessage "hello"
             , Event now EAssistantReply "hi"
             ]
  withJsonlFile root evts $ \dir -> do
    result <- loadResumeEvents dir
    case result of
      Nothing -> pure (Left "Expected Just")
      Just rr ->
        if not (rrUsedResetBoundary rr)
          then pure (Right ())
          else pure (Left "Expected no reset boundary")

-- | ResumeResult counts safe text resume-context messages.
testResumeMessageCount :: Test
testResumeMessageCount = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-resume-msg-count"
  now <- getCurrentTime
  let evts = [ Event now EUserMessage "q1"
             , Event now EAssistantReply "a1"
             , Event now EUserMessage "q2"
             , Event now EAssistantReply "a2"
             ]
  withJsonlFile root evts $ \dir -> do
    result <- loadResumeEvents dir
    case result of
      Nothing -> pure (Left "Expected Just")
      Just rr ->
        if rrMessageCount rr == 4
          then pure (Right ())
          else pure (Left $ "Expected 4 messages, got " ++ show (rrMessageCount rr))

-- | ResumeResult message count respects reset boundary.
testResumeMessageCountAfterReset :: Test
testResumeMessageCountAfterReset = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-resume-msg-count-reset"
  now <- getCurrentTime
  let evts = [ Event now EUserMessage "before1"
             , Event now EAssistantReply "reply1"
             , Event now EConversationReset "reset"
             , Event now EUserMessage "after1"
             , Event now EAssistantReply "reply2"
             ]
  withJsonlFile root evts $ \dir -> do
    result <- loadResumeEvents dir
    case result of
      Nothing -> pure (Left "Expected Just")
      Just rr ->
        -- Only 2 messages after the reset boundary
        if rrMessageCount rr == 2
          then pure (Right ())
          else pure (Left $ "Expected 2 messages after reset, got " ++ show (rrMessageCount rr))

-- | ResumeResult with malformed lines counts them separately.
testResumeAccountingWithMalformed :: Test
testResumeAccountingWithMalformed = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-resume-malformed-acct"
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  createDirectory root
  now <- getCurrentTime
  -- Write 2 valid events and 1 malformed line
  let validLog = foldl (\acc e -> logEvent e acc) emptyLog
        [ Event now EUserMessage "hello"
        , Event now EAssistantReply "hi"
        ]
  flushLog root 0 validLog
  let path = root </> "session.jsonl"
  LBS.appendFile path "bad json line\n"
  result <- loadResumeEvents root
  _ <- try (removeDirectoryRecursive root) :: IO (Either IOException ())
  case result of
    Nothing -> pure (Left "Expected Just")
    Just rr ->
      if rrParsedValid rr == 2
         && rrMalformed rr == 1
         && rrMessageEventCount rr == 2
         && rrSkipped rr == 0
         && rrMessageCount rr == 2
        then pure (Right ())
        else pure (Left $ "parsedValid=" ++ show (rrParsedValid rr)
                         ++ " malformed=" ++ show (rrMalformed rr)
                         ++ " messageEvents=" ++ show (rrMessageEventCount rr)
                         ++ " skipped=" ++ show (rrSkipped rr)
                         ++ " messages=" ++ show (rrMessageCount rr))

-- | formatResumeSummary includes key sections.
testFormatResumeSummarySections :: Test
testFormatResumeSummarySections = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-resume-format"
  now <- getCurrentTime
  let evts = [ Event now ESessionStart "started"
             , Event now EUserMessage "hello"
             , Event now EAssistantReply "hi"
             , Event now EConversationReset "reset"
             , Event now EUserMessage "after"
             , Event now EAssistantReply "reply"
             , Event now ESessionEnd "ended"
             ]
  withJsonlFile root evts $ \dir -> do
    result <- loadResumeEvents dir
    case result of
      Nothing -> pure (Left "Expected Just")
      Just rr -> do
        let logPath = dir </> "session.jsonl"
            out = formatResumeSummary logPath rr
        if T.isInfixOf "Resumed from:" out
           && T.isInfixOf "Messages:" out
           && T.isInfixOf "Valid events:" out
           && T.isInfixOf "Message events:" out
           && T.isInfixOf "Skipped:" out
           && T.isInfixOf "Malformed:" out
           && T.isInfixOf "Reset boundary: yes" out
          then pure (Right ())
          else pure (Left $ "formatResumeSummary missing sections: " ++ T.unpack (T.take 400 out))

-- | formatResumeSummary shows reset boundary: no when no reset.
testFormatResumeSummaryNoResetBoundary :: Test
testFormatResumeSummaryNoResetBoundary = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-resume-format-noreset"
  now <- getCurrentTime
  let evts = [ Event now EUserMessage "hello"
             , Event now EAssistantReply "hi"
             ]
  withJsonlFile root evts $ \dir -> do
    result <- loadResumeEvents dir
    case result of
      Nothing -> pure (Left "Expected Just")
      Just rr -> do
        let logPath = dir </> "session.jsonl"
            out = formatResumeSummary logPath rr
        if T.isInfixOf "Reset boundary: no" out
          then pure (Right ())
          else pure (Left $ "Expected 'Reset boundary: no': " ++ T.unpack (T.take 300 out))

-- | formatResumeSummary reports accepted compaction boundaries.
testFormatResumeSummaryCompactionBoundary :: Test
testFormatResumeSummaryCompactionBoundary = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-resume-format-compact"
  now <- getCurrentTime
  let evts = [ Event now EUserMessage "before"
             , Event now EConversationCompacted "compact memory"
             ]
  withJsonlFile root evts $ \dir -> do
    result <- loadResumeEvents dir
    case result of
      Nothing -> pure (Left "Expected Just")
      Just rr -> do
        let logPath = dir </> "session.jsonl"
            out = formatResumeSummary logPath rr
        if T.isInfixOf "Compact boundary: yes" out
          then pure (Right ())
          else pure (Left $ "Expected compaction boundary summary: " ++ T.unpack (T.take 300 out))

-- | formatResumeSummary shows correct message count.
testFormatResumeSummaryMessageCount :: Test
testFormatResumeSummaryMessageCount = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-resume-format-msgcount"
  now <- getCurrentTime
  let evts = [ Event now EUserMessage "q1"
             , Event now EAssistantReply "a1"
             , Event now EUserMessage "q2"
             , Event now EAssistantReply "a2"
             ]
  withJsonlFile root evts $ \dir -> do
    result <- loadResumeEvents dir
    case result of
      Nothing -> pure (Left "Expected Just")
      Just rr -> do
        let logPath = dir </> "session.jsonl"
            out = formatResumeSummary logPath rr
        if T.isInfixOf "Messages:       4" out
          then pure (Right ())
          else pure (Left $ "Expected 'Messages:       4': " ++ T.unpack (T.take 300 out))

-- | Resume with zero safe text messages reports clearly.
testResumeZeroMessages :: Test
testResumeZeroMessages = do
  tmpDir <- getTemporaryDirectory
  let root = tmpDir </> "haskode-resume-zero-msgs"
  now <- getCurrentTime
  -- Only lifecycle events - no safe text message events
  let evts = [ Event now ESessionStart "started"
             , Event now ESessionEnd "ended"
             ]
  withJsonlFile root evts $ \dir -> do
    result <- loadResumeEvents dir
    case result of
      Nothing -> pure (Left "Expected Just (file exists with valid events)")
      Just rr ->
        if rrMessageCount rr == 0
           && rrMessageEventCount rr == 0
           && rrParsedValid rr == 2
           && rrSkipped rr == 2
          then pure (Right ())
          else pure (Left $ "messages=" ++ show (rrMessageCount rr)
                           ++ " messageEvents=" ++ show (rrMessageEventCount rr)
                           ++ " parsedValid=" ++ show (rrParsedValid rr)
                           ++ " skipped=" ++ show (rrSkipped rr))

tests :: [Test]
tests =
  [ testMultiToolResultOrder
  , testMultiToolAssistantPreservesCalls
  , testMultiToolCallIdCorrespondence
  , testShellTruncationMetaNewFormat
  , testShellShortOutputNoTruncMeta
  , testSessionEventAssistantReplyWithToolCalls
  , testSessionEventAssistantReplyTextOnly
  , testSessionEventToolCallData
  , testSessionEventToolResultData
  , testSessionEventChronologicalOrder
  , testMultiToolThenShellSessionEvents
  , testSessionEventDenyData
  , testSessionEventRejectData
  , testFlushLogWritesJsonl
  , testFlushLogEmpty
  , testFlushLogAppends
  , testFlushLogAgentIntegration
  , testFlushLogOnExceptionNoFlushOnSuccess
  , testFlushLogOnExceptionFlushesOnThrow
  , testFlushLogOnExceptionEmptyLog
  , testFlushLogRotationUnderLimit
  , testFlushLogRotationOverLimit
  , testFlushLogRotationBackupReplaced
  , testFlushLogRotationEmptySession
  , testFlushLogRotationValidJsonl
  , testSummarizeSessionMissingFile
  , testSummarizeSessionValidJsonl
  , testSummarizeSessionMalformedLines
  , testSummarizeSessionEmptyFile
  , testSummarizeSessionIgnoresBackup
  , testAgentUnchangedWithSessionSummary
  , testFormatSessionSummaryContainsSections
  , testConversationResetJSON
  , testSummarizeSessionConversationReset
  , testFormatSessionSummaryConversationReset
  , testConversationCompactedJSON
  , testSummarizeSessionConversationCompacted
  , testFormatSessionSummaryConversationCompacted
  , testRecordSessionStartEvent
  , testRecordSessionStartWithResumeEvent
  , testRecordSessionEndEvent
  , testRecordConversationResetEvent
  , testNewPreservesSessionAndAppendsReset
  , testFullLifecycleEventSequence
  , testIsMeaningfulEmpty
  , testIsMeaningfulLifecycleOnly
  , testIsMeaningfulWithReset
  , testIsMeaningfulWithCompaction
  , testIsMeaningfulWithUserMessage
  , testIsMeaningfulWithAssistantReply
  , testIsMeaningfulWithToolCall
  , testIsMeaningfulWithToolResult
  , testIsMeaningfulWithPolicyDecision
  , testIsMeaningfulFullLifecycle
  , testFlushLogOnExceptionLifecycleOnly
  , testFormatSessionSummaryMissingLog
  , testFormatSessionSummaryMalformedZero
  , testFormatSessionSummaryMalformedNonZero
  , testSummarizeSessionBackupExists
  , testSummarizeSessionBackupAbsent
  , testFormatSessionSummaryBackupExists
  -- Resume tests
  , testResumeIncludesUserMessages
  , testResumeIncludesAssistantReplies
  , testResumeIgnoresLifecycleEvents
  , testResumeIgnoresPolicyDecisions
  , testResumeDoesNotReconstructToolCalls
  , testResumeLoadMissingFile
  , testResumeLoadEmptyFile
  , testResumeSkipsMalformedLines
  , testResumeCountsOnlySafeTextMessages
  , testResumeLoadsOnlySafeTextContext
  , testResumeRespectsConversationReset
  , testResumeRespectsConversationCompaction
  , testResumeLoadConversationCompactionBoundary
  , testResumeEndToEnd
  -- Resume accounting tests (phase zero polish)
  , testResumeParsedValidCount
  , testResumeSkippedEventCount
  , testResumeResetBoundaryDetected
  , testResumeNoResetBoundary
  , testResumeMessageCount
  , testResumeMessageCountAfterReset
  , testResumeAccountingWithMalformed
  , testFormatResumeSummarySections
  , testFormatResumeSummaryNoResetBoundary
  , testFormatResumeSummaryCompactionBoundary
  , testFormatResumeSummaryMessageCount
  , testResumeZeroMessages
  ]
