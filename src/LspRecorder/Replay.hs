-- | The Replay mode of lsp-recorder.
--
-- Reads a previously recorded JSONL trace file, filters it down to the
-- client→server messages, and re-sends them to a fresh language server
-- subprocess. Timing between messages is controlled by a 'TimingStrategy'.
--
-- For each outgoing request (a message with both a @method@ and an @id@
-- field), the replay engine spawns an async task that waits for the
-- corresponding server response, then records the round-trip latency. After
-- all messages have been sent and responses collected, a JSON report is
-- written containing per-method p50/p95/p99 latency statistics.
--
-- = Concurrency model
--
-- Three concurrent actors cooperate via STM:
--
-- 1. __Main loop__ (@replayMessages@): sends client→server frames in order
--    with optional delays. For each tracked request, it inserts a @(method,
--    'TMVar')@ entry into 'PendingMap' before sending the frame, so the
--    reader thread can signal it on arrival.
--
-- 2. __Reader thread__ (@resolveResponse@): continuously reads framed
--    responses from the server. When it finds a response whose @id@ matches
--    an entry in 'PendingMap', it timestamps the arrival and signals the
--    corresponding 'TMVar'.
--
-- 3. __Per-request async tasks__: each tracked request spawns one task that
--    blocks on its 'TMVar', computes @recvTs - sendTs@, and appends the
--    latency to @latenciesRef@. All handles are awaited before writing the
--    report.
module LspRecorder.Replay
  ( ReplayConfig (..)
  , runReplay
  , extractMethod
  , extractId
  , extractIdValue
  , rewriteId
  , parseTrace
  , rewritePayload
  ) where

import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async, async, cancel, mapConcurrently_, wait)
import Control.Concurrent.STM
  ( TMVar
  , TQueue
  , TVar
  , atomically
  , modifyTVar'
  , newEmptyTMVar
  , newTQueueIO
  , newTVarIO
  , putTMVar
  , readTVar
  , readTVarIO
  , retry
  , takeTMVar
  , tryReadTQueue
  , writeTQueue
  )
import Control.Exception (bracket)
import Control.Monad (when)
import Data.Aeson (Value (..), decodeStrict, encode)
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import LspRecorder.Config (ExportConfig (..))
import LspRecorder.Lsp.Framing (encodeFrame, readFramedMessages)
import LspRecorder.Lsp.Types (Direction (..), TraceHeader (..), TraceMessage (..))
import LspRecorder.Replay.FileSync (DocState, applyFileEffect, newDocState)
import LspRecorder.Replay.Report (ReplayReport (..), generateReport, writeReport)
import LspRecorder.Replay.Timing (TimingStrategy (..))
import LspRecorder.Server (cleanupServer, spawnServer)
import LspRecorder.Snapshot (restoreSnapshot, snapshotArchivePath)
import System.Directory.OsPath
  ( copyFile
  , createDirectoryIfMissing
  , doesFileExist
  , getCurrentDirectory
  , getTemporaryDirectory
  , listDirectory
  , makeAbsolute
  , removeDirectoryRecursive
  , removeFile
  )
import System.FilePattern ((?==))
import System.IO (BufferMode (..), Handle, hClose, hPutStrLn, hSetBuffering, openTempFile, stderr)
import System.OsPath (OsPath, decodeFS, encodeFS, takeFileName, (</>))
import System.Timeout (timeout)

-- | Configuration for a replay session.
data ReplayConfig = ReplayConfig
  { rcTrace :: OsPath
  -- ^ Path to the JSONL trace file produced by a previous recording session.
  , rcServerCommand :: String
  -- ^ Shell command used to launch the language server under test.
  , rcTiming :: TimingStrategy
  -- ^ Controls inter-message delays during replay.
  , rcReplaySpeed :: Int
  -- ^ Factor of reduction between events in original recorded trace.
  , rcReportPath :: OsPath
  -- ^ Path where the JSON latency report will be written.
  , rcSpeedupFactor :: Int
  -- ^ Factor to reduce pauses between events in 'realistic'.
  , rcTimeoutSeconds :: Int
  -- ^ Per-request timeout in seconds. 0 means no timeout.
  , rcNoRestore :: Bool
  -- ^ When 'True', skip snapshot extraction even if the trace has one.
  , rcNoFileSync :: Bool
  -- ^ When 'True', skip applying file changes to disk during replay.
  , rcExport :: Maybe ExportConfig
  -- ^ When 'Just', copy matching files from server cwd to destination after replay.
  , rcCallerCwd :: OsPath
  -- ^ Working directory of the lsp-recorder process at startup, used as the
  -- default export destination when 'ecDestination' is absent.
  }

-- | In-flight requests awaiting a server response.
-- Maps request @id@ to the @method@ name and a 'TMVar' that the reader thread
-- fills with the response arrival timestamp.
type PendingMap = Map Text (Text, TMVar UTCTime)

-- | Pre-scan the trace for server→client requests (messages with both a method
-- and an id, direction ServerToClient). Returns a map from method name to a
-- 'TQueue' of the original ids in trace order. During replay, when the fresh
-- server sends a request for a given method, we pop the next original id from
-- the queue to build the id remap.
buildOrigServerReqs :: [TraceMessage] -> IO (Map Text (TQueue Text))
buildOrigServerReqs msgs = do
  let serverReqs =
        [ (meth, rid)
        | m <- msgs
        , tmDirection m == ServerToClient
        , Just meth <- [extractMethod (tmMessage m)]
        , Just rid <- [extractId (tmMessage m)]
        ]
  -- Group by method
  let grouped = Map.fromListWith (++) [(meth, [rid]) | (meth, rid) <- serverReqs]
  -- Create TQueues with ids in trace order
  Map.traverseWithKey
    ( \_ ids -> do
        q <- newTQueueIO
        mapM_ (atomically . writeTQueue q) ids
        pure q
    )
    grouped

-- | Run a replay session.
--
-- 1. Parses the trace file and filters to client→server messages.
-- 2. Optionally extracts the snapshot archive to a temp directory and rewrites
--    file paths in each message payload.
-- 3. Spawns the language server subprocess (with cwd set to the temp dir when
--    a snapshot was restored).
-- 4. Starts a reader thread that consumes server responses and resolves
--    pending request 'TMVar's.
-- 5. Sends client messages via 'replayMessages', which returns async handles
--    for all tracked request/response pairs.
-- 6. Waits 1 s for any in-flight responses to arrive, then awaits all async
--    handles and the reader thread.
-- 7. Writes the latency report to 'rcReportPath'.
runReplay :: ReplayConfig -> IO ()
runReplay
  ReplayConfig
    { rcTrace
    , rcServerCommand
    , rcTiming
    , rcReplaySpeed
    , rcReportPath
    , rcSpeedupFactor
    , rcTimeoutSeconds
    , rcNoRestore
    , rcNoFileSync
    , rcExport
    , rcCallerCwd
    } = do
    (header, messages) <- parseTrace rcTrace

    -- Strip any shutdown/exit from the trace — we always append our own at the
    -- end (after all latency measurements are complete) so HLS gets a clean exit.
    let isShutdownOrExit m =
          extractMethod (tmMessage m) `elem` [Just "shutdown", Just "exit"]
        clientMsgs =
          filter (\m -> tmDirection m == ClientToServer && not (isShutdownOrExit m)) messages

    mSnapshotPath <-
      if rcNoRestore || null (thSnapshotPath header)
        then pure Nothing
        else Just <$> snapshotArchivePath rcTrace
    oldRoot <- encodeFS (thProjectRoot header)

    pendingRef <- newTVarIO (Map.empty :: PendingMap)
    latenciesRef <- newTVarIO ([] :: [(Text, Double)])
    timeoutsRef <- newTVarIO (0 :: Int)
    origServerReqs <- buildOrigServerReqs messages
    idRemapRef <- newTVarIO (Map.empty :: Map Text Value)

    mDocState <-
      if rcNoFileSync || rcNoRestore
        then pure Nothing
        else Just <$> newDocState

    withMaybeSnapshot mSnapshotPath oldRoot $ \(mcwd, rewriter) -> do
      bracket (spawnServer mcwd rcServerCommand) cleanupServer $ \(serverIn, serverOut, _ph) -> do
        hSetBuffering serverIn NoBuffering
        hSetBuffering serverOut NoBuffering
        startTime <- getCurrentTime

        readerThread <- async $ readFramedMessages serverOut (handleServerMessage pendingRef origServerReqs idRemapRef)

        asyncHandles <-
          replayMessages
            serverIn
            clientMsgs
            rcTiming
            pendingRef
            latenciesRef
            timeoutsRef
            rcSpeedupFactor
            rcTimeoutSeconds
            rewriter
            mDocState
            idRemapRef

        when (not $ tsWaitForResponse rcTiming) $ threadDelay 1_000_000

        hPutStrLn stderr $ "[lsp-recorder] Waiting on " <> show (length asyncHandles) <> " events."
        mapConcurrently_ wait asyncHandles

        -- Send shutdown request and wait for the response before sending exit.
        shutdownVar <- atomically newEmptyTMVar
        let makeFrame = encodeFrame . BL.toStrict . encode . Object . KM.fromList
            shutdownId = "lsp-recorder-shutdown"
            shutdownFrame = makeFrame
              [ ("jsonrpc", String "2.0")
              , ("id", String shutdownId)
              , ("method", String "shutdown")
              , ("params", Null)
              ]
        atomically $ modifyTVar' pendingRef (Map.insert shutdownId ("shutdown", shutdownVar))
        BS.hPut serverIn shutdownFrame
        hPutStrLn stderr "[lsp-recorder] sent shutdown request"
        shutdownResult <- timeout (rcTimeoutSeconds * 1_000_000) $ atomically $ takeTMVar shutdownVar
        case shutdownResult of
          Nothing -> hPutStrLn stderr "[lsp-recorder] warning: shutdown response timed out"
          Just _ -> hPutStrLn stderr "[lsp-recorder] shutdown response received"

        -- Send exit notification so HLS terminates and prints RTS stats.
        let exitFrame = makeFrame [("jsonrpc", String "2.0"), ("method", String "exit")]
        BS.hPut serverIn exitFrame
        hPutStrLn stderr "[lsp-recorder] sent exit notification"

        -- Wait for the reader thread to finish (HLS closes stdout on exit).
        readerResult <- timeout (rcTimeoutSeconds * 1_000_000) $ wait readerThread
        case readerResult of
          Nothing -> do
            hPutStrLn stderr "[lsp-recorder] reader thread timed out, cancelling"
            cancel readerThread
          Just _ -> pure ()

        hPutStrLn stderr "[lsp-recorder] replay done."
        endTime <- getCurrentTime
        let totalMs = round (realToFrac (diffUTCTime endTime startTime) * 1000 :: Double)

        (latencies, timedOut) <- atomically $ (,) <$> readTVar latenciesRef <*> readTVar timeoutsRef
        rrTraceStr <- decodeFS rcTrace
        let report =
              ReplayReport
                { rrTrace = rrTraceStr
                , rrReplaySpeed = rcReplaySpeed
                , rrTotalDurationMs = totalMs
                , rrTimedOut = timedOut
                , rrMethods = generateReport latencies
                }

        writeReport rcReportPath report

      -- Export runs after cleanupServer so profiling files are fully written.
      case rcExport of
        Nothing -> pure ()
        Just cfg -> do
          serverCwd <- maybe getCurrentDirectory pure mcwd
          runExport cfg serverCwd rcCallerCwd

-- | Copy files matching 'ecGlobs' from @srcDir@ (the server's working
-- directory) into 'ecDestination' (or @callerCwd@ when absent). Only the
-- immediate files in @srcDir@ are scanned — profiling outputs (@.hp@,
-- @.prof@, @.eventlog@) are always written at the top level of the server cwd.
runExport :: ExportConfig -> OsPath -> OsPath -> IO ()
runExport ExportConfig{ecGlobs, ecDestination} srcDir callerCwd = do
  dest <- maybe (pure callerCwd) (\d -> encodeFS d >>= makeAbsolute) ecDestination
  createDirectoryIfMissing True dest
  entries <- listDirectory srcDir
  mapM_ (exportEntry srcDir dest) entries
 where
  exportEntry dir dst name = do
    let absPath = dir </> name
    isFile <- doesFileExist absPath
    when isFile $ do
      nameStr <- decodeFS (takeFileName name)
      when (any (?== nameStr) ecGlobs) $ do
        copyFile absPath (dst </> name)
        hPutStrLn stderr $ "[lsp-recorder] exported: " <> nameStr

-- | If @mArchivePath@ is 'Just', extract the snapshot to a fresh temp directory
-- and run the action with that directory as cwd and a path-rewriting function.
-- If 'Nothing', run the action immediately with no cwd override and identity rewriter.
withMaybeSnapshot
  :: Maybe OsPath
  -> OsPath
  -> ((Maybe OsPath, ByteString -> ByteString) -> IO a)
  -> IO a
withMaybeSnapshot Nothing _ action = action (Nothing, id)
withMaybeSnapshot (Just archivePath) oldRoot action =
  withSnapshotRestore archivePath $ \tmpDir -> do
    oldRootStr <- decodeFS oldRoot
    tmpDirStr <- decodeFS tmpDir
    action (Just tmpDir, rewritePayload (BC.pack oldRootStr) (BC.pack tmpDirStr))

-- | Create a temporary directory, extract @archivePath@ into it, run the
-- action, then remove the directory on exit (even on exception).
withSnapshotRestore :: OsPath -> (OsPath -> IO a) -> IO a
withSnapshotRestore archivePath action =
  bracket acquire cleanup action
 where
  acquire = do
    base <- getTemporaryDirectory
    baseStr <- decodeFS base
    (tmpFileStr, h) <- openTempFile baseStr "lsp-recorder-snapshot"
    hClose h
    tmpFile <- encodeFS tmpFileStr
    removeFile tmpFile
    dotD <- encodeFS ".d"
    let dir = tmpFile <> dotD
    createDirectoryIfMissing True dir
    restoreSnapshot archivePath dir
    pure dir
  cleanup = removeDirectoryRecursive

-- | Replace all occurrences of @oldRoot@ with @newRoot@ in a JSON payload.
-- Operates on the raw UTF-8 bytes so it catches every path occurrence
-- (bare paths, @file://@ URIs, @rootUri@, etc.) without needing to
-- understand the JSON structure.
rewritePayload :: ByteString -> ByteString -> ByteString -> ByteString
rewritePayload oldRoot newRoot payload =
  TE.encodeUtf8
    ( T.replace
        (TE.decodeUtf8 oldRoot)
        (TE.decodeUtf8 newRoot)
        (TE.decodeUtf8 payload)
    )

-- | Send a sequence of client→server messages to the server, respecting the
-- configured 'TimingStrategy'. Returns a list of async handles — one per
-- tracked request — each of which completes once the server response arrives
-- and the latency has been recorded.
--
-- A message is "tracked" if it has both a @method@ field and an @id@ field,
-- i.e. it is a JSON-RPC request (as opposed to a notification, which has no
-- @id@). Notifications are forwarded but not measured.
replayMessages
  :: Handle
  -> [TraceMessage]
  -> TimingStrategy
  -> TVar PendingMap
  -> TVar [(Text, Double)]
  -> TVar Int
  -> Int
  -> Int
  -> (ByteString -> ByteString)
  -> Maybe DocState
  -> TVar (Map Text Value)
  -> IO [Async ()]
replayMessages serverIn msgs timing pendingRef latenciesRef timeoutsRef speedupFactor timeoutSeconds rewriter mDocState idRemapRef =
  let initUs = case msgs of
        (m : _) -> tmTimestampUs m
        [] -> 0
   in go msgs initUs []
 where
  go [] _ acc = pure acc
  go (m : rest) prevUs acc = do
    let rawDelay = tsComputeDelay timing prevUs (tmTimestampUs m)
        delay = rawDelay `quot` speedupFactor
    when (delay > 0) $ threadDelay delay

    -- TODO: Deduplicate this with the id-writing where possible
    let payload = rewriter $ BL.toStrict $ encode (tmMessage m)
        method = extractMethod (tmMessage m)
        msgId = extractId (tmMessage m)

    newAcc <- case (method, msgId) of
      (Just meth, Just rid) -> do
        var <- atomically newEmptyTMVar
        sendTs <- getCurrentTime
        atomically $ modifyTVar' pendingRef (Map.insert rid (meth, var))
        hPutStrLn stderr $
          "[lsp-recorder] sending request: " <> T.unpack meth <> " (id=" <> T.unpack rid <> ")"
        BS.hPut serverIn (encodeFrame payload)
        let waitAction = atomically $ takeTMVar var
            doWait = do
              result <-
                if timeoutSeconds <= 0
                  then fmap Just waitAction
                  else timeout (timeoutSeconds * 1_000_000) waitAction
              case result of
                Just recvTs -> do
                  let latencyMs = realToFrac (diffUTCTime recvTs sendTs) * 1000
                  atomically $ modifyTVar' latenciesRef ((meth, latencyMs) :)
                Nothing -> do
                  atomically $ do
                    modifyTVar' pendingRef (Map.delete rid)
                    modifyTVar' timeoutsRef (+ 1)
                  hPutStrLn stderr $
                    "Warning: request timed out after "
                      <> show timeoutSeconds
                      <> "s: "
                      <> T.unpack meth
                      <> " (id="
                      <> T.unpack rid
                      <> ")"
        if tsWaitForResponse timing
          then do
            doWait
            pure acc
          else do
            h <- async doWait
            pure (h : acc)

      -- Client response to a server request (has id, no method): remap the id
      (Nothing, Just origId) -> do
        -- Wait up to 5s for the reader thread to process the corresponding server request
        remapped <- timeout 5_000_000 $ atomically $ do
          remap <- readTVar idRemapRef
          case Map.lookup origId remap of
            Nothing -> retry
            Just newIdVal -> do
              modifyTVar' idRemapRef (Map.delete origId)
              pure newIdVal

        case remapped of
          Just newIdVal -> do
            -- TODO: This is re-encoded, can probably be made more efficient and
            -- deduplicated with the initial one above.
            let rewritten = BL.toStrict $ encode $ rewriteId newIdVal (tmMessage m)
                rewrittenPayload = rewriter rewritten
            hPutStrLn stderr $ "[lsp-recorder] sending response (id=" <> T.unpack origId <> ")"
            BS.hPut serverIn (encodeFrame rewrittenPayload)
          Nothing -> do
            hPutStrLn stderr $
              "[lsp-recorder] warning: no id remap found for client response id="
                <> T.unpack origId
                <> ", sending as-is"
            BS.hPut serverIn (encodeFrame payload)
        pure acc

      -- Notification (has method, no id) or other
      _ -> do
        case (mDocState, decodeStrict payload) of
          (Just ds, Just val) -> applyFileEffect ds val
          _ -> pure ()
        hPutStrLn stderr $ "[lsp-recorder] sending notification: " <> maybe "unknown" T.unpack method
        BS.hPut serverIn (encodeFrame payload)
        pure acc

    go rest (tmTimestampUs m) newAcc

-- | Called by the reader thread for each framed message from the server.
-- Handles two kinds of messages:
--
-- * __Server response__ (has @id@, no @method@): resolves the matching entry
--   in 'PendingMap' by signalling the 'TMVar' with the current time.
--
-- * __Server request__ (has both @method@ and @id@): pops the next original
--   id for that method from 'origServerReqs' and inserts a mapping from the
--   original id to the new (fresh server) id into 'idRemapRef'. This allows
--   the main loop to rewrite client responses before forwarding them.
handleServerMessage
  :: TVar PendingMap
  -> Map Text (TQueue Text)
  -> TVar (Map Text Value)
  -> ByteString
  -> IO ()
handleServerMessage pendingRef origServerReqs idRemapRef payload =
  case decodeStrict payload of
    Nothing -> pure ()
    Just val -> do
      let method = extractMethod val
          msgId = extractId val
          idVal = extractIdValue val
      case (method, msgId, idVal) of
        -- Server request: has method and id
        (Just meth, _msgId, Just newIdVal) ->
          case Map.lookup meth origServerReqs of
            Nothing -> pure () -- unknown method, ignore
            Just q -> do
              mOrigId <- atomically $ tryReadTQueue q
              case mOrigId of
                Nothing -> pure () -- no more original ids for this method
                Just origId -> atomically $ modifyTVar' idRemapRef (Map.insert origId newIdVal)
        -- Server response: has id, no method
        (Nothing, Just rid, _) -> do
          pending <- readTVarIO pendingRef
          case Map.lookup rid pending of
            Nothing -> pure ()
            Just (_meth, var) -> do
              atomically $ modifyTVar' pendingRef (Map.delete rid)
              ts <- getCurrentTime
              atomically $ putTMVar var ts
        -- Server notification or other: ignore
        _ -> pure ()

-- | Read and parse a JSONL trace file.
--
-- The first line must be a valid 'TraceHeader'. Subsequent lines are parsed as
-- 'TraceMessage' entries; lines that fail to parse are counted and a warning
-- is printed to stderr, but they do not cause failure. This makes the parser
-- tolerant of truncated or partially-written traces.
--
-- Fails (via 'fail') if the file is empty or the header line cannot be parsed.
parseTrace :: OsPath -> IO (TraceHeader, [TraceMessage])
parseTrace path = do
  pathStr <- decodeFS path
  contents <- BC.readFile pathStr
  let ls = BC.lines contents
  case ls of
    [] -> fail "Empty trace file"
    (headerLine : msgLines) -> do
      header <- case decodeStrict headerLine of
        Just h -> pure h
        Nothing -> fail "Failed to parse trace header"
      let (msgs, badCount) = foldl' parseLine ([], 0 :: Int) msgLines
      when (badCount > 0) $
        hPutStrLn stderr $
          "Warning: skipped " <> show badCount <> " unparseable trace lines"
      pure (header, reverse msgs)
 where
  parseLine (acc, bad) line =
    case decodeStrict line of
      Just m -> (m : acc, bad)
      Nothing -> (acc, bad + 1)

-- | Extract the @method@ field from a JSON-RPC message value.
-- Returns 'Nothing' if the value is not an object, the field is absent, or
-- the field value is not a string.
extractMethod :: Value -> Maybe Text
extractMethod (Object o) = case KM.lookup (Key.fromText "method") o of
  Just (String t) -> Just t
  _ -> Nothing
extractMethod _ = Nothing

-- | Extract the @id@ field from a JSON-RPC message value as 'Text'.
-- String ids are returned as-is; numeric ids are rendered via 'show' on the
-- underlying 'Scientific' value (e.g. @42@ becomes @"42.0"@).
-- Returns 'Nothing' if the value is not an object, the field is absent, or
-- the field value is neither a string nor a number.
extractId :: Value -> Maybe Text
extractId (Object o) = case KM.lookup (Key.fromText "id") o of
  Just (String t) -> Just t
  Just (Number n) -> Just (T.pack $ show n)
  _ -> Nothing
extractId _ = Nothing

-- | Extract the @id@ field from a JSON-RPC message as a raw 'Value'.
-- Unlike 'extractId', this preserves the original JSON type (Number vs String)
-- so it can be written back without type coercion.
extractIdValue :: Value -> Maybe Value
extractIdValue (Object o) = case KM.lookup (Key.fromText "id") o of
  Just v@(String _) -> Just v
  Just v@(Number _) -> Just v
  _ -> Nothing
extractIdValue _ = Nothing

-- | Replace the @id@ field in a JSON-RPC message object with a new value.
-- Non-object values pass through unchanged.
rewriteId :: Value -> Value -> Value
rewriteId newId (Object o) = Object (KM.insert (Key.fromText "id") newId o)
rewriteId _ v = v
