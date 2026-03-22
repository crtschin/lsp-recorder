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
  , parseTrace
  , rewritePayload
  ) where

import Control.Concurrent (threadDelay)
import System.Timeout (timeout)
import Control.Concurrent.Async (Async, async, mapConcurrently_, wait)
import Control.Concurrent.STM
  ( TMVar
  , TVar
  , atomically
  , modifyTVar'
  , newEmptyTMVar
  , newTVarIO
  , putTMVar
  , readTVarIO
  , takeTMVar
  )
import Control.Exception (SomeException, bracket, catch)
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
import LspRecorder.Lsp.Framing (encodeFrame, readFramedMessages)
import LspRecorder.Lsp.Types (Direction (..), TraceHeader (..), TraceMessage (..))
import LspRecorder.Replay.FileSync (DocState, applyFileEffect, newDocState)
import LspRecorder.Replay.Report (ReplayReport (..), generateReport, writeReport)
import LspRecorder.Replay.Timing (TimingStrategy (..))
import LspRecorder.Server (cleanupServer, spawnServer)
import LspRecorder.Snapshot (restoreSnapshot)
import System.Directory.OsPath
  ( createDirectoryIfMissing
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.IO (BufferMode (..), Handle, hClose, hPutStrLn, hSetBuffering, openTempFile, stderr)
import System.OsPath (OsPath, decodeFS, encodeFS)

-- | Configuration for a replay session.
data ReplayConfig = ReplayConfig
  { rcTrace :: OsPath
  -- ^ Path to the JSONL trace file produced by a previous recording session.
  , rcServerCommand :: String
  -- ^ Shell command used to launch the language server under test.
  , rcTiming :: TimingStrategy
  -- ^ Controls inter-message delays during replay.
  , rcTimingModeName :: Text
  -- ^ Human-readable name of the timing mode, written into the report.
  , rcReportPath :: OsPath
  -- ^ Path where the JSON latency report will be written.
  , rcTimeoutSeconds :: Int
  -- ^ Per-request timeout in seconds. 0 means no timeout.
  , rcNoRestore :: Bool
  -- ^ When 'True', skip snapshot extraction even if the trace has one.
  , rcNoFileSync :: Bool
  -- ^ When 'True', skip applying file changes to disk during replay.
  }

-- | In-flight requests awaiting a server response.
-- Maps request @id@ to the @method@ name and a 'TMVar' that the reader thread
-- fills with the response arrival timestamp.
type PendingMap = Map Text (Text, TMVar UTCTime)

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
runReplay ReplayConfig{rcTrace, rcServerCommand, rcTiming, rcTimingModeName, rcReportPath, rcTimeoutSeconds, rcNoRestore, rcNoFileSync} = do
  (header, messages) <- parseTrace rcTrace

  let clientMsgs = filter (\m -> tmDirection m == ClientToServer) messages
      mSnapshotStr = if rcNoRestore then Nothing else thSnapshotPath header

  mSnapshotPath <- traverse encodeFS mSnapshotStr
  oldRoot <- encodeFS (thProjectRoot header)

  pendingRef <- newTVarIO (Map.empty :: PendingMap)
  latenciesRef <- newTVarIO ([] :: [(Text, Double)])
  timeoutsRef <- newTVarIO (0 :: Int)

  mDocState <-
    if rcNoFileSync || rcNoRestore
      then pure Nothing
      else Just <$> newDocState

  withMaybeSnapshot mSnapshotPath oldRoot $ \(mcwd, rewriter) ->
    bracket (spawnServer mcwd rcServerCommand) cleanupServer $ \(serverIn, serverOut, _ph) -> do
      hSetBuffering serverIn NoBuffering
      hSetBuffering serverOut NoBuffering

      startTime <- getCurrentTime

      readerThread <-
        async $
          readFramedMessages serverOut (resolveResponse pendingRef)
            `catch` \(_ :: SomeException) -> pure ()

      asyncHandles <- replayMessages serverIn clientMsgs rcTiming pendingRef latenciesRef timeoutsRef rcTimeoutSeconds rewriter mDocState

      threadDelay 1_000_000

      mapConcurrently_ wait asyncHandles
      wait readerThread `catch` \(_ :: SomeException) -> pure ()

      endTime <- getCurrentTime
      let totalMs = round (realToFrac (diffUTCTime endTime startTime) * 1000 :: Double)

      latencies <- readTVarIO latenciesRef
      timedOut <- readTVarIO timeoutsRef
      rrTraceStr <- decodeFS rcTrace
      let report =
            ReplayReport
              { rrTrace = rrTraceStr
              , rrTimingMode = rcTimingModeName
              , rrTotalDurationMs = totalMs
              , rrTimedOut = timedOut
              , rrMethods = generateReport latencies
              }

      writeReport rcReportPath report

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
  -> (ByteString -> ByteString)
  -> Maybe DocState
  -> IO [Async ()]
replayMessages serverIn msgs timing pendingRef latenciesRef timeoutsRef timeoutSeconds rewriter mDocState = go msgs 0 []
 where
  go [] _ acc = pure acc
  go (m : rest) prevUs acc = do
    let delay = tsComputeDelay timing prevUs (tmTimestampUs m)
    when (delay > 0) $ threadDelay delay

    let payload = rewriter $ BL.toStrict $ encode (tmMessage m)
        method = extractMethod (tmMessage m)
        msgId = extractId (tmMessage m)

    newAcc <- case (method, msgId) of
      (Just meth, Just rid) -> do
        var <- atomically newEmptyTMVar
        sendTs <- getCurrentTime
        atomically $ modifyTVar' pendingRef (Map.insert rid (meth, var))
        BS.hPut serverIn (encodeFrame payload)
        h <- async $ do
          let waitAction = atomically $ takeTMVar var
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
        pure (h : acc)
      _ -> do
        case (mDocState, decodeStrict payload) of
          (Just ds, Just val) -> applyFileEffect ds val
          _ -> pure ()
        BS.hPut serverIn (encodeFrame payload)
        pure acc

    go rest (tmTimestampUs m) newAcc

-- | Called by the reader thread for each framed response from the server.
-- Decodes the payload, looks up its @id@ in 'PendingMap', and if found,
-- removes the entry and signals the waiting 'TMVar' with the current time.
-- Silently ignores malformed JSON and responses with no matching pending entry
-- (e.g. server-initiated notifications).
resolveResponse :: TVar PendingMap -> ByteString -> IO ()
resolveResponse pendingRef payload =
  case decodeStrict payload of
    Nothing -> pure ()
    Just val ->
      case extractId val of
        Nothing -> pure ()
        Just rid -> do
          pending <- readTVarIO pendingRef
          case Map.lookup rid pending of
            Nothing -> pure ()
            Just (_meth, var) -> do
              atomically $ modifyTVar' pendingRef (Map.delete rid)
              ts <- getCurrentTime
              atomically $ putTMVar var ts

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
