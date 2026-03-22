-- | The Record mode of lsp-recorder.
--
-- Sits as a transparent stdio proxy between an editor and a language server,
-- capturing every LSP message into a timestamped JSONL trace file. The trace
-- file begins with a 'TraceHeader' (line 1) followed by one 'TraceMessage' per
-- LSP frame. After the session ends, the header is back-filled with
-- 'ServerInfo' extracted from the server's @initialize@ response.
module LspRecorder.Record
  ( runRecord
  , timestampUs
  , decodePayload
  , backfillHeader
  ) where

import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (TBQueue, atomically, newTBQueueIO, readTBQueue, writeTBQueue)
import Control.Exception (SomeException, bracket, catch)
import Data.Aeson (Value (..), decodeStrict, encode)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import LspRecorder.Config (RecordConfig (..))
import LspRecorder.Env (extractServerInfo, getOsInfo)
import LspRecorder.Lsp.Types
  ( Direction (..)
  , LogEntry (..)
  , ServerInfo
  , TraceHeader (..)
  , TraceMessage (..)
  )
import LspRecorder.Proxy (ProxyConfig (..), runProxy)
import LspRecorder.Server (cleanupServer, spawnServer)
import LspRecorder.Snapshot (takeSnapshot)
import System.OsPath (OsPath, decodeFS, encodeFS, takeBaseName, takeDirectory, (</>))
import System.IO
  ( BufferMode (..)
  , Handle
  , IOMode (..)
  , hClose
  , hPutStrLn
  , hSetBuffering
  , stderr
  , stdin
  , stdout
  )
import System.IO qualified as IO
import System.Posix.Signals (Handler (..), installHandler, sigPIPE, sigTERM)

-- | Run a recording session.
--
-- 1. Spawns the language server as a subprocess.
-- 2. Starts the bidirectional proxy ('runProxy') that forwards LSP frames
--    between editor stdio and the server, teeing each message into a bounded
--    STM queue.
-- 3. A background logging thread drains the queue and writes 'TraceMessage'
--    entries to the trace file.
-- 4. On shutdown (proxy exits or exception), sends a @Nothing@ sentinel to
--    stop the logging thread, then back-fills the trace header with
--    'ServerInfo' if one was captured from the server's @initialize@ response.
runRecord :: RecordConfig -> IO ()
runRecord RecordConfig{rcServerCommand, rcTraceOut, rcProjectRoot, rcSnapshot} = do
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

  now <- getCurrentTime
  osInfo <- getOsInfo

  -- Gracefully handle SIGTERM; ignore SIGPIPE so broken-pipe doesn't crash us.
  _ <- installHandler sigTERM (CatchOnce $ pure ()) Nothing
  _ <- installHandler sigPIPE Ignore Nothing

  traceOutStr <- decodeFS rcTraceOut
  projectRootStr <- decodeFS rcProjectRoot

  hPutStrLn stderr $ "[lsp-recorder] starting session: server=" <> rcServerCommand <> " trace=" <> traceOutStr

  -- Take project snapshot before starting the proxy, if configured.
  mSnapshotPath <- case rcSnapshot of
    Nothing -> pure Nothing
    Just snapCfg -> do
      archivePath <- snapshotArchivePath rcTraceOut
      takeSnapshot snapCfg rcProjectRoot archivePath
      archivePathStr <- decodeFS archivePath
      hPutStrLn stderr $ "[lsp-recorder] snapshot written to " <> archivePathStr
      pure (Just archivePathStr)

  bracket (spawnServer Nothing rcServerCommand) cleanupServer $ \(serverIn, serverOut, _ph) -> do
    hSetBuffering serverIn NoBuffering
    hSetBuffering serverOut NoBuffering
    hPutStrLn stderr "[lsp-recorder] server spawned"

    queue <- newTBQueueIO 1024
    serverInfoRef <- newIORef Nothing

    let header =
          TraceHeader
            { thTraceVersion = 1
            , thRecordedAt = now
            , thServerCommand = T.pack rcServerCommand
            , thProjectRoot = projectRootStr
            , thServerInfo = Nothing
            , thOs = osInfo
            , thSnapshotPath = mSnapshotPath
            }

    bracket (IO.openFile traceOutStr WriteMode) hClose $ \traceHandle -> do
      hSetBuffering traceHandle (BlockBuffering Nothing)
      BC.hPutStrLn traceHandle (BL.toStrict $ encode header)

      logThread <- async $ runLoggingThread traceHandle queue serverInfoRef 0

      let proxyCfg =
            ProxyConfig
              { pcServerIn = serverIn
              , pcServerOut = serverOut
              , pcEditorIn = stdin
              , pcEditorOut = stdout
              , pcLogQueue = queue
              }
      runProxy proxyCfg `catch` \(e :: SomeException) ->
        hPutStrLn stderr $ "[lsp-recorder] proxy exception: " <> show e

      -- Signal the logging thread to drain and exit.
      atomically $ writeTBQueue queue Nothing
      msgCount <- wait logThread
      hPutStrLn stderr $ "[lsp-recorder] session ended, " <> show msgCount <> " messages recorded"

      -- Back-fill the trace header with server info if we captured it.
      mServerInfo <- readIORef serverInfoRef
      case mServerInfo of
        Nothing -> hPutStrLn stderr "[lsp-recorder] server info not captured (no initialize response seen)"
        Just si -> do
          backfillHeader rcTraceOut header{thServerInfo = Just si}
          hPutStrLn stderr "[lsp-recorder] server info backfilled"

-- | Derive the snapshot archive path from the trace file path.
-- e.g. @"session.jsonl"@ → @"session.snapshot.tar.zst"@
snapshotArchivePath :: OsPath -> IO OsPath
snapshotArchivePath tracePath = do
  suffix <- encodeFS ".snapshot.tar.zst"
  pure $ takeDirectory tracePath </> (takeBaseName tracePath <> suffix)

-- | Logging thread loop. Drains 'LogEntry' values from the bounded queue,
-- converts each to a 'TraceMessage' with a monotonic sequence number, and
-- writes it as a JSON line. Also watches server-to-client messages for the
-- @initialize@ response so we can capture 'ServerInfo'. Terminates when it
-- reads the @Nothing@ sentinel.
runLoggingThread :: Handle -> TBQueue (Maybe LogEntry) -> IORef (Maybe ServerInfo) -> Int -> IO Int
runLoggingThread traceHandle queue serverInfoRef seqNum = do
  mEntry <- atomically $ readTBQueue queue
  case mEntry of
    Nothing -> pure seqNum
    Just entry -> do
      let ts = timestampUs (leTimestamp entry)
          msg = decodePayload (lePayload entry)
          traceMsg =
            TraceMessage
              { tmSeq = seqNum
              , tmTimestampUs = ts
              , tmDirection = leDirection entry
              , tmRawLength = leRawLength entry
              , tmMessage = msg
              }
      BC.hPutStrLn traceHandle (BL.toStrict $ encode traceMsg)
      case leDirection entry of
        ServerToClient -> do
          mSi <- readIORef serverInfoRef
          case mSi of
            Just _ -> pure ()
            Nothing ->
              case extractServerInfo (lePayload entry) of
                Just si -> writeIORef serverInfoRef (Just si)
                Nothing -> pure ()
        ClientToServer -> pure ()
      runLoggingThread traceHandle queue serverInfoRef (seqNum + 1)

-- | Convert a 'UTCTime' to microseconds since the Unix epoch.
timestampUs :: UTCTime -> Int
timestampUs t = round $ utcTimeToPOSIXSeconds t * 1_000_000

-- | Attempt to decode a raw LSP payload as JSON, falling back to 'Null'.
decodePayload :: ByteString -> Value
decodePayload bs = case decodeStrict bs of
  Just v -> v
  Nothing -> Null

-- | Rewrite the first line of the trace file with an updated 'TraceHeader'.
-- Used after the session ends to fill in 'ServerInfo' that wasn't available
-- when the header was originally written.
backfillHeader :: OsPath -> TraceHeader -> IO ()
backfillHeader path newHeader = do
  pathStr <- decodeFS path
  contents <- BS.readFile pathStr
  let ls = BC.lines contents
  case ls of
    [] -> pure ()
    (_ : rest) ->
      let newFirstLine = BL.toStrict (encode newHeader)
          newContents = BC.unlines (newFirstLine : rest)
       in BS.writeFile pathStr newContents
