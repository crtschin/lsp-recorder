module LspRecorder.Record (
    RecordConfig (..),
    runRecord,
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
import LspRecorder.Env (extractServerInfo, getOsInfo)
import LspRecorder.Lsp.Types (
    Direction (..),
    LogEntry (..),
    ServerInfo,
    TraceHeader (..),
    TraceMessage (..),
 )
import LspRecorder.Proxy (ProxyConfig (..), runProxy)
import LspRecorder.Server (cleanupServer, spawnServer)
import System.IO (
    BufferMode (..),
    Handle,
    IOMode (..),
    hClose,
    hSetBuffering,
    stdin,
    stdout,
 )
import System.IO qualified as IO
import System.Posix.Signals (Handler (..), installHandler, sigPIPE, sigTERM)

data RecordConfig = RecordConfig
    { rcServerCommand :: String
    , rcTraceOut :: FilePath
    , rcProjectRoot :: FilePath
    }

runRecord :: RecordConfig -> IO ()
runRecord RecordConfig{rcServerCommand, rcTraceOut, rcProjectRoot} = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering

    now <- getCurrentTime
    osInfo <- getOsInfo

    _ <- installHandler sigTERM (CatchOnce $ pure ()) Nothing
    _ <- installHandler sigPIPE Ignore Nothing

    bracket (spawnServer rcServerCommand) cleanupServer $ \(serverIn, serverOut, _ph) -> do
        hSetBuffering serverIn NoBuffering
        hSetBuffering serverOut NoBuffering

        queue <- newTBQueueIO 1024
        serverInfoRef <- newIORef Nothing

        let header =
                TraceHeader
                    { thTraceVersion = 1
                    , thRecordedAt = now
                    , thServerCommand = T.pack rcServerCommand
                    , thProjectRoot = rcProjectRoot
                    , thServerInfo = Nothing
                    , thOs = osInfo
                    }

        bracket (IO.openFile rcTraceOut WriteMode) hClose $ \traceHandle -> do
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
            runProxy proxyCfg `catch` \(_ :: SomeException) -> pure ()

            atomically $ writeTBQueue queue Nothing
            wait logThread

            mServerInfo <- readIORef serverInfoRef
            case mServerInfo of
                Nothing -> pure ()
                Just si -> backfillHeader rcTraceOut header{thServerInfo = Just si}

runLoggingThread :: Handle -> TBQueue (Maybe LogEntry) -> IORef (Maybe ServerInfo) -> Int -> IO ()
runLoggingThread traceHandle queue serverInfoRef seqNum = do
    mEntry <- atomically $ readTBQueue queue
    case mEntry of
        Nothing -> pure ()
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

timestampUs :: UTCTime -> Int
timestampUs t = round $ utcTimeToPOSIXSeconds t * 1_000_000

decodePayload :: ByteString -> Value
decodePayload bs = case decodeStrict bs of
    Just v -> v
    Nothing -> Null

backfillHeader :: FilePath -> TraceHeader -> IO ()
backfillHeader path newHeader = do
    contents <- BS.readFile path
    let ls = BC.lines contents
    case ls of
        [] -> pure ()
        (_ : rest) ->
            let newFirstLine = BL.toStrict (encode newHeader)
                newContents = BC.unlines (newFirstLine : rest)
             in BS.writeFile path newContents
