module LspRecorder.Replay
  ( ReplayConfig (..)
  , runReplay
  ) where

import Control.Concurrent (threadDelay)
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
import Data.Time (UTCTime, diffUTCTime, getCurrentTime)
import LspRecorder.Lsp.Framing (encodeFrame, readFramedMessages)
import LspRecorder.Lsp.Types (Direction (..), TraceHeader, TraceMessage (..))
import LspRecorder.Replay.Report (ReplayReport (..), generateReport, writeReport)
import LspRecorder.Replay.Timing (TimingStrategy (..))
import LspRecorder.Server (cleanupServer, spawnServer)
import System.IO (BufferMode (..), Handle, hPutStrLn, hSetBuffering, stderr)

data ReplayConfig = ReplayConfig
  { rcTrace :: FilePath
  , rcServerCommand :: String
  , rcTiming :: TimingStrategy
  , rcTimingModeName :: Text
  , rcReportPath :: FilePath
  }

-- | Map from request id -> (method name, TMVar that receives the server response time)
type PendingMap = Map Text (Text, TMVar UTCTime)

runReplay :: ReplayConfig -> IO ()
runReplay ReplayConfig{rcTrace, rcServerCommand, rcTiming, rcTimingModeName, rcReportPath} = do
  (_header, messages) <- parseTrace rcTrace

  let clientMsgs = filter (\m -> tmDirection m == ClientToServer) messages

  pendingRef <- newTVarIO (Map.empty :: PendingMap)
  latenciesRef <- newTVarIO ([] :: [(Text, Double)])

  bracket (spawnServer rcServerCommand) cleanupServer $ \(serverIn, serverOut, _ph) -> do
    hSetBuffering serverIn NoBuffering
    hSetBuffering serverOut NoBuffering

    startTime <- getCurrentTime

    readerThread <-
      async $
        readFramedMessages serverOut (resolveResponse pendingRef)
          `catch` \(_ :: SomeException) -> pure ()

    asyncHandles <- replayMessages serverIn clientMsgs rcTiming pendingRef latenciesRef

    threadDelay 500_000

    mapConcurrently_ wait asyncHandles
    wait readerThread `catch` \(_ :: SomeException) -> pure ()

    endTime <- getCurrentTime
    let totalMs = round (realToFrac (diffUTCTime endTime startTime) * 1000 :: Double)

    latencies <- readTVarIO latenciesRef
    let report =
          ReplayReport
            { rrTrace = rcTrace
            , rrTimingMode = rcTimingModeName
            , rrTotalDurationMs = totalMs
            , rrMethods = generateReport latencies
            }

    writeReport rcReportPath report

replayMessages
  :: Handle
  -> [TraceMessage]
  -> TimingStrategy
  -> TVar PendingMap
  -> TVar [(Text, Double)]
  -> IO [Async ()]
replayMessages serverIn msgs timing pendingRef latenciesRef = go msgs 0 []
 where
  go [] _ acc = pure acc
  go (m : rest) prevUs acc = do
    let delay = tsComputeDelay timing prevUs (tmTimestampUs m)
    when (delay > 0) $ threadDelay delay

    let payload = BL.toStrict $ encode (tmMessage m)
        method = extractMethod (tmMessage m)
        msgId = extractId (tmMessage m)

    newAcc <- case (method, msgId) of
      (Just meth, Just rid) -> do
        var <- atomically newEmptyTMVar
        sendTs <- getCurrentTime
        atomically $ modifyTVar' pendingRef (Map.insert rid (meth, var))
        BS.hPut serverIn (encodeFrame payload)
        h <- async $ do
          recvTs <- atomically $ takeTMVar var
          let latencyMs = realToFrac (diffUTCTime recvTs sendTs) * 1000
          atomically $ modifyTVar' latenciesRef ((meth, latencyMs) :)
        pure (h : acc)
      _ -> do
        BS.hPut serverIn (encodeFrame payload)
        pure acc

    go rest (tmTimestampUs m) newAcc

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

parseTrace :: FilePath -> IO (TraceHeader, [TraceMessage])
parseTrace path = do
  contents <- BC.readFile path
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

extractMethod :: Value -> Maybe Text
extractMethod (Object o) = case KM.lookup (Key.fromText "method") o of
  Just (String t) -> Just t
  _ -> Nothing
extractMethod _ = Nothing

extractId :: Value -> Maybe Text
extractId (Object o) = case KM.lookup (Key.fromText "id") o of
  Just (String t) -> Just t
  Just (Number n) -> Just (T.pack $ show n)
  _ -> Nothing
extractId _ = Nothing
