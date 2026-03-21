module RecordSpec (spec) where

import Control.Exception (finally)
import Data.Aeson (Value (..), decodeStrict, encode, object, (.=))
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import LspRecorder.Env (extractServerInfo)
import LspRecorder.Lsp.Framing (encodeFrame)
import LspRecorder.Lsp.Types
  ( Direction (..)
  , ServerInfo (..)
  , TraceHeader (..)
  , TraceMessage (..)
  )
import LspRecorder.Record (backfillHeader, decodePayload, timestampUs)
import System.Directory (removeFile)
import System.IO
  ( BufferMode (..)
  , hClose
  , hFlush
  , hSetBuffering
  )
import System.Process
  ( CreateProcess (..)
  , StdStream (..)
  , createProcess
  , proc
  , waitForProcess
  )
import Test.Hspec (Spec, around_, describe, it, shouldBe, shouldSatisfy)

traceFile :: FilePath
traceFile = "/tmp/lsp-recorder-test.jsonl"

withTraceFile :: IO () -> IO ()
withTraceFile action = action `finally` removeFile traceFile

-- | A fixed epoch time for timestampUs tests.
epochTime :: UTCTime
epochTime = posixSecondsToUTCTime 0

-- | Spawn `lsp-recorder record --server-command cat`, send frames, wait for exit.
runRecordCat :: BC.ByteString -> IO ()
runRecordCat frames = do
  (Just sIn, _sOut, _sErr, ph) <-
    createProcess
      ( proc
          "cabal"
          [ "run"
          , "lsp-recorder"
          , "--"
          , "record"
          , "--server-command"
          , "cat"
          , "--trace-out"
          , traceFile
          , "--project-root"
          , "/tmp"
          ]
      )
        { std_in = CreatePipe
        , std_out = CreatePipe
        , std_err = Inherit
        }
  hSetBuffering sIn NoBuffering
  BC.hPut sIn frames
  hFlush sIn
  hClose sIn
  _ <- waitForProcess ph
  pure ()

spec :: Spec
spec = do
  describe "timestampUs" $ do
    it "converts epoch to 0 microseconds" $
      timestampUs epochTime `shouldBe` 0

    it "converts 1 second past epoch to 1_000_000 microseconds" $
      timestampUs (posixSecondsToUTCTime 1) `shouldBe` 1_000_000

    it "converts 1.5 seconds past epoch to 1_500_000 microseconds" $
      timestampUs (posixSecondsToUTCTime 1.5) `shouldBe` 1_500_000

  describe "decodePayload" $ do
    it "decodes valid JSON object" $
      decodePayload (BC.pack "{\"x\":1}") `shouldBe` object ["x" .= (1 :: Int)]

    it "returns Null for invalid JSON" $
      decodePayload (BC.pack "not json") `shouldBe` Null

    it "returns Null for empty input" $
      decodePayload BC.empty `shouldBe` Null

  describe "backfillHeader" $ around_ withTraceFile $ do
    it "rewrites first line with new header, leaves second line unchanged" $ do
      let header =
            TraceHeader
              { thTraceVersion = 1
              , thRecordedAt = posixSecondsToUTCTime 0
              , thServerCommand = "cat"
              , thProjectRoot = "/tmp"
              , thServerInfo = Nothing
              , thOs = "linux-x86_64"
              , thSnapshotPath = Nothing
              }
          msgLine = BC.pack "{\"seq\":0,\"msg\":\"hello\"}"

      BC.writeFile traceFile $
        BL.toStrict (encode header) <> "\n" <> msgLine <> "\n"

      let updatedHeader = header{thServerInfo = Just ServerInfo{serverInfoName = "myls", serverInfoVersion = Just "1.0"}}
      backfillHeader traceFile updatedHeader

      contents <- BC.readFile traceFile
      case BC.lines contents of
        (firstLine : secondLine : _) -> do
          case decodeStrict firstLine of
            Nothing -> fail "first line not valid header JSON"
            Just h ->
              thServerInfo h `shouldBe` Just ServerInfo{serverInfoName = "myls", serverInfoVersion = Just "1.0"}
          secondLine `shouldBe` msgLine
        ls -> fail $ "expected at least 2 lines, got " <> show (length ls)

  describe "extractServerInfo" $ do
    it "extracts name and version from initialize response" $ do
      let payload =
            BC.pack $
              "{\"jsonrpc\":\"2.0\",\"id\":0,\"result\":{\"serverInfo\":{\"name\":\"hls\",\"version\":\"2.0\"}}}"
      extractServerInfo payload
        `shouldBe` Just ServerInfo{serverInfoName = "hls", serverInfoVersion = Just "2.0"}

    it "extracts name without version" $ do
      let payload =
            BC.pack $
              "{\"jsonrpc\":\"2.0\",\"id\":0,\"result\":{\"serverInfo\":{\"name\":\"hls\"}}}"
      extractServerInfo payload
        `shouldBe` Just ServerInfo{serverInfoName = "hls", serverInfoVersion = Nothing}

    it "returns Nothing for missing serverInfo" $ do
      let payload = BC.pack "{\"jsonrpc\":\"2.0\",\"id\":0,\"result\":{}}"
      extractServerInfo payload `shouldBe` Nothing

    it "returns Nothing for non-JSON input" $
      extractServerInfo (BC.pack "not json") `shouldBe` Nothing

    it "returns Nothing for missing result field" $ do
      let payload = BC.pack "{\"jsonrpc\":\"2.0\",\"method\":\"initialize\"}"
      extractServerInfo payload `shouldBe` Nothing

  describe "Record integration" $ around_ withTraceFile $ do
    it "records an LSP message to a JSONL trace file" $ do
      let lspPayload = "{\"jsonrpc\":\"2.0\",\"method\":\"initialized\",\"params\":{}}"
      runRecordCat (encodeFrame (BC.pack lspPayload))

      contents <- BC.readFile traceFile
      case BC.lines contents of
        (headerLine : msgLine : _) -> do
          case decodeStrict headerLine of
            Nothing -> fail "Failed to parse trace header"
            Just header -> do
              thTraceVersion header `shouldBe` 1
              thProjectRoot header `shouldBe` "/tmp"

          case decodeStrict msgLine :: Maybe TraceMessage of
            Nothing -> fail "Failed to parse trace message"
            Just msg -> do
              tmSeq msg `shouldBe` 0
              tmDirection msg `shouldBe` ClientToServer
              tmRawLength msg `shouldBe` BC.length (BC.pack lspPayload)
        ls -> fail $ "Expected at least 2 lines in trace, got " <> show (length ls)

    it "records multiple messages with monotonically increasing seq numbers and both directions" $ do
      let mkPayload n =
            BC.pack $
              "{\"jsonrpc\":\"2.0\",\"id\":"
                <> show (n :: Int)
                <> ",\"method\":\"textDocument/hover\",\"params\":{}}"
      runRecordCat (mconcat [encodeFrame (mkPayload i) | i <- [1 .. 3]])

      contents <- BC.readFile traceFile
      let ls = BC.lines contents
      -- header + 3 client→server msgs + 3 server→client echoes (cat echoes back)
      length ls `shouldSatisfy` (>= 7)

      let parsedMsgs = [m | Just m <- map (decodeStrict :: BC.ByteString -> Maybe TraceMessage) (drop 1 ls)]
          seqs = map tmSeq parsedMsgs
          dirs = map tmDirection parsedMsgs

      seqs `shouldBe` [0 .. length parsedMsgs - 1]
      dirs `shouldSatisfy` elem ClientToServer
      dirs `shouldSatisfy` elem ServerToClient
