module ReplaySpec (spec) where

import Control.Exception (catch, try)
import Data.Aeson (Value (..), encode, object, (.=))
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict qualified as Map
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import LspRecorder.Lsp.Types (Direction (..), TraceHeader (..), TraceMessage (..))
import LspRecorder.Replay (extractId, extractMethod, parseTrace, rewritePayload)
import LspRecorder.Replay.Report (MethodStats (..), generateReport)
import LspRecorder.Replay.Timing (TimingStrategy (..), immediateStrategy, realisticStrategy)
import System.Directory (removeFile)
import System.IO (BufferMode (..), hSetBuffering)
import System.IO qualified as IO
import System.IO.Error (isUserError)
import System.IO.Unsafe (unsafePerformIO)
import System.OsPath (OsPath, encodeFS)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonNegative (..))

-- | A fixed UTCTime for use in tests.
testTime :: UTCTime
testTime = posixSecondsToUTCTime 0

tracePathStr :: FilePath
tracePathStr = "/tmp/lsp-recorder-replay-spec.jsonl"

{-# NOINLINE tracePath #-}
tracePath :: OsPath
tracePath = unsafePerformIO $ encodeFS tracePathStr

-- | Write lines to a temp file, run action, remove the file.
withTempTrace :: [BL.ByteString] -> IO () -> IO ()
withTempTrace ls action = do
  IO.withFile tracePathStr IO.WriteMode $ \h -> do
    hSetBuffering h (BlockBuffering Nothing)
    mapM_ (BL.hPutStr h . (<> "\n")) ls
  action `catch` (\e -> removeFile tracePathStr >> ioError e)
  removeFile tracePathStr

-- | Encode a minimal valid TraceHeader line.
minimalHeaderLine :: BL.ByteString
minimalHeaderLine =
  encode $
    TraceHeader
      { thTraceVersion = 1
      , thRecordedAt = testTime
      , thServerCommand = "cat"
      , thProjectRoot = "/tmp"
      , thServerInfo = Nothing
      , thOs = "linux-x86_64"
      , thSnapshotPath = Nothing
      }

-- | Encode a TraceMessage line.
msgLine :: Int -> Direction -> BL.ByteString
msgLine seqN dir =
  encode $
    TraceMessage
      { tmSeq = seqN
      , tmTimestampUs = seqN * 1000
      , tmDirection = dir
      , tmRawLength = 10
      , tmMessage = object ["method" .= ("test" :: String)]
      }

spec :: Spec
spec = do
  describe "TimingStrategy" $ do
    describe "immediateStrategy" $ do
      prop "always returns 0 regardless of args" $
        \prev curr -> tsComputeDelay immediateStrategy prev curr == 0

    describe "realisticStrategy" $ do
      it "returns delta when positive" $
        tsComputeDelay realisticStrategy 1000 3000 `shouldBe` 2000

      it "returns 0 when equal" $
        tsComputeDelay realisticStrategy 1000 1000 `shouldBe` 0

      it "returns 0 for negative delta" $
        tsComputeDelay realisticStrategy 3000 1000 `shouldBe` 0

      prop "delay is always >= 0" $
        \(NonNegative prev) (NonNegative curr) ->
          tsComputeDelay realisticStrategy prev curr >= 0

  describe "extractMethod" $ do
    it "extracts string method field" $
      extractMethod (object ["method" .= ("initialize" :: String)]) `shouldBe` Just "initialize"

    it "returns Nothing for missing field" $
      extractMethod (object ["id" .= (1 :: Int)]) `shouldBe` Nothing

    it "returns Nothing for non-string value" $
      extractMethod (object ["method" .= (42 :: Int)]) `shouldBe` Nothing

    it "returns Nothing for non-object input" $
      extractMethod (String "initialize") `shouldBe` Nothing

    it "returns Nothing for Null" $
      extractMethod Null `shouldBe` Nothing

  describe "extractId" $ do
    it "extracts string id" $
      extractId (object ["id" .= ("req-1" :: String)]) `shouldBe` Just "req-1"

    it "extracts numeric id rendered via Scientific show" $
      extractId (object ["id" .= (42 :: Int)]) `shouldBe` Just "42.0"

    it "returns Nothing for missing field" $
      extractId (object ["method" .= ("test" :: String)]) `shouldBe` Nothing

    it "returns Nothing for non-object input" $
      extractId (String "42") `shouldBe` Nothing

    it "returns Nothing for boolean value" $
      extractId (object ["id" .= True]) `shouldBe` Nothing

  describe "generateReport" $ do
    it "empty input produces empty map" $
      generateReport [] `shouldBe` Map.empty

    it "single sample has all percentiles equal to that value" $ do
      let result = generateReport [("textDocument/hover", 50.0)]
      case Map.lookup "textDocument/hover" result of
        Nothing -> fail "expected method in report"
        Just stats -> do
          msCount stats `shouldBe` 1
          msP50Ms stats `shouldBe` 50.0
          msP95Ms stats `shouldBe` 50.0
          msP99Ms stats `shouldBe` 50.0

    it "10 samples [10,20..100]" $ do
      let samples = map (\i -> ("m", fromIntegral (i * 10 :: Int))) [1 .. 10 :: Int]
          result = generateReport samples
      case Map.lookup "m" result of
        Nothing -> fail "expected method in report"
        Just stats -> do
          msCount stats `shouldBe` 10
          msP50Ms stats `shouldBe` 50.0
          msP95Ms stats `shouldBe` 100.0
          msP99Ms stats `shouldBe` 100.0

    it "groups multiple methods correctly" $ do
      let samples =
            [ ("hover", 10.0)
            , ("hover", 20.0)
            , ("completion", 30.0)
            ]
          result = generateReport samples
      Map.size result `shouldBe` 2
      fmap msCount (Map.lookup "hover" result) `shouldBe` Just 2
      fmap msCount (Map.lookup "completion" result) `shouldBe` Just 1

    it "100 samples [1..100] -> p50=50, p95=95, p99=99" $ do
      let samples = map (\i -> ("m", fromIntegral (i :: Int))) [1 .. 100]
          result = generateReport samples
      case Map.lookup "m" result of
        Nothing -> fail "expected method in report"
        Just stats -> do
          msCount stats `shouldBe` 100
          msP50Ms stats `shouldBe` 50.0
          msP95Ms stats `shouldBe` 95.0
          msP99Ms stats `shouldBe` 99.0

  describe "rewritePayload" $ do
    it "replaces bare paths in a JSON payload" $
      rewritePayload "/home/user/proj" "/tmp/snap.d" "{\"uri\":\"/home/user/proj/Foo.hs\"}"
        `shouldBe` "{\"uri\":\"/tmp/snap.d/Foo.hs\"}"

    it "replaces file:// URI paths" $
      rewritePayload "/home/user/proj" "/tmp/snap.d" "{\"uri\":\"file:///home/user/proj/Foo.hs\"}"
        `shouldBe` "{\"uri\":\"file:///tmp/snap.d/Foo.hs\"}"

    it "is a no-op when old and new roots are identical" $
      rewritePayload "/home/user/proj" "/home/user/proj" "{\"rootUri\":\"file:///home/user/proj\"}"
        `shouldBe` "{\"rootUri\":\"file:///home/user/proj\"}"

  describe "parseTrace" $ do
    it "parses valid header + messages" $ do
      withTempTrace [minimalHeaderLine, msgLine 0 ClientToServer, msgLine 1 ServerToClient] $ do
        (header, msgs) <- parseTrace tracePath
        thTraceVersion header `shouldBe` 1
        length msgs `shouldBe` 2

    it "fails on empty file" $ do
      withTempTrace [] $ do
        result <- try (parseTrace tracePath) :: IO (Either IOError (TraceHeader, [TraceMessage]))
        result `shouldSatisfy` \case
          Left e -> isUserError e
          Right _ -> False

    it "skips unparseable lines without failing" $ do
      withTempTrace [minimalHeaderLine, "not valid json at all", msgLine 0 ClientToServer] $ do
        (_header, msgs) <- parseTrace tracePath
        length msgs `shouldBe` 1
