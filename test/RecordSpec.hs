module RecordSpec (spec) where

import Data.Aeson (decodeStrict)
import Data.ByteString.Char8 qualified as BC
import LspRecorder.Lsp.Framing (encodeFrame)
import LspRecorder.Lsp.Types (Direction (..), TraceHeader (..), TraceMessage (..))
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
withTraceFile action = do
  action
  removeFile traceFile

spec :: Spec
spec = describe "Record integration" $ around_ withTraceFile $ do
  it "records an LSP message to a JSONL trace file" $ do
    let lspPayload = "{\"jsonrpc\":\"2.0\",\"method\":\"initialized\",\"params\":{}}"
        frame = encodeFrame (BC.pack lspPayload)

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
    BC.hPut sIn frame
    hFlush sIn
    hClose sIn

    _ <- waitForProcess ph

    contents <- BC.readFile traceFile
    let ls = BC.lines contents
    length ls `shouldSatisfy` (>= 2)

    case ls of
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
      _ -> fail $ "Expected at least 2 lines in trace, got " <> show (length ls)
