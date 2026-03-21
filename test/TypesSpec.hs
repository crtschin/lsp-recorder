module TypesSpec (spec) where

import Data.Aeson (decode, encode)
import Data.Aeson qualified as Aeson
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy qualified as BL
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import LspRecorder.Lsp.Types
  ( Direction (..)
  , ServerInfo (..)
  , TraceHeader (..)
  , TraceMessage (..)
  )
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

-- | A fixed UTCTime for use in tests.
testTime :: UTCTime
testTime = posixSecondsToUTCTime 0

spec :: Spec
spec = describe "Lsp.Types JSON" $ do
  describe "Direction" $ do
    it "ClientToServer roundtrips" $
      decode (encode ClientToServer) `shouldBe` Just ClientToServer

    it "ServerToClient roundtrips" $
      decode (encode ServerToClient) `shouldBe` Just ServerToClient

    it "rejects invalid string" $
      (decode "\"invalid\"" :: Maybe Direction) `shouldBe` Nothing

  describe "ServerInfo" $ do
    it "roundtrips with version" $ do
      let si = ServerInfo{serverInfoName = "myls", serverInfoVersion = Just "1.2.3"}
      decode (encode si) `shouldBe` Just si

    it "roundtrips without version" $ do
      let si = ServerInfo{serverInfoName = "myls", serverInfoVersion = Nothing}
      decode (encode si) `shouldBe` Just si

  describe "TraceHeader" $ do
    it "roundtrips with serverInfo" $ do
      let h =
            TraceHeader
              { thTraceVersion = 1
              , thRecordedAt = testTime
              , thServerCommand = "hls --lsp"
              , thProjectRoot = "/tmp/proj"
              , thServerInfo = Just ServerInfo{serverInfoName = "hls", serverInfoVersion = Just "2.0"}
              , thOs = "linux-x86_64"
              , thSnapshotPath = Nothing
              }
      decode (encode h) `shouldBe` Just h

    it "roundtrips without serverInfo" $ do
      let h =
            TraceHeader
              { thTraceVersion = 1
              , thRecordedAt = testTime
              , thServerCommand = "hls --lsp"
              , thProjectRoot = "/tmp/proj"
              , thServerInfo = Nothing
              , thOs = "linux-x86_64"
              , thSnapshotPath = Nothing
              }
      decode (encode h) `shouldBe` Just h

    it "roundtrips with snapshotPath" $ do
      let h =
            TraceHeader
              { thTraceVersion = 1
              , thRecordedAt = testTime
              , thServerCommand = "hls --lsp"
              , thProjectRoot = "/tmp/proj"
              , thServerInfo = Nothing
              , thOs = "linux-x86_64"
              , thSnapshotPath = Just "/tmp/proj/session.snapshot.tar.zst"
              }
      decode (encode h) `shouldBe` Just h

    it "parses old traces without snapshot_path field" $ do
      let json =
            "{\"trace_version\":1,\"recorded_at\":\"1970-01-01T00:00:00Z\"\
            \,\"server_command\":\"hls\",\"project_root\":\"/p\"\
            \,\"os\":\"linux\",\"server_info\":null}"
      let h = decode json :: Maybe TraceHeader
      fmap thSnapshotPath h `shouldBe` Just Nothing

  describe "TraceMessage" $ do
    it "roundtrips with object payload" $ do
      let msg =
            TraceMessage
              { tmSeq = 0
              , tmTimestampUs = 1000000
              , tmDirection = ClientToServer
              , tmRawLength = 42
              , tmMessage = Aeson.object ["method" Aeson..= ("initialized" :: String)]
              }
      decode (encode msg) `shouldBe` Just msg

    it "roundtrips with Null payload" $ do
      let msg =
            TraceMessage
              { tmSeq = 1
              , tmTimestampUs = 2000000
              , tmDirection = ServerToClient
              , tmRawLength = 4
              , tmMessage = Aeson.Null
              }
      decode (encode msg) `shouldBe` Just msg

    it "encodes direction as string" $ do
      let msg =
            TraceMessage
              { tmSeq = 0
              , tmTimestampUs = 0
              , tmDirection = ClientToServer
              , tmRawLength = 0
              , tmMessage = Aeson.Null
              }
          encoded = encode msg
      BL.toStrict encoded `shouldSatisfy` BC.isInfixOf "client_to_server"
