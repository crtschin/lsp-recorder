module FramingSpec (spec) where

import Control.Exception (finally)
import Data.Attoparsec.ByteString qualified as AP
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.IORef (modifyIORef', newIORef, readIORef)
import LspRecorder.Lsp.Framing
  ( FrameStreamConfig (..)
  , defaultFrameStreamConfig
  , encodeFrame
  , readFramedMessages
  , singleFrameParser
  , streamFrames
  )
import System.IO (Handle, hClose, hSetBinaryMode)
import System.Posix.IO (createPipe, fdToHandle)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (Gen, choose, forAll, vectorOf)
import Test.QuickCheck.Arbitrary (Arbitrary (..))

-- Newtype to avoid orphan instance for Arbitrary ByteString
newtype ArbBS = ArbBS {unArbBS :: ByteString}
  deriving stock (Show)

instance Arbitrary ArbBS where
  arbitrary = do
    n <- choose (0, 256)
    bytes <- vectorOf n arbitrary
    pure $ ArbBS (BS.pack bytes)
  shrink (ArbBS bs) = [ArbBS (BS.take n bs) | n <- [0 .. BS.length bs - 1]]

-- | Write bytes to the write end of a pipe, close it, then pass the read end to action.
withPipeHandle :: ByteString -> (Handle -> IO a) -> IO a
withPipeHandle bytes action = do
  (readFd, writeFd) <- createPipe
  readH <- fdToHandle readFd
  writeH <- fdToHandle writeFd
  hSetBinaryMode readH True
  hSetBinaryMode writeH True
  BS.hPut writeH bytes
  hClose writeH
  action readH `finally` hClose readH

spec :: Spec
spec = describe "LSP Framing" $ do
  describe "encodeFrame / parse round-trip" $ do
    prop "round-trips arbitrary ByteString" $
      \(ArbBS bs) ->
        parseOne (encodeFrame bs) == Right bs

    it "round-trips empty ByteString" $
      parseOne (encodeFrame BS.empty) `shouldBe` Right BS.empty

  describe "concatenated frames" $ do
    prop "parses two concatenated frames" $
      \(ArbBS a) (ArbBS b) ->
        parseBoth (encodeFrame a <> encodeFrame b) == Right (a, b)

  describe "partial feeds" $ do
    prop "parses frame split at arbitrary offset" $
      \(ArbBS bs) ->
        forAll (splitPoint (encodeFrame bs)) $ \n ->
          let frame = encodeFrame bs
              (f1, f2) = BS.splitAt n frame
              partial = AP.parse singleFrameParser f1
              result = AP.feed partial f2
           in case result of
                AP.Done _ payload -> payload == bs
                _ -> False

  describe "large payloads" $ do
    it "parses 5MB ByteString without stack overflow" $ do
      let big = BS.replicate (5 * 1024 * 1024) 0x41
      parseOne (encodeFrame big) `shouldBe` Right big

  describe "multi-digit Content-Length" $ do
    it "parses frame with length > 9999" $ do
      let bs = BS.replicate 10000 0x7B
      parseOne (encodeFrame bs) `shouldBe` Right bs

    it "parses frame with length > 99999" $ do
      let bs = BS.replicate 100000 0x7B
      parseOne (encodeFrame bs) `shouldBe` Right bs

  describe "Content-Length header" $ do
    prop "encoded Content-Length matches actual payload size" $
      \(ArbBS bs) ->
        let frame = encodeFrame bs
            header = BC.pack "Content-Length: " <> BC.pack (show (BS.length bs)) <> "\r\n\r\n"
         in BS.isPrefixOf header frame

  describe "streamFrames" $ do
    it "delivers payload and calls fscOnChunk and fscOnEof for a single valid frame" $ do
      let payload = "hello lsp"
      chunksRef <- newIORef ([] :: [ByteString])
      eofRef <- newIORef (0 :: Int)
      let cfg =
            defaultFrameStreamConfig
              { fscOnChunk = \c -> modifyIORef' chunksRef (c :)
              , fscOnEof = modifyIORef' eofRef (+ 1)
              }
      payloadsRef <- newIORef ([] :: [ByteString])
      withPipeHandle (encodeFrame payload) $ \h ->
        streamFrames cfg h (\p -> modifyIORef' payloadsRef (p :))
      payloads <- readIORef payloadsRef
      reverse payloads `shouldBe` [payload]
      chunks <- readIORef chunksRef
      BS.concat (reverse chunks) `shouldBe` encodeFrame payload
      eofCount <- readIORef eofRef
      eofCount `shouldBe` 1

    it "delivers all payloads in order for multiple concatenated frames" $ do
      let payloads0 = ["frame-one", "frame-two", "frame-three"]
      payloadsRef <- newIORef ([] :: [ByteString])
      withPipeHandle (foldMap encodeFrame payloads0) $ \h ->
        streamFrames defaultFrameStreamConfig h (\p -> modifyIORef' payloadsRef (p :))
      payloads <- readIORef payloadsRef
      reverse payloads `shouldBe` payloads0

    it "calls fscOnEof and no payload action on empty input" $ do
      eofRef <- newIORef (0 :: Int)
      payloadsRef <- newIORef ([] :: [ByteString])
      let cfg = defaultFrameStreamConfig{fscOnEof = modifyIORef' eofRef (+ 1)}
      withPipeHandle BS.empty $ \h ->
        streamFrames cfg h (\p -> modifyIORef' payloadsRef (p :))
      eofCount <- readIORef eofRef
      eofCount `shouldBe` 1
      payloads <- readIORef payloadsRef
      payloads `shouldBe` []

    it "calls fscOnParseError on garbage input and still delivers a subsequent valid frame" $ do
      let garbage = "this is not a valid lsp frame\r\n\r\n"
          good = "recovered"
      errorsRef <- newIORef ([] :: [String])
      payloadsRef <- newIORef ([] :: [ByteString])
      let cfg =
            defaultFrameStreamConfig
              { fscOnParseError = \e -> modifyIORef' errorsRef (e :)
              }
      withPipeHandle (garbage <> encodeFrame good) $ \h ->
        streamFrames cfg h (\p -> modifyIORef' payloadsRef (p :))
      errors <- readIORef errorsRef
      errors `shouldSatisfy` (not . null)
      payloads <- readIORef payloadsRef
      payloads `shouldBe` [good]

    it "readFramedMessages delivers payload equivalently to streamFrames defaultFrameStreamConfig" $ do
      let payload = "readFramedMessages test"
      payloadsRef <- newIORef ([] :: [ByteString])
      withPipeHandle (encodeFrame payload) $ \h ->
        readFramedMessages h (\p -> modifyIORef' payloadsRef (p :))
      payloads <- readIORef payloadsRef
      payloads `shouldBe` [payload]

parseOne :: ByteString -> Either String ByteString
parseOne bs = AP.parseOnly singleFrameParser bs

parseBoth :: ByteString -> Either String (ByteString, ByteString)
parseBoth bs = case AP.parse singleFrameParser bs of
  AP.Done remainder a -> case AP.parseOnly singleFrameParser remainder of
    Left err -> Left err
    Right b -> Right (a, b)
  AP.Partial _ -> Left "incomplete input"
  AP.Fail _ _ err -> Left err

splitPoint :: ByteString -> Gen Int
splitPoint bs = choose (0, max 0 (BS.length bs))
