module FramingSpec (spec) where

import Data.Attoparsec.ByteString qualified as AP
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import LspRecorder.Lsp.Framing (encodeFrame, singleFrameParser)
import Test.Hspec (Spec, describe, it, shouldBe)
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
