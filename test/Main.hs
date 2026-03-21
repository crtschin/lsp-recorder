module Main where

import FramingSpec qualified
import RecordSpec qualified
import Test.Hspec (hspec)

main :: IO ()
main = hspec $ do
  FramingSpec.spec
  RecordSpec.spec
