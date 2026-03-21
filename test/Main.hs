module Main where

import Test.Hspec (hspec)
import FramingSpec qualified
import RecordSpec qualified

main :: IO ()
main = hspec $ do
    FramingSpec.spec
    RecordSpec.spec
