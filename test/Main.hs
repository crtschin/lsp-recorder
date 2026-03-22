module Main where

import ConfigSpec qualified
import FileSyncSpec qualified
import FramingSpec qualified
import RecordSpec qualified
import ReplaySpec qualified
import SnapshotSpec qualified
import Test.Hspec (hspec)
import TypesSpec qualified

main :: IO ()
main = hspec $ do
  ConfigSpec.spec
  FileSyncSpec.spec
  FramingSpec.spec
  RecordSpec.spec
  ReplaySpec.spec
  SnapshotSpec.spec
  TypesSpec.spec
