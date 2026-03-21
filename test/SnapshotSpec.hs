module SnapshotSpec (spec) where

import Control.Exception (bracket)
import Data.List (sort)
import LspRecorder.Config (SnapshotConfig (..))
import LspRecorder.Snapshot (collectFiles, restoreSnapshot, takeSnapshot)
import System.Directory
  ( createDirectoryIfMissing
  , doesFileExist
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.FilePath ((</>))
import System.IO (hClose, openTempFile)
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldMatchList, shouldReturn)

-- | Run a test inside a fresh temporary directory, cleaned up afterward.
withTempDir :: (FilePath -> IO ()) -> IO ()
withTempDir action =
  bracket acquire release action
  where
    acquire = do
      base <- getTemporaryDirectory
      (tmpFile, h) <- openTempFile base "snapshot-test"
      hClose h
      removeFile tmpFile
      let dir = tmpFile <> ".d"
      createDirectoryIfMissing True dir
      pure dir
    release = removeDirectoryRecursive

-- | Write an empty file at @path@, creating parent directories as needed.
touch :: FilePath -> IO ()
touch path = do
  createDirectoryIfMissing True (reverse . dropWhile (/= '/') . reverse $ path)
  writeFile path ""

spec :: Spec
spec = do
  describe "Snapshot.collectFiles" $ do
    around withTempDir $ do
      it "collects files matching include globs" $ \tmp -> do
        touch (tmp </> "Main.hs")
        touch (tmp </> "Lib.hs")
        touch (tmp </> "README.md")
        let cfg = SnapshotConfig{scInclude = ["**/*.hs"], scExclude = []}
        files <- collectFiles tmp cfg
        sort files `shouldMatchList` ["Lib.hs", "Main.hs"]

      it "excludes files matching exclude globs" $ \tmp -> do
        touch (tmp </> "src/Lib.hs")
        touch (tmp </> "dist-newstyle/build/Main.hs")
        let cfg = SnapshotConfig{scInclude = ["**/*.hs"], scExclude = ["dist-newstyle/**"]}
        files <- collectFiles tmp cfg
        files `shouldBe` ["src/Lib.hs"]

      it "returns empty list when no files match" $ \tmp -> do
        touch (tmp </> "build.log")
        let cfg = SnapshotConfig{scInclude = ["**/*.hs"], scExclude = []}
        files <- collectFiles tmp cfg
        files `shouldBe` []

      it "collects files from nested directories" $ \tmp -> do
        touch (tmp </> "a/b/c/deep.hs")
        touch (tmp </> "top.hs")
        let cfg = SnapshotConfig{scInclude = ["**/*.hs"], scExclude = []}
        files <- collectFiles tmp cfg
        sort files `shouldMatchList` ["a/b/c/deep.hs", "top.hs"]

      it "prunes excluded directories without descending" $ \tmp -> do
        touch (tmp </> "src/Good.hs")
        touch (tmp </> ".stack-work/build/Bad.hs")
        let cfg = SnapshotConfig{scInclude = ["**/*.hs"], scExclude = [".stack-work/**"]}
        files <- collectFiles tmp cfg
        files `shouldBe` ["src/Good.hs"]

  describe "Snapshot.restoreSnapshot" $ do
    it "round-trips with takeSnapshot — files exist with expected contents" $ do
      withTempDir $ \srcDir -> do
        writeFile (srcDir </> "Main.hs") "main = pure ()"
        createDirectoryIfMissing True (srcDir </> "src")
        writeFile (srcDir </> "src" </> "Lib.hs") "module Lib where"
        let cfg = SnapshotConfig{scInclude = ["**/*.hs"], scExclude = []}
        withTempDir $ \archiveDir -> do
          let archivePath = archiveDir </> "test.snapshot.tar.zst"
          takeSnapshot cfg srcDir archivePath
          withTempDir $ \destDir -> do
            restoreSnapshot archivePath destDir
            doesFileExist (destDir </> "Main.hs") `shouldReturn` True
            readFile (destDir </> "Main.hs") `shouldReturn` "main = pure ()"
            doesFileExist (destDir </> "src" </> "Lib.hs") `shouldReturn` True
            readFile (destDir </> "src" </> "Lib.hs") `shouldReturn` "module Lib where"
