module SnapshotSpec (spec) where

import Control.Exception (bracket)
import Data.List (sort)
import LspRecorder.Config (SnapshotConfig (..))
import LspRecorder.Snapshot (collectFiles, restoreSnapshot, takeSnapshot)
import System.Directory.OsPath
  ( createDirectoryIfMissing
  , doesFileExist
  , getTemporaryDirectory
  , removeDirectoryRecursive
  , removeFile
  )
import System.IO (hClose, openTempFile)
import System.OsPath (OsPath, decodeFS, encodeFS, takeDirectory, (</>))
import Test.Hspec (Spec, around, describe, it, shouldBe, shouldMatchList, shouldReturn)

-- | Run a test inside a fresh temporary directory, cleaned up afterward.
withTempDir :: (OsPath -> IO ()) -> IO ()
withTempDir action =
  bracket acquire release action
 where
  acquire = do
    base <- getTemporaryDirectory
    baseStr <- decodeFS base
    (tmpFileStr, h) <- openTempFile baseStr "snapshot-test"
    hClose h
    tmpFile <- encodeFS tmpFileStr
    removeFile tmpFile
    dotDSuffix <- encodeFS ".d"
    let dir = tmpFile <> dotDSuffix
    createDirectoryIfMissing True dir
    pure dir
  release = removeDirectoryRecursive

-- | Write an empty file at @path@, creating parent directories as needed.
touch :: OsPath -> IO ()
touch path = do
  createDirectoryIfMissing True (takeDirectory path)
  pathStr <- decodeFS path
  writeFile pathStr ""

spec :: Spec
spec = do
  describe "Snapshot.collectFiles" $ do
    around withTempDir $ do
      it "collects files matching include globs" $ \tmp -> do
        mainHs <- encodeFS "Main.hs"
        libHs <- encodeFS "Lib.hs"
        readmeMd <- encodeFS "README.md"
        touch (tmp </> mainHs)
        touch (tmp </> libHs)
        touch (tmp </> readmeMd)
        let cfg = SnapshotConfig{scInclude = ["**/*.hs"], scExclude = []}
        files <- collectFiles tmp cfg
        fileStrs <- mapM decodeFS files
        sort fileStrs `shouldMatchList` ["Lib.hs", "Main.hs"]

      it "excludes files matching exclude globs" $ \tmp -> do
        srcLibHs <- encodeFS "src/Lib.hs"
        distMainHs <- encodeFS "dist-newstyle/build/Main.hs"
        touch (tmp </> srcLibHs)
        touch (tmp </> distMainHs)
        let cfg = SnapshotConfig{scInclude = ["**/*.hs"], scExclude = ["dist-newstyle/**"]}
        files <- collectFiles tmp cfg
        fileStrs <- mapM decodeFS files
        fileStrs `shouldBe` ["src/Lib.hs"]

      it "returns empty list when no files match" $ \tmp -> do
        buildLog <- encodeFS "build.log"
        touch (tmp </> buildLog)
        let cfg = SnapshotConfig{scInclude = ["**/*.hs"], scExclude = []}
        files <- collectFiles tmp cfg
        files `shouldBe` []

      it "collects files from nested directories" $ \tmp -> do
        deepHs <- encodeFS "a/b/c/deep.hs"
        topHs <- encodeFS "top.hs"
        touch (tmp </> deepHs)
        touch (tmp </> topHs)
        let cfg = SnapshotConfig{scInclude = ["**/*.hs"], scExclude = []}
        files <- collectFiles tmp cfg
        fileStrs <- mapM decodeFS files
        sort fileStrs `shouldMatchList` ["a/b/c/deep.hs", "top.hs"]

      it "prunes excluded directories without descending" $ \tmp -> do
        srcGoodHs <- encodeFS "src/Good.hs"
        stackBadHs <- encodeFS ".stack-work/build/Bad.hs"
        touch (tmp </> srcGoodHs)
        touch (tmp </> stackBadHs)
        let cfg = SnapshotConfig{scInclude = ["**/*.hs"], scExclude = [".stack-work/**"]}
        files <- collectFiles tmp cfg
        fileStrs <- mapM decodeFS files
        fileStrs `shouldBe` ["src/Good.hs"]

  describe "Snapshot.restoreSnapshot" $ do
    it "round-trips with takeSnapshot — files exist with expected contents" $ do
      withTempDir $ \srcDir -> do
        srcDirStr <- decodeFS srcDir
        srcSubDir <- encodeFS (srcDirStr <> "/src")
        createDirectoryIfMissing True srcSubDir
        writeFile (srcDirStr <> "/Main.hs") "main = pure ()"
        writeFile (srcDirStr <> "/src/Lib.hs") "module Lib where"
        let cfg = SnapshotConfig{scInclude = ["**/*.hs"], scExclude = []}
        withTempDir $ \archiveDir -> do
          archiveName <- encodeFS "test.snapshot.tar.zst"
          let archivePath = archiveDir </> archiveName
          takeSnapshot cfg srcDir archivePath
          withTempDir $ \destDir -> do
            restoreSnapshot archivePath destDir
            destDirStr <- decodeFS destDir
            mainHs <- encodeFS "Main.hs"
            srcLibHs <- encodeFS "src/Lib.hs"
            doesFileExist (destDir </> mainHs) `shouldReturn` True
            readFile (destDirStr <> "/Main.hs") `shouldReturn` "main = pure ()"
            doesFileExist (destDir </> srcLibHs) `shouldReturn` True
            readFile (destDirStr <> "/src/Lib.hs") `shouldReturn` "module Lib where"
