module ConfigSpec (spec) where

import Data.Aeson (decode, encode)
import LspRecorder.Cli (RecordOpts (..))
import LspRecorder.Config
  ( RecordConfig (..)
  , RecordConfigFile (..)
  , SnapshotConfig (..)
  , mergeWithCli
  )
import System.IO.Unsafe (unsafePerformIO)
import System.OsPath (OsPath, encodeFS)
import Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

-- | Encode a known-good ASCII absolute path to OsPath (safe for test constants).
{-# NOINLINE toOsPath #-}
toOsPath :: String -> OsPath
toOsPath s = unsafePerformIO $ encodeFS s

emptyFile :: RecordConfigFile
emptyFile =
  RecordConfigFile
    { cfServerCommand = Nothing
    , cfTraceOut = Nothing
    , cfProjectRoot = Nothing
    , cfSnapshot = Nothing
    }

emptyOpts :: RecordOpts
emptyOpts =
  RecordOpts
    { roServerCommand = Nothing
    , roTraceOut = Nothing
    , roProjectRoot = Nothing
    , roConfig = Nothing
    }

-- Use absolute paths so makeAbsolute is a no-op and comparisons are stable.
fullFile :: RecordConfigFile
fullFile =
  RecordConfigFile
    { cfServerCommand = Just "hls --lsp"
    , cfTraceOut = Just "/tmp/session.jsonl"
    , cfProjectRoot = Just "/tmp/proj"
    , cfSnapshot =
        Just
          SnapshotConfig
            { scInclude = ["**/*.hs", "**/*.cabal"]
            , scExclude = ["dist-newstyle/**"]
            }
    }

spec :: Spec
spec = describe "Config" $ do
  describe "SnapshotConfig JSON" $ do
    it "roundtrips" $ do
      let sc = SnapshotConfig{scInclude = ["**/*.hs"], scExclude = ["dist/**"]}
      decode (encode sc) `shouldBe` Just sc

    it "defaults to empty lists when keys absent" $ do
      let sc = decode "{}" :: Maybe SnapshotConfig
      sc `shouldBe` Just SnapshotConfig{scInclude = [], scExclude = []}

  describe "RecordConfigFile JSON" $ do
    it "roundtrips full config" $
      decode (encode fullFile) `shouldBe` Just fullFile

    it "roundtrips empty config" $
      decode (encode emptyFile) `shouldBe` Just emptyFile

  describe "mergeWithCli" $ do
    it "succeeds when all required fields come from file" $ do
      result <- mergeWithCli fullFile emptyOpts
      case result of
        Left err -> fail err
        Right cfg -> do
          rcServerCommand cfg `shouldBe` "hls --lsp"
          rcTraceOut cfg `shouldBe` toOsPath "/tmp/session.jsonl"
          rcProjectRoot cfg `shouldBe` toOsPath "/tmp/proj"

    it "CLI flags override file values" $ do
      let opts =
            emptyOpts
              { roServerCommand = Just "my-lsp"
              , roTraceOut = Just "/tmp/out.jsonl"
              , roProjectRoot = Just "/my/proj"
              }
      result <- mergeWithCli fullFile opts
      case result of
        Left err -> fail err
        Right cfg -> do
          rcServerCommand cfg `shouldBe` "my-lsp"
          rcTraceOut cfg `shouldBe` toOsPath "/tmp/out.jsonl"
          rcProjectRoot cfg `shouldBe` toOsPath "/my/proj"

    it "fails when server-command is missing" $ do
      let file = emptyFile{cfTraceOut = Just "/tmp/t.jsonl", cfProjectRoot = Just "/p"}
      result <- mergeWithCli file emptyOpts
      result `shouldSatisfy` isLeft

    it "fails when trace-out is missing" $ do
      let file = emptyFile{cfServerCommand = Just "hls", cfProjectRoot = Just "/p"}
      result <- mergeWithCli file emptyOpts
      result `shouldSatisfy` isLeft

    it "fails when project-root is missing" $ do
      let file = emptyFile{cfServerCommand = Just "hls", cfTraceOut = Just "/tmp/t.jsonl"}
      result <- mergeWithCli file emptyOpts
      result `shouldSatisfy` isLeft

    it "carries snapshot config from file" $ do
      result <- mergeWithCli fullFile emptyOpts
      case result of
        Left err -> fail err
        Right cfg -> rcSnapshot cfg `shouldBe` cfSnapshot fullFile

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False
