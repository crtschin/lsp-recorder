module FileSyncSpec (spec) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Rope (Rope)
import LspRecorder.Replay.FileSync
  ( applyContentChanges
  , applyFileEffect
  , newDocState
  , uriToFilePath
  )
import System.Directory (doesFileExist, removeFile)
import System.IO.Error (isDoesNotExistError)
import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)
import Control.Exception (catch)

-- ---------------------------------------------------------------------------
-- Helpers

-- | Build a didOpen notification for the given URI and content.
didOpenMsg :: Text -> Text -> Value
didOpenMsg uri content =
  object
    [ "method" .= ("textDocument/didOpen" :: Text)
    , "params"
        .= object
          [ "textDocument"
              .= object
                [ "uri" .= uri
                , "text" .= content
                , "languageId" .= ("haskell" :: Text)
                , "version" .= (1 :: Int)
                ]
          ]
    ]

-- | Build a full-replacement didChange notification.
didChangeFull :: Text -> Text -> Value
didChangeFull uri content =
  object
    [ "method" .= ("textDocument/didChange" :: Text)
    , "params"
        .= object
          [ "textDocument" .= object ["uri" .= uri, "version" .= (2 :: Int)]
          , "contentChanges" .= [object ["text" .= content]]
          ]
    ]

-- | Build an incremental didChange notification.
didChangeRange :: Text -> Int -> Int -> Int -> Int -> Text -> Value
didChangeRange uri sl sc el ec newText =
  object
    [ "method" .= ("textDocument/didChange" :: Text)
    , "params"
        .= object
          [ "textDocument" .= object ["uri" .= uri, "version" .= (2 :: Int)]
          , "contentChanges"
              .= [ object
                    [ "range"
                        .= object
                          [ "start" .= object ["line" .= sl, "character" .= sc]
                          , "end" .= object ["line" .= el, "character" .= ec]
                          ]
                    , "text" .= newText
                    ]
                 ]
          ]
    ]

-- | Build a didSave notification (with embedded text).
didSaveWithText :: Text -> Text -> Value
didSaveWithText uri content =
  object
    [ "method" .= ("textDocument/didSave" :: Text)
    , "params"
        .= object
          [ "textDocument" .= object ["uri" .= uri]
          , "text" .= content
          ]
    ]

-- | Build a didSave notification (no embedded text — server reads from disk).
didSaveNoText :: Text -> Value
didSaveNoText uri =
  object
    [ "method" .= ("textDocument/didSave" :: Text)
    , "params" .= object ["textDocument" .= object ["uri" .= uri]]
    ]

tmpFile :: FilePath
tmpFile = "/tmp/lsp-recorder-filesync-spec-test.hs"

tmpUri :: Text
tmpUri = "file://" <> T.pack tmpFile

cleanupTmp :: IO ()
cleanupTmp = removeFile tmpFile `catch` \e -> if isDoesNotExistError e then pure () else ioError e

-- ---------------------------------------------------------------------------
-- Specs

spec :: Spec
spec = do
  describe "uriToFilePath" $ do
    it "strips file:// prefix" $
      uriToFilePath "file:///home/user/foo.hs" `shouldBe` Just "/home/user/foo.hs"

    it "handles double-slash (relative, non-standard)" $
      uriToFilePath "file://foo.hs" `shouldBe` Just "foo.hs"

    it "returns Nothing for non-file URI" $
      uriToFilePath "https://example.com/foo" `shouldBe` Nothing

    it "returns Nothing for bare path" $
      uriToFilePath "/home/user/foo.hs" `shouldBe` Nothing

  describe "applyContentChanges" $ do
    it "full replacement (no range)" $
      applyContentChanges "old content" [object ["text" .= ("new content" :: Text)]]
        `shouldBe` "new content"

    it "empty changes list is a no-op" $
      applyContentChanges ("unchanged" :: Rope) [] `shouldBe` "unchanged"

    it "incremental: replace a word on line 0" $
      -- "hello world" -> replace chars 6-11 ("world") with "there"
      applyContentChanges
        "hello world"
        [ object
            [ "range"
                .= object
                  [ "start" .= object ["line" .= (0 :: Int), "character" .= (6 :: Int)]
                  , "end" .= object ["line" .= (0 :: Int), "character" .= (11 :: Int)]
                  ]
            , "text" .= ("there" :: Text)
            ]
        ]
        `shouldBe` "hello there"

    it "incremental: replace across lines" $
      -- "line1\nline2\nline3" -> replace from (1,0) to (1,5) ("line2") with "replaced"
      applyContentChanges
        "line1\nline2\nline3"
        [ object
            [ "range"
                .= object
                  [ "start" .= object ["line" .= (1 :: Int), "character" .= (0 :: Int)]
                  , "end" .= object ["line" .= (1 :: Int), "character" .= (5 :: Int)]
                  ]
            , "text" .= ("replaced" :: Text)
            ]
        ]
        `shouldBe` "line1\nreplaced\nline3"

    it "applies multiple changes sequentially" $
      applyContentChanges
        "abc"
        [ object ["text" .= ("xyz" :: Text)]
        , object ["text" .= ("123" :: Text)]
        ]
        `shouldBe` "123"

  describe "applyFileEffect" $ do
    it "didOpen does not write content to disk" $ do
      cleanupTmp
      ds <- newDocState
      applyFileEffect ds (didOpenMsg tmpUri "module Main where\n")
      doesFileExist tmpFile `shouldReturn` False

    it "didChange (full) does not write to disk; didSave does" $ do
      cleanupTmp
      ds <- newDocState
      applyFileEffect ds (didOpenMsg tmpUri "original\n")
      applyFileEffect ds (didChangeFull tmpUri "updated\n")
      doesFileExist tmpFile `shouldReturn` False
      applyFileEffect ds (didSaveNoText tmpUri)
      TIO.readFile tmpFile `shouldReturn` "updated\n"
      cleanupTmp

    it "didChange (incremental) does not write to disk; didSave flushes correct content" $ do
      cleanupTmp
      ds <- newDocState
      applyFileEffect ds (didOpenMsg tmpUri "hello world\n")
      -- replace "world" (chars 6-11 on line 0) with "there"
      applyFileEffect ds (didChangeRange tmpUri 0 6 0 11 "there")
      doesFileExist tmpFile `shouldReturn` False
      applyFileEffect ds (didSaveNoText tmpUri)
      TIO.readFile tmpFile `shouldReturn` "hello there\n"
      cleanupTmp

    it "didSave with text updates file on disk" $ do
      cleanupTmp
      ds <- newDocState
      applyFileEffect ds (didOpenMsg tmpUri "first\n")
      applyFileEffect ds (didSaveWithText tmpUri "saved\n")
      TIO.readFile tmpFile `shouldReturn` "saved\n"
      cleanupTmp

    it "didSave without text re-writes current in-memory content" $ do
      cleanupTmp
      ds <- newDocState
      applyFileEffect ds (didOpenMsg tmpUri "content\n")
      applyFileEffect ds (didSaveNoText tmpUri)
      TIO.readFile tmpFile `shouldReturn` "content\n"
      cleanupTmp

    it "didChange for unknown URI logs warning and does not crash" $ do
      ds <- newDocState
      -- no prior didOpen — should not throw
      applyFileEffect ds (didChangeFull tmpUri "should not crash")
      doesFileExist tmpFile `shouldReturn` False

    it "non-file-sync method is a no-op" $ do
      ds <- newDocState
      applyFileEffect ds (object ["method" .= ("textDocument/hover" :: Text)])
      pure () -- just checking it doesn't throw
