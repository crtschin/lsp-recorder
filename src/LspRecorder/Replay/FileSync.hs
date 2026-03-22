-- | Track and apply editor file modifications during replay.
--
-- During replay the snapshot is restored to a temp directory, but files
-- remain static. Language servers often read files from disk for import
-- resolution and type checking, so without keeping the temp directory in sync
-- with the LSP messages the server sees stale state.
--
-- This module maintains an in-memory document map (URI → current content) and
-- writes each change to disk before the message is forwarded to the server.
module LspRecorder.Replay.FileSync
  ( DocState
  , newDocState
  , applyFileEffect
  , uriToFilePath
  , applyContentChanges
  ) where

import Data.Aeson (Value (..))
import Data.Aeson.Key qualified as Key
import Data.Aeson.KeyMap qualified as KM
import Data.Foldable (toList)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Text.Rope (Position (..), Rope)
import Data.Text.Rope qualified as Rope
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import System.IO (hPutStrLn, stderr)

-- | Opaque document state: tracks current content of open documents by URI.
newtype DocState = DocState (IORef (Map Text Rope))

-- | Create a new empty document state.
newDocState :: IO DocState
newDocState = DocState <$> newIORef Map.empty

-- | Apply any file-system side-effects implied by an LSP notification.
-- Handles @textDocument/didOpen@, @textDocument/didChange@, and
-- @textDocument/didSave@; all other methods are no-ops.
applyFileEffect :: DocState -> Value -> IO ()
applyFileEffect ds val = case getMethod val of
  Just "textDocument/didOpen" -> handleDidOpen ds val
  Just "textDocument/didChange" -> handleDidChange ds val
  Just "textDocument/didSave" -> handleDidSave ds val
  _ -> pure ()

-- ---------------------------------------------------------------------------
-- Handlers

handleDidOpen :: DocState -> Value -> IO ()
handleDidOpen (DocState ref) val = do
  let mUri = params val >>= field "textDocument" >>= field "uri" >>= asText
      mText = params val >>= field "textDocument" >>= field "text" >>= asText
  case (mUri, mText) of
    (Just uri, Just content) -> modifyIORef' ref (Map.insert uri (Rope.fromText content))
    _ -> pure ()

handleDidChange :: DocState -> Value -> IO ()
handleDidChange (DocState ref) val = do
  let mUri = params val >>= field "textDocument" >>= field "uri" >>= asText
      mChanges = params val >>= field "contentChanges" >>= asArray
  case (mUri, mChanges) of
    (Just uri, Just changes) -> do
      docs <- readIORef ref
      case Map.lookup uri docs of
        Nothing ->
          hPutStrLn stderr $ "Warning: didChange for unknown URI: " <> T.unpack uri
        Just current -> do
          let updated = applyContentChanges current changes
          modifyIORef' ref (Map.insert uri updated)
    _ -> pure ()

handleDidSave :: DocState -> Value -> IO ()
handleDidSave (DocState ref) val = do
  let mUri = params val >>= field "textDocument" >>= field "uri" >>= asText
      mText = params val >>= field "text" >>= asText
  case mUri of
    Nothing -> pure ()
    Just uri -> do
      rope <- case mText of
        Just t -> Rope.fromText t <$ modifyIORef' ref (Map.insert uri (Rope.fromText t))
        Nothing -> fromMaybe mempty . Map.lookup uri <$> readIORef ref
      mapM_ (`writeToDisk` Rope.toText rope) (uriToFilePath uri)

-- ---------------------------------------------------------------------------
-- Content-change application

-- | Apply a sequence of content changes to the current document rope.
-- Changes are applied in order, each one operating on the result of the
-- previous.
applyContentChanges :: Rope -> [Value] -> Rope
applyContentChanges = foldl applyOneChange

-- | Apply a single LSP @TextDocumentContentChangeEvent@.
-- If the change has no @range@, it is a full replacement.
-- If @range@ is present, the named range is spliced with the new text.
--
-- __Known limitation__: LSP specifies @character@ as a UTF-16 code-unit
-- offset. This implementation treats it as a Unicode code-point index, which
-- diverges for characters outside the Basic Multilingual Plane. Acceptable
-- for ASCII-dominated source code.
applyOneChange :: Rope -> Value -> Rope
applyOneChange current val =
  let mRange = field "range" val
      mText = field "text" val >>= asText
   in case (mRange, mText) of
        (Nothing, Just newText) -> Rope.fromText newText
        (Just range, Just newText) ->
          case (field "start" range, field "end" range) of
            (Just start, Just end) ->
              let startPos =
                    Position
                      { posLine = fromIntegral (intField "line" start)
                      , posColumn = fromIntegral (intField "character" start)
                      }
                  endPos =
                    Position
                      { posLine = fromIntegral (intField "line" end)
                      , posColumn = fromIntegral (intField "character" end)
                      }
                  (before, _) = Rope.splitAtPosition startPos current
                  (_, after) = Rope.splitAtPosition endPos current
               in before <> Rope.fromText newText <> after
            _ -> current
        _ -> current

-- ---------------------------------------------------------------------------
-- URI / path helpers

-- | Strip the @file://@ prefix from an LSP URI to obtain a 'FilePath'.
-- Returns 'Nothing' for non-@file://@ URIs.
--
-- >>> uriToFilePath "file:///home/user/foo.hs"
-- Just "/home/user/foo.hs"
uriToFilePath :: Text -> Maybe FilePath
uriToFilePath uri
  | T.isPrefixOf "file://" uri = Just (T.unpack (T.drop 7 uri))
  | otherwise = Nothing

-- ---------------------------------------------------------------------------
-- Disk I/O

-- | Write @content@ to @fp@, creating any missing parent directories first.
writeToDisk :: FilePath -> Text -> IO ()
writeToDisk fp content = do
  createDirectoryIfMissing True (takeDirectory fp)
  TIO.writeFile fp content

-- ---------------------------------------------------------------------------
-- JSON micro-helpers

getMethod :: Value -> Maybe Text
getMethod (Object o) = case KM.lookup (Key.fromText "method") o of
  Just (String t) -> Just t
  _ -> Nothing
getMethod _ = Nothing

params :: Value -> Maybe Value
params (Object o) = KM.lookup (Key.fromText "params") o
params _ = Nothing

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

asText :: Value -> Maybe Text
asText (String t) = Just t
asText _ = Nothing

asArray :: Value -> Maybe [Value]
asArray (Array a) = Just (toList a)
asArray _ = Nothing

intField :: Text -> Value -> Int
intField k v = case field k v of
  Just (Number n) -> round n
  _ -> 0
