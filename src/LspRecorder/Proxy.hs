module LspRecorder.Proxy
  ( ProxyConfig (..)
  , runProxy
  , forwardAndTee
  ) where

import Control.Concurrent.Async (concurrently_)
import Control.Concurrent.STM (TBQueue, atomically, writeTBQueue)
import Control.Exception (SomeException, catch)
import Data.Attoparsec.ByteString qualified as AP
import Data.ByteString qualified as BS
import Data.Time (getCurrentTime)
import LspRecorder.Lsp.Framing (singleFrameParser)
import LspRecorder.Lsp.Types (Direction (..), LogEntry (..))
import System.IO (Handle, hClose, hPutStrLn, stderr)

data ProxyConfig = ProxyConfig
  { pcServerIn :: Handle
  , pcServerOut :: Handle
  , pcEditorIn :: Handle
  , pcEditorOut :: Handle
  , pcLogQueue :: TBQueue (Maybe LogEntry)
  }

-- | Run the bidirectional proxy. Blocks until either direction closes.
runProxy :: ProxyConfig -> IO ()
runProxy ProxyConfig{pcServerIn, pcServerOut, pcEditorIn, pcEditorOut, pcLogQueue} =
  concurrently_
    (forwardAndTee pcEditorIn pcServerIn ClientToServer pcLogQueue)
    (forwardAndTee pcServerOut pcEditorOut ServerToClient pcLogQueue)

-- | Forward bytes from src to dst and tee complete frames to the log queue.
forwardAndTee :: Handle -> Handle -> Direction -> TBQueue (Maybe LogEntry) -> IO ()
forwardAndTee src dst direction queue = loop (AP.parse singleFrameParser BS.empty)
 where
  chunkSize :: Int
  chunkSize = 32768

  loop partial = do
    chunk <- BS.hGetSome src chunkSize
    if BS.null chunk
      then do
        hPutStrLn stderr $ "[lsp-recorder] EOF on " <> show direction <> " channel"
        hClose dst `catch` \(e :: SomeException) ->
          hPutStrLn stderr $ "[lsp-recorder] hClose error (" <> show direction <> "): " <> show e
      else do
        BS.hPut dst chunk
        feedChunk chunk partial

  feedChunk chunk partial =
    case AP.feed partial chunk of
      AP.Fail _ _ failMsg -> do
        -- Parse error: reset parser and continue forwarding
        hPutStrLn stderr $ "[lsp-recorder] parse error (" <> show direction <> "): " <> failMsg
        loop (AP.parse singleFrameParser BS.empty)
      AP.Partial cont ->
        loop (AP.Partial cont)
      AP.Done remainder payload -> do
        enqueue payload
        if BS.null remainder
          then loop (AP.parse singleFrameParser BS.empty)
          else feedChunk remainder (AP.parse singleFrameParser BS.empty)

  enqueue payload = do
    ts <- getCurrentTime
    let entry =
          LogEntry
            { leDirection = direction
            , lePayload = payload
            , leTimestamp = ts
            , leRawLength = BS.length payload
            }
    atomically $ writeTBQueue queue (Just entry)
