module LspRecorder.Lsp.Framing
  ( framingHeaderParser
  , singleFrameParser
  , encodeFrame
  , FrameStreamConfig (..)
  , defaultFrameStreamConfig
  , streamFrames
  , readFramedMessages
  ) where

import Control.Exception (IOException, catch)
import Data.Attoparsec.ByteString (Parser)
import Data.Attoparsec.ByteString qualified as AP
import Data.Attoparsec.ByteString.Char8 qualified as APC
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import System.IO (Handle)

-- | Parse the Content-Length header line and return the payload length.
-- Expects: "Content-Length: <decimal>\r\n\r\n"
-- Ignores any other headers between the first and the blank line.
framingHeaderParser :: Parser Int
framingHeaderParser = do
  len <- APC.string "Content-Length: " *> APC.decimal
  _ <- APC.string "\r\n"
  skipExtraHeaders
  _ <- APC.string "\r\n"
  pure len
 where
  -- Skip any additional headers (e.g. Content-Type) until blank line
  skipExtraHeaders = do
    nextTwo <- AP.peekWord8'
    if nextTwo == 0x0D -- '\r'
      then pure () -- blank line next, done
      else do
        -- skip until end of this header line
        _ <- AP.takeWhile (/= 0x0A) -- skip to '\n'
        _ <- AP.word8 0x0A -- consume '\n'
        skipExtraHeaders

-- | Parse a complete LSP frame: header then exactly N bytes of payload.
singleFrameParser :: Parser ByteString
singleFrameParser = do
  len <- framingHeaderParser
  AP.take len

-- | Encode a payload into a valid LSP frame.
encodeFrame :: ByteString -> ByteString
encodeFrame payload =
  "Content-Length: "
    <> BC.pack (show (BS.length payload))
    <> "\r\n\r\n"
    <> payload

-- | Configuration for 'streamFrames'.
data FrameStreamConfig = FrameStreamConfig
  { fscOnEof :: IO ()
  -- ^ Called when the source handle reaches EOF.
  , fscOnChunk :: ByteString -> IO ()
  -- ^ Called with each raw chunk before parsing (e.g. for forwarding).
  , fscOnParseError :: String -> IO ()
  -- ^ Called on parse failures with an error message.
  }

-- | Default config with all callbacks as no-ops.
defaultFrameStreamConfig :: FrameStreamConfig
defaultFrameStreamConfig =
  FrameStreamConfig
    { fscOnEof = pure ()
    , fscOnChunk = \_ -> pure ()
    , fscOnParseError = \_ -> pure ()
    }

-- | Stream complete frames from a Handle, calling the action for each payload.
-- Reads 32KB chunks and feeds them incrementally to attoparsec.
-- Returns on EOF or IOException.
streamFrames :: FrameStreamConfig -> Handle -> (ByteString -> IO ()) -> IO ()
streamFrames FrameStreamConfig{fscOnEof, fscOnChunk, fscOnParseError} h action =
  loop freshParser
 where
  chunkSize :: Int
  chunkSize = 32768

  freshParser = AP.parse singleFrameParser BS.empty

  loop partial = do
    chunk <- BS.hGetSome h chunkSize `catch` \(_ :: IOException) -> pure BS.empty
    if BS.null chunk
      then fscOnEof
      else do
        fscOnChunk chunk
        feedChunk chunk partial

  feedChunk chunk partial =
    case AP.feed partial chunk of
      AP.Fail remaining _ failMsg -> do
        fscOnParseError failMsg
        -- Skip one byte to resynchronise, then retry with remaining bytes.
        let remaining' = BS.drop 1 remaining
        if BS.null remaining'
          then loop freshParser
          else feedChunk remaining' freshParser
      AP.Partial cont ->
        loop (AP.Partial cont)
      AP.Done remainder payload -> do
        action payload
        if BS.null remainder
          then loop freshParser
          else feedChunk remainder freshParser

-- | Stream complete frames using default config (no chunk forwarding, no error logging).
readFramedMessages :: Handle -> (ByteString -> IO ()) -> IO ()
readFramedMessages = streamFrames defaultFrameStreamConfig
