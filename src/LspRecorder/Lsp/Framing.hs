module LspRecorder.Lsp.Framing (
    framingHeaderParser,
    singleFrameParser,
    encodeFrame,
    readFramedMessages,
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

-- | Stream complete frames from a Handle, calling the action for each payload.
-- Reads 32KB chunks and feeds them incrementally to attoparsec.
-- Returns on EOF or IOException.
readFramedMessages :: Handle -> (ByteString -> IO ()) -> IO ()
readFramedMessages h action = loop (AP.parse singleFrameParser BS.empty)
  where
    chunkSize :: Int
    chunkSize = 32768

    loop partial = do
        chunk <- BS.hGetSome h chunkSize `catch` \(_ :: IOException) -> pure BS.empty
        if BS.null chunk
            then pure () -- EOF
            else feedChunk chunk partial

    feedChunk chunk partial =
        case AP.feed partial chunk of
            AP.Fail{} ->
                -- Parse error: skip and try to recover by starting fresh
                loop (AP.parse singleFrameParser BS.empty)
            AP.Partial cont ->
                -- Need more data
                loop (AP.Partial cont)
            AP.Done remainder payload -> do
                action payload
                -- Process the remainder with a fresh parser
                if BS.null remainder
                    then loop (AP.parse singleFrameParser BS.empty)
                    else feedChunk remainder (AP.parse singleFrameParser BS.empty)
