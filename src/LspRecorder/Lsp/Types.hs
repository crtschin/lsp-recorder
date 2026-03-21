module LspRecorder.Lsp.Types (
    Direction (..),
    ServerInfo (..),
    TraceHeader (..),
    TraceMessage (..),
    LogEntry (..),
) where

import Data.Aeson (
    FromJSON (..),
    ToJSON (..),
    Value,
    object,
    withObject,
    withText,
    (.:),
    (.:?),
    (.=),
 )
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Time (UTCTime)

--------------------------------------------------------------------------------
-- Direction

data Direction = ClientToServer | ServerToClient
    deriving stock (Eq, Show)

instance ToJSON Direction where
    toJSON ClientToServer = "client_to_server"
    toJSON ServerToClient = "server_to_client"

instance FromJSON Direction where
    parseJSON = withText "Direction" $ \case
        "client_to_server" -> pure ClientToServer
        "server_to_client" -> pure ServerToClient
        v -> fail $ "Direction: expected client_to_server or server_to_client, got " <> show v

--------------------------------------------------------------------------------
-- ServerInfo

data ServerInfo = ServerInfo
    { serverInfoName :: Text
    , serverInfoVersion :: Maybe Text
    }
    deriving stock (Eq, Show)

instance ToJSON ServerInfo where
    toJSON ServerInfo{serverInfoName, serverInfoVersion} =
        object
            [ "name" .= serverInfoName
            , "version" .= serverInfoVersion
            ]

instance FromJSON ServerInfo where
    parseJSON = withObject "ServerInfo" $ \o ->
        ServerInfo
            <$> o .: "name"
            <*> o .:? "version"

--------------------------------------------------------------------------------
-- TraceHeader

data TraceHeader = TraceHeader
    { thTraceVersion :: Int
    , thRecordedAt :: UTCTime
    , thServerCommand :: Text
    , thProjectRoot :: FilePath
    , thServerInfo :: Maybe ServerInfo
    , thOs :: Text
    }
    deriving stock (Eq, Show)

instance ToJSON TraceHeader where
    toJSON TraceHeader{thTraceVersion, thRecordedAt, thServerCommand, thProjectRoot, thServerInfo, thOs} =
        object
            [ "trace_version" .= thTraceVersion
            , "recorded_at" .= thRecordedAt
            , "server_command" .= thServerCommand
            , "project_root" .= thProjectRoot
            , "server_info" .= thServerInfo
            , "os" .= thOs
            ]

instance FromJSON TraceHeader where
    parseJSON = withObject "TraceHeader" $ \o ->
        TraceHeader
            <$> o .: "trace_version"
            <*> o .: "recorded_at"
            <*> o .: "server_command"
            <*> o .: "project_root"
            <*> o .:? "server_info"
            <*> o .: "os"

--------------------------------------------------------------------------------
-- TraceMessage

data TraceMessage = TraceMessage
    { tmSeq :: Int
    , tmTimestampUs :: Int
    , tmDirection :: Direction
    , tmRawLength :: Int
    , tmMessage :: Value
    }
    deriving stock (Eq, Show)

instance ToJSON TraceMessage where
    toJSON TraceMessage{tmSeq, tmTimestampUs, tmDirection, tmRawLength, tmMessage} =
        object
            [ "seq" .= tmSeq
            , "timestamp_us" .= tmTimestampUs
            , "direction" .= tmDirection
            , "raw_length" .= tmRawLength
            , "message" .= tmMessage
            ]

instance FromJSON TraceMessage where
    parseJSON = withObject "TraceMessage" $ \o ->
        TraceMessage
            <$> o .: "seq"
            <*> o .: "timestamp_us"
            <*> o .: "direction"
            <*> o .: "raw_length"
            <*> o .: "message"

--------------------------------------------------------------------------------
-- LogEntry (internal queue type)

data LogEntry = LogEntry
    { leDirection :: Direction
    , lePayload :: ByteString
    , leTimestamp :: UTCTime
    , leRawLength :: Int
    }
    deriving stock (Eq, Show)
