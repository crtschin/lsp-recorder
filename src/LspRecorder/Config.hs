-- | Config file support for the record subcommand.
--
-- Supports an optional JSON config file (@--config FILE@). CLI flags take
-- precedence over config file values. When no config file is given, all
-- required fields must be supplied via CLI flags.
module LspRecorder.Config
  ( SnapshotConfig (..)
  , RecordConfigFile (..)
  , RecordConfig (..)
  , loadConfig
  , mergeWithCli
  ) where

import Data.Aeson
  ( FromJSON (..)
  , ToJSON (..)
  , object
  , withObject
  , (.:?)
  , (.=)
  )
import Data.ByteString.Lazy qualified as BL
import Data.Aeson qualified as Aeson
import LspRecorder.Cli (RecordOpts (..))

--------------------------------------------------------------------------------
-- SnapshotConfig

data SnapshotConfig = SnapshotConfig
  { scInclude :: [String]
  -- ^ Glob patterns for files to include (relative to project root).
  , scExclude :: [String]
  -- ^ Glob patterns for files/directories to exclude.
  }
  deriving stock (Eq, Show)

instance ToJSON SnapshotConfig where
  toJSON SnapshotConfig{scInclude, scExclude} =
    object
      [ "include" .= scInclude
      , "exclude" .= scExclude
      ]

instance FromJSON SnapshotConfig where
  parseJSON = withObject "SnapshotConfig" $ \o ->
    SnapshotConfig
      <$> o .:? "include" Aeson..!= []
      <*> o .:? "exclude" Aeson..!= []

--------------------------------------------------------------------------------
-- RecordConfigFile

-- | Representation of a JSON config file. All fields are optional; required
-- ones must be supplied via CLI flags if absent.
data RecordConfigFile = RecordConfigFile
  { cfServerCommand :: Maybe String
  , cfTraceOut :: Maybe FilePath
  , cfProjectRoot :: Maybe FilePath
  , cfSnapshot :: Maybe SnapshotConfig
  }
  deriving stock (Eq, Show)

instance ToJSON RecordConfigFile where
  toJSON RecordConfigFile{cfServerCommand, cfTraceOut, cfProjectRoot, cfSnapshot} =
    object
      [ "server_command" .= cfServerCommand
      , "trace_out" .= cfTraceOut
      , "project_root" .= cfProjectRoot
      , "snapshot" .= cfSnapshot
      ]

instance FromJSON RecordConfigFile where
  parseJSON = withObject "RecordConfigFile" $ \o ->
    RecordConfigFile
      <$> o .:? "server_command"
      <*> o .:? "trace_out"
      <*> o .:? "project_root"
      <*> o .:? "snapshot"

-- | The merged, validated record configuration used by 'runRecord'.
data RecordConfig = RecordConfig
  { rcServerCommand :: String
  , rcTraceOut :: FilePath
  , rcProjectRoot :: FilePath
  , rcSnapshot :: Maybe SnapshotConfig
  }
  deriving stock (Eq, Show)

-- | Load a 'RecordConfigFile' from a JSON file.
loadConfig :: FilePath -> IO RecordConfigFile
loadConfig path = do
  bs <- BL.readFile path
  case Aeson.eitherDecode bs of
    Left err -> fail $ "Failed to parse config file " <> path <> ": " <> err
    Right cfg -> pure cfg

-- | Merge a 'RecordConfigFile' with CLI opts, with CLI taking precedence.
-- Returns 'Left' with an error message if any required field is absent after
-- merging.
mergeWithCli :: RecordConfigFile -> RecordOpts -> Either String RecordConfig
mergeWithCli cfg RecordOpts{roServerCommand, roTraceOut, roProjectRoot, roConfig = _} = do
  serverCommand <- require "server_command / --server-command" roServerCommand (cfServerCommand cfg)
  traceOut <- require "trace_out / --trace-out" roTraceOut (cfTraceOut cfg)
  projectRoot <- require "project_root / --project-root" roProjectRoot (cfProjectRoot cfg)
  pure
    RecordConfig
      { rcServerCommand = serverCommand
      , rcTraceOut = traceOut
      , rcProjectRoot = projectRoot
      , rcSnapshot = cfSnapshot cfg
      }
  where
    require :: String -> Maybe a -> Maybe a -> Either String a
    require field cli file = case cli <|> file of
      Just v -> Right v
      Nothing -> Left $ "Missing required field: " <> field
    (<|>) = pickFirst
    pickFirst (Just x) _ = Just x
    pickFirst Nothing y = y
