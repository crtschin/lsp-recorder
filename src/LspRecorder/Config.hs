-- | Config file support for the record subcommand.
--
-- Supports an optional JSON config file (@--config FILE@). CLI flags take
-- precedence over config file values. When no config file is given, all
-- required fields must be supplied via CLI flags.
module LspRecorder.Config
  ( SnapshotConfig (..)
  , ExportConfig (..)
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
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import LspRecorder.Cli (RecordOpts (..))
import System.Directory.OsPath (makeAbsolute)
import System.OsPath (OsPath, decodeFS, encodeFS)

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
-- ExportConfig

-- | Configuration for exporting server-emitted files after a replay session.
-- Globs are matched against file names in the server's working directory.
data ExportConfig = ExportConfig
  { ecGlobs :: [String]
  -- ^ Glob patterns for files to copy (matched against names in server cwd).
  , ecDestination :: Maybe FilePath
  -- ^ Directory to copy matching files into. When absent, defaults to the
  -- working directory of the lsp-recorder process at startup.
  }
  deriving stock (Eq, Show)

instance ToJSON ExportConfig where
  toJSON ExportConfig{ecGlobs, ecDestination} =
    object
      [ "globs" .= ecGlobs
      , "destination" .= ecDestination
      ]

instance FromJSON ExportConfig where
  parseJSON = withObject "ExportConfig" $ \o ->
    ExportConfig
      <$> o .:? "globs" Aeson..!= []
      <*> o .:? "destination"

--------------------------------------------------------------------------------
-- RecordConfigFile

-- | Representation of a JSON config file. All fields are optional; required
-- ones must be supplied via CLI flags if absent.
data RecordConfigFile = RecordConfigFile
  { cfServerCommand :: Maybe String
  , cfTraceOut :: Maybe FilePath
  , cfProjectRoot :: Maybe FilePath
  , cfSnapshot :: Maybe SnapshotConfig
  , cfExport :: Maybe ExportConfig
  }
  deriving stock (Eq, Show)

instance ToJSON RecordConfigFile where
  toJSON RecordConfigFile{cfServerCommand, cfTraceOut, cfProjectRoot, cfSnapshot, cfExport} =
    object
      [ "server_command" .= cfServerCommand
      , "trace_out" .= cfTraceOut
      , "project_root" .= cfProjectRoot
      , "snapshot" .= cfSnapshot
      , "export" .= cfExport
      ]

instance FromJSON RecordConfigFile where
  parseJSON = withObject "RecordConfigFile" $ \o ->
    RecordConfigFile
      <$> o .:? "server_command"
      <*> o .:? "trace_out"
      <*> o .:? "project_root"
      <*> o .:? "snapshot"
      <*> o .:? "export"

-- | The merged, validated record configuration used by 'runRecord'.
data RecordConfig = RecordConfig
  { rcServerCommand :: String
  , rcTraceOut :: OsPath
  , rcProjectRoot :: OsPath
  , rcSnapshot :: Maybe SnapshotConfig
  , rcExport :: Maybe ExportConfig
  }
  deriving stock (Eq, Show)

-- | Load a 'RecordConfigFile' from a JSON file.
loadConfig :: OsPath -> IO RecordConfigFile
loadConfig path = do
  pathStr <- decodeFS path
  bs <- BL.readFile pathStr
  case Aeson.eitherDecode bs of
    Left err -> fail $ "Failed to parse config file " <> pathStr <> ": " <> err
    Right cfg -> pure cfg

-- | Merge a 'RecordConfigFile' with CLI opts, with CLI taking precedence.
-- Resolves all paths to absolute paths before storing them.
-- Returns 'Left' with an error message if any required field is absent after
-- merging.
mergeWithCli :: RecordConfigFile -> RecordOpts -> IO (Either String RecordConfig)
mergeWithCli cfg RecordOpts{roServerCommand, roTraceOut, roProjectRoot, roConfig = _} = do
  let serverCommand = roServerCommand `pickFirst` cfServerCommand cfg
      traceOutStr = roTraceOut `pickFirst` cfTraceOut cfg
      projectRootStr = roProjectRoot `pickFirst` cfProjectRoot cfg
  case (serverCommand, traceOutStr, projectRootStr) of
    (Nothing, _, _) -> pure $ Left "Missing required field: server_command / --server-command"
    (_, Nothing, _) -> pure $ Left "Missing required field: trace_out / --trace-out"
    (_, _, Nothing) -> pure $ Left "Missing required field: project_root / --project-root"
    (Just sc, Just to, Just pr) -> do
      traceOut <- encodeFS to >>= makeAbsolute
      projectRoot <- encodeFS pr >>= makeAbsolute
      pure $
        Right
          RecordConfig
            { rcServerCommand = sc
            , rcTraceOut = traceOut
            , rcProjectRoot = projectRoot
            , rcSnapshot = cfSnapshot cfg
            , rcExport = cfExport cfg
            }
 where
  pickFirst (Just x) _ = Just x
  pickFirst Nothing y = y
