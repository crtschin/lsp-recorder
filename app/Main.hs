module Main where

import Control.Applicative ((<|>))
import LspRecorder.Cli
  ( Command (..)
  , RecordOpts (..)
  , ReplayOpts (..)
  , parseCommand
  )
import LspRecorder.Config
  ( RecordConfigFile (..)
  , loadConfig
  , mergeWithCli
  )
import LspRecorder.Record (runRecord)
import LspRecorder.Replay (ReplayConfig (..), runReplay)
import LspRecorder.Replay.Timing (realisticStrategy)
import System.Directory.OsPath (getCurrentDirectory, makeAbsolute)
import System.IO (BufferMode (..), hSetBuffering, stderr, stdin, stdout)
import System.OsPath (OsPath, encodeFS)

-- | Encode a 'FilePath' string to 'OsPath' and resolve it to an absolute path.
resolvePath :: FilePath -> IO OsPath
resolvePath s = encodeFS s >>= makeAbsolute

main :: IO ()
main = do
  callerCwd <- getCurrentDirectory
  cmd <- parseCommand
  hSetBuffering stdin LineBuffering
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  case cmd of
    CmdRecord opts@RecordOpts{roConfig} -> do
      fileCfg <- case roConfig of
        Nothing ->
          pure
            RecordConfigFile
              { cfServerCommand = Nothing
              , cfTraceOut = Nothing
              , cfProjectRoot = Nothing
              , cfSnapshot = Nothing
              , cfExport = Nothing
              }
        Just path -> resolvePath path >>= loadConfig
      mergeResult <- mergeWithCli fileCfg opts
      case mergeResult of
        Left err -> fail err
        Right cfg -> runRecord cfg
    CmdReplay
      ReplayOpts
        { rpTrace
        , rpServerCommand
        , rpReport
        , rpTimeout
        , rpSpeedupFactor
        , rpNoRestore
        , rpNoFileSync
        , rpConfig
        } -> do
        fileCfg <- case rpConfig of
          Nothing ->
            pure
              RecordConfigFile
                { cfServerCommand = Nothing
                , cfTraceOut = Nothing
                , cfProjectRoot = Nothing
                , cfSnapshot = Nothing
                , cfExport = Nothing
                }
          Just path -> resolvePath path >>= loadConfig
        serverCommand <- case rpServerCommand <|> cfServerCommand fileCfg of
          Nothing -> fail "Missing required field: server_command / --server-command"
          Just sc -> pure sc
        traceOs <- resolvePath rpTrace
        reportOs <- resolvePath rpReport
        runReplay
          ReplayConfig
            { rcTrace = traceOs
            , rcServerCommand = serverCommand
            , rcTiming = realisticStrategy
            , rcReplaySpeed = rpSpeedupFactor
            , rcReportPath = reportOs
            , rcTimeoutSeconds = rpTimeout
            , rcSpeedupFactor = rpSpeedupFactor
            , rcNoRestore = rpNoRestore
            , rcNoFileSync = rpNoFileSync
            , rcExport = cfExport fileCfg
            , rcCallerCwd = callerCwd
            }
