module Main where

import Control.Applicative ((<|>))
import LspRecorder.Cli
  ( Command (..)
  , RecordOpts (..)
  , ReplayOpts (..)
  , TimingMode (..)
  , parseCommand
  )
import LspRecorder.Config
  ( RecordConfigFile (..)
  , loadConfig
  , mergeWithCli
  )
import LspRecorder.Record (runRecord)
import LspRecorder.Replay (ReplayConfig (..), runReplay)
import LspRecorder.Replay.Timing (immediateStrategy, realisticStrategy)
import System.Directory.OsPath (makeAbsolute)
import System.OsPath (OsPath, encodeFS)

-- | Encode a 'FilePath' string to 'OsPath' and resolve it to an absolute path.
resolvePath :: FilePath -> IO OsPath
resolvePath s = encodeFS s >>= makeAbsolute

main :: IO ()
main = do
  cmd <- parseCommand
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
        , rpTiming
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
              }
        Just path -> resolvePath path >>= loadConfig
      serverCommand <- case rpServerCommand <|> cfServerCommand fileCfg of
        Nothing -> fail "Missing required field: server_command / --server-command"
        Just sc -> pure sc
      traceOs <- resolvePath rpTrace
      reportOs <- resolvePath rpReport
      let (strategy, modeName) = case rpTiming of
            Immediate -> (immediateStrategy, "immediate")
            Realistic -> (realisticStrategy, "realistic")
      runReplay
        ReplayConfig
          { rcTrace = traceOs
          , rcServerCommand = serverCommand
          , rcTiming = strategy
          , rcTimingModeName = modeName
          , rcReportPath = reportOs
          , rcTimeoutSeconds = rpTimeout
          , rcSpeedupFactor = rpSpeedupFactor
          , rcNoRestore = rpNoRestore
          , rcNoFileSync = rpNoFileSync
          }
