module Main where

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
        Just path -> loadConfig path
      case mergeWithCli fileCfg opts of
        Left err -> fail err
        Right cfg -> runRecord cfg
    CmdReplay ReplayOpts{rpTrace, rpServerCommand, rpTiming, rpReport, rpTimeout, rpNoRestore} ->
      let
        (strategy, modeName) = case rpTiming of
          Immediate -> (immediateStrategy, "immediate")
          Realistic -> (realisticStrategy, "realistic")
       in
        runReplay
          ReplayConfig
            { rcTrace = rpTrace
            , rcServerCommand = rpServerCommand
            , rcTiming = strategy
            , rcTimingModeName = modeName
            , rcReportPath = rpReport
            , rcTimeoutSeconds = rpTimeout
            , rcNoRestore = rpNoRestore
            }
