module Main where

import LspRecorder.Cli
  ( Command (..)
  , RecordOpts (..)
  , ReplayOpts (..)
  , TimingMode (..)
  , parseCommand
  )
import LspRecorder.Record (RecordConfig (..), runRecord)
import LspRecorder.Replay (ReplayConfig (..), runReplay)
import LspRecorder.Replay.Timing (immediateStrategy, realisticStrategy)

main :: IO ()
main = do
  cmd <- parseCommand
  case cmd of
    CmdRecord RecordOpts{roServerCommand, roTraceOut, roProjectRoot} ->
      runRecord
        RecordConfig
          { rcServerCommand = roServerCommand
          , rcTraceOut = roTraceOut
          , rcProjectRoot = roProjectRoot
          }
    CmdReplay ReplayOpts{rpTrace, rpServerCommand, rpTiming, rpReport} ->
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
            }
