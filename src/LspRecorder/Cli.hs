module LspRecorder.Cli
  ( Command (..)
  , RecordOpts (..)
  , ReplayOpts (..)
  , TimingMode (..)
  , parseCommand
  ) where

import Options.Applicative

data Command = CmdRecord RecordOpts | CmdReplay ReplayOpts

data RecordOpts = RecordOpts
  { roServerCommand :: Maybe String
  , roTraceOut :: Maybe FilePath
  , roProjectRoot :: Maybe FilePath
  , roConfig :: Maybe FilePath
  }

data TimingMode = Realistic | Immediate
  deriving stock (Eq, Show)

data ReplayOpts = ReplayOpts
  { rpTrace :: FilePath
  , rpServerCommand :: String
  , rpTiming :: TimingMode
  , rpReport :: FilePath
  , rpTimeout :: Int
  , rpNoRestore :: Bool
  , rpNoFileSync :: Bool
  }

parseCommand :: IO Command
parseCommand = execParser (info (commandParser <**> helper) (fullDesc <> progDesc "LSP recorder and replayer"))

commandParser :: Parser Command
commandParser =
  subparser
    ( command "record" (info (CmdRecord <$> recordOpts) (progDesc "Record LSP session to a trace file"))
        <> command
          "replay"
          (info (CmdReplay <$> replayOpts) (progDesc "Replay a trace file against a language server"))
    )

recordOpts :: Parser RecordOpts
recordOpts =
  RecordOpts
    <$> optional
      ( strOption
          ( long "server-command"
              <> metavar "CMD"
              <> help "Shell command to launch the language server"
          )
      )
    <*> optional
      ( strOption
          ( long "trace-out"
              <> metavar "FILE"
              <> help "Output path for the JSONL trace file"
          )
      )
    <*> optional
      ( strOption
          ( long "project-root"
              <> metavar "DIR"
              <> help "Project root directory"
          )
      )
    <*> optional
      ( strOption
          ( long "config"
              <> metavar "FILE"
              <> help "Path to a JSON config file (CLI flags override config file values)"
          )
      )

replayOpts :: Parser ReplayOpts
replayOpts =
  ReplayOpts
    <$> strOption
      ( long "trace"
          <> metavar "FILE"
          <> help "Path to the JSONL trace file to replay"
      )
    <*> strOption
      ( long "server-command"
          <> metavar "CMD"
          <> help "Shell command to launch the language server"
      )
    <*> option
      timingModeReader
      ( long "timing"
          <> metavar "MODE"
          <> value Immediate
          <> showDefaultWith (const "immediate")
          <> help "Timing mode: realistic or immediate"
      )
    <*> strOption
      ( long "report"
          <> metavar "FILE"
          <> help "Output path for the JSON performance report"
      )
    <*> option
      auto
      ( long "timeout"
          <> metavar "SECONDS"
          <> value 60
          <> showDefault
          <> help "Per-request timeout in seconds (0 = no timeout)"
      )
    <*> switch
      ( long "no-restore"
          <> help "Skip snapshot extraction; run server in current working directory"
      )
    <*> switch
      ( long "no-file-sync"
          <> help "Skip applying file changes to disk during replay"
      )

timingModeReader :: ReadM TimingMode
timingModeReader = eitherReader $ \case
  "realistic" -> Right Realistic
  "immediate" -> Right Immediate
  s -> Left $ "Unknown timing mode: " <> s <> " (expected: realistic, immediate)"
