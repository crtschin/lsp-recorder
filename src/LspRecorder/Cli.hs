module LspRecorder.Cli (
    Command (..),
    RecordOpts (..),
    ReplayOpts (..),
    TimingMode (..),
    parseCommand,
) where

import Options.Applicative

data Command = CmdRecord RecordOpts | CmdReplay ReplayOpts

data RecordOpts = RecordOpts
    { roServerCommand :: String
    , roTraceOut :: FilePath
    , roProjectRoot :: FilePath
    }

data TimingMode = Realistic | Immediate
    deriving stock (Eq, Show)

data ReplayOpts = ReplayOpts
    { rpTrace :: FilePath
    , rpServerCommand :: String
    , rpTiming :: TimingMode
    , rpReport :: FilePath
    }

parseCommand :: IO Command
parseCommand = execParser (info (commandParser <**> helper) (fullDesc <> progDesc "LSP recorder and replayer"))

commandParser :: Parser Command
commandParser =
    subparser
        ( command "record" (info (CmdRecord <$> recordOpts) (progDesc "Record LSP session to a trace file"))
            <> command "replay" (info (CmdReplay <$> replayOpts) (progDesc "Replay a trace file against a language server"))
        )

recordOpts :: Parser RecordOpts
recordOpts =
    RecordOpts
        <$> strOption
            ( long "server-command"
                <> metavar "CMD"
                <> help "Shell command to launch the language server"
            )
        <*> strOption
            ( long "trace-out"
                <> metavar "FILE"
                <> help "Output path for the JSONL trace file"
            )
        <*> strOption
            ( long "project-root"
                <> metavar "DIR"
                <> help "Project root directory"
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

timingModeReader :: ReadM TimingMode
timingModeReader = eitherReader $ \case
    "realistic" -> Right Realistic
    "immediate" -> Right Immediate
    s -> Left $ "Unknown timing mode: " <> s <> " (expected: realistic, immediate)"
