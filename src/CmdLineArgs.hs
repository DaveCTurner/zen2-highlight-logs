module CmdLineArgs (CmdLineArgs(..), getCmdLineArgs, parseCmdLineArgs) where

import           Options.Applicative

data CmdLineArgs = CmdLineArgs
  { claContinuationLines :: Bool
  , claLineNumbers       :: Bool
  , claTimestamp         :: Bool
  , claLogLevel          :: Bool
  , claLogFile           :: FilePath
  } deriving (Show, Eq)

cmdLineArgs :: Parser CmdLineArgs
cmdLineArgs = CmdLineArgs
  <$> (switch $ long "continuation-lines" <> help "Also render continuation lines")
  <*> (switch $ long "line-numbers"       <> help "Include original line numbers")
  <*> (switch $ long "original-timestamp" <> help "Include original timestamp")
  <*> (switch $ long "log-level"          <> help "Include log level")
  <*> (strArgument $ metavar "FILE" <> help "Test output log file")

cmdLineArgsInfo :: ParserInfo CmdLineArgs
cmdLineArgsInfo = info (cmdLineArgs <**> helper)
      ( fullDesc
     <> progDesc "Colourize Zen2 CoordinatorTest logs"
     <> header "zen2-highlight-logs - colourize Zen2 CoordinatorTest logs" )

getCmdLineArgs :: IO CmdLineArgs
getCmdLineArgs = execParser cmdLineArgsInfo

parseCmdLineArgs :: [String] -> Maybe CmdLineArgs
parseCmdLineArgs args = case execParserPure defaultPrefs cmdLineArgsInfo args of
  Success a -> Just a
  _         -> Nothing
