module CmdLineArgs (CmdLineArgs(..), getCmdLineArgs, parseCmdLineArgs) where

import           Options.Applicative
import qualified Data.HashSet as HS
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Data.Word8
import Data.Word

data CmdLineArgs = CmdLineArgs
  { claContinuationLines :: Bool
  , claLineNumbers       :: Bool
  , claTimestamp         :: Bool
  , claLogLevel          :: Bool
  , claOnlyNodes         :: HS.HashSet B.ByteString
  , claRelativeTime      :: Maybe Word64
  , claLogFile           :: FilePath
  } deriving (Show, Eq)

cmdLineArgs :: Parser CmdLineArgs
cmdLineArgs = CmdLineArgs
  <$> (switch $ long "continuation-lines" <> help "Also render continuation lines")
  <*> (switch $ long "line-numbers"       <> help "Include original line numbers")
  <*> (switch $ long "original-timestamp" <> help "Include original timestamp")
  <*> (switch $ long "log-level"          <> help "Include log level")
  <*> (HS.unions . (map setFromCommaSeparatedString) <$> many (strOption
              $ long "only-nodes"         <> help "Only show output from the listed nodes"))
  <*> (optional $ option auto
              $ long "relative-time"      <> help "Include times relative to the given timepoint")
  <*> (strArgument $ metavar "FILE"       <> help "Test output log file")

setFromCommaSeparatedString :: String -> HS.HashSet B.ByteString
setFromCommaSeparatedString s = HS.fromList . B.split _comma . T.encodeUtf8 $ T.pack s

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
