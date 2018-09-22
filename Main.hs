{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Options.Applicative
import Data.Conduit
import Data.Conduit.Combinators as DCC
import Control.Monad.Trans.Resource
import qualified Data.ByteString as B
import Data.Word
import Control.Arrow (second)
import Data.Maybe
import Control.Monad.IO.Class
import Text.Printf
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import System.Console.ANSI
import qualified Data.HashMap.Strict as HM

import FirstLine
import Parsers
import DiscoveryNode

data CmdLineArgs = CmdLineArgs
  { claLogFile :: FilePath
  } deriving (Show, Eq)

cmdLineArgs :: Parser CmdLineArgs
cmdLineArgs = CmdLineArgs
  <$> (strArgument $ metavar "FILE")

cmdLineArgsInfo :: ParserInfo CmdLineArgs
cmdLineArgsInfo = info (cmdLineArgs <**> helper)
      ( fullDesc
     <> progDesc "Colourize Zen2 CoordinatorTest logs"
     <> header "zen2-highlight-logs - colourize Zen2 CoordinatorTest logs" )

type NumberedLine = (Word64, B.ByteString)

numberLines :: Monad m => ConduitT B.ByteString NumberedLine m ()
numberLines = go 1
  where
    go n = awaitForever $ \l -> yield (n, l) >> (go $! n+1)

gradleIndent :: B.ByteString
gradleIndent = "  1> "

removeGradleIndent :: Monad m => ConduitT NumberedLine NumberedLine m ()
removeGradleIndent = DCC.filter (B.isPrefixOf gradleIndent . snd)
                  .| DCC.map (second $ B.drop $ B.length gradleIndent)

data CombinedLines = CombinedLines
  { clStart             :: Word64
  , clEnd               :: Word64
  , clFirstLine         :: FirstLine
  , clContinuationLines :: [B.ByteString]
  } deriving (Show, Eq)

combineContinuationLines :: Monad m => ConduitT NumberedLine CombinedLines m ()
combineContinuationLines = awaitForever $ \(n,l) -> startBlock n $ fromMaybe (unreadableFirstLine l) $ tryParseFirstLine l
  where
    startBlock n fl = go CombinedLines {clStart = n, clEnd = n, clFirstLine = fl, clContinuationLines = []}

    go currentBlock@CombinedLines{..} = await >>= \case
      Nothing    -> yield currentBlock
      Just (n,l) -> case tryParseFirstLine l of
        Nothing -> go currentBlock
          { clEnd = n
          , clContinuationLines = clContinuationLines ++ [l]
          }
        Just fl -> yield currentBlock >> startBlock n fl 

data MessageSource
  = SourceNode DiscoveryNode [SGR]
  | TestFixture
  | AdvanceTime
  | RunTask DiscoveryNode [SGR]
  | Unknown
  deriving (Show, Eq)

data DecoratedLines = DecoratedLines
  { dlMillis :: Word64
  , dlSource :: MessageSource
  , dlLines  :: CombinedLines
  } deriving (Show, Eq)

main :: IO ()
main = do
  CmdLineArgs{..} <- execParser cmdLineArgsInfo

  runResourceT $ runConduit
    $  sourceFile claLogFile
    .| linesUnboundedAscii
    .| numberLines
    .| removeGradleIndent
    .| combineContinuationLines
    .| decorateLines
    .| DCC.mapM_ (liftIO . displayLine)

displayLine :: DecoratedLines -> IO ()
displayLine DecoratedLines{..} = do
  setSGR sgrs
  putStrLn $ printf "[%9dms][%-5s][%-40s] %s" dlMillis src (showBs $ flComponent $ clFirstLine dlLines) (showBs $ flMessage $ clFirstLine dlLines)
  setSGR []
  where
    sgrs = case dlSource of
      Unknown     -> [SetColor Foreground Dull Black, SetColor Background Dull Red]
      TestFixture -> [SetColor Foreground Vivid White, SetConsoleIntensity BoldIntensity]
      AdvanceTime -> [SetColor Foreground Vivid Black]
      SourceNode _ sgrs -> sgrs
      RunTask    _ sgrs -> SetConsoleIntensity BoldIntensity : sgrs

    src = case dlSource of
      SourceNode n _ -> showBs $ dnName n
      RunTask    n _ -> showBs $ dnName n
      _              -> ""

    showBs :: B.ByteString -> String
    showBs = T.unpack . T.decodeUtf8

nodeSGRs :: [[SGR]]
nodeSGRs = cycle
  [ [SetColor Foreground Dull Red]
  , [SetColor Foreground Dull Green]
  , [SetColor Foreground Dull Yellow]
  , [SetColor Foreground Vivid Blue]
  , [SetColor Foreground Dull Magenta]
  , [SetColor Foreground Dull Cyan]
  ]

decorateLines :: Monad m => ConduitT CombinedLines DecoratedLines m ()
decorateLines = go Nothing 0 HM.empty nodeSGRs
  where

    go currentNode currentTime sgrsByNode sgrQueue = awaitForever $ \cl@CombinedLines{..} -> let
      newTime = case ("DeterministicTaskQueue" `B.isSuffixOf` flComponent clFirstLine
                     ,tryParseAdvanceTime $ flMessage clFirstLine
                     ) of

                   (True, Just t) -> t
                   _              -> currentTime

      newNode = case ("DeterministicTaskQueue" `B.isSuffixOf` flComponent clFirstLine
                     , tryParseRunningTaskNode $ flMessage clFirstLine
                     ) of
                  (True, Just n) -> Just n
                  _ -> currentNode

      (runTask, sourceNode, sgrsByNode', sgrQueue') = case newNode of
        Nothing -> (Unknown, Unknown, sgrsByNode, sgrQueue)
        Just n -> case HM.lookup (dnUUID n) sgrsByNode of
          Just sgrs -> (RunTask n sgrs, SourceNode n sgrs, sgrsByNode, sgrQueue)
          Nothing -> case sgrQueue of
            [] -> error "impossible"
            (sgrs:sgrss') -> (RunTask n sgrs, SourceNode n sgrs, HM.insert (dnUUID n) sgrs sgrsByNode, sgrss')

      source | "Tests" `B.isSuffixOf` flComponent clFirstLine                  = TestFixture
             | "DeterministicTaskQueue" `B.isSuffixOf` flComponent clFirstLine
             , "advanceTime"            `B.isPrefixOf` flMessage   clFirstLine = AdvanceTime
             | "DeterministicTaskQueue" `B.isSuffixOf` flComponent clFirstLine
             , "running task "          `B.isPrefixOf` flMessage   clFirstLine = runTask
             | currentNode == flNodeId clFirstLine = sourceNode
             | otherwise = Unknown

      in do yield DecoratedLines { dlMillis = newTime, dlSource = source, dlLines = cl }
            go newNode newTime sgrsByNode' sgrQueue'
