{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module LibMain where

import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import qualified Data.ByteString              as B
import           Data.Conduit
import           Data.Conduit.Combinators     as DCC
import qualified Data.Text                    as T
import qualified Data.Text.Encoding           as T
import           System.Console.ANSI
import           Text.Printf
import qualified Data.HashSet as HS

import           CmdLineArgs
import           CombinedLines
import           DecoratedLines
import           DiscoveryNode
import           FirstLine
import           GradleExtractor
import           NumberLines

main :: IO ()
main = do
  cmdLineArgs <- getCmdLineArgs

  runResourceT $ runConduit
    $  sourceFile (claLogFile cmdLineArgs)
    .| splitAndNumberLines
    .| extractLogFromOutput
    .| combineContinuationLines
    .| decorateLines
    .| DCC.mapM_ (liftIO . displayLine cmdLineArgs)

displayLine :: CmdLineArgs -> DecoratedLines -> IO ()
displayLine CmdLineArgs{..} DecoratedLines{..} = when nodeMatches $ do
  setSGR sgrs
  let lineNumber = if claLineNumbers then printf "[%5d]"  (clStart dlLines) else "" :: String
      timestamp  = if claTimestamp   then printf "[%s]"   (showBs $ flTimestamp $ clFirstLine dlLines) else "" :: String
      relativeTimeStamp = case claRelativeTime of
        Nothing -> "" :: String
        Just t0 | t0 <= dlMillis -> printf "[+%9dms]"  (dlMillis - t0)
                | otherwise      -> printf "[-%9dms]" (t0 - dlMillis)
      logLevel   = if claLogLevel    then printf "[%-5s]" (showBs $ flLevel     $ clFirstLine dlLines) else "" :: String
  putStrLn $ printf "%s%s[%9dms]%s[%-5s]%s[%-42s] %s"
                     lineNumber
                         timestamp
                                dlMillis
                                  relativeTimeStamp
                                        src
                                          logLevel
                                                   (showBs $ flComponent $ clFirstLine dlLines) (showBs $ flMessage $ clFirstLine dlLines)
  when claContinuationLines $ forM_ (clContinuationLines dlLines) $ \l -> setSGR sgrs >> putStrLn (showBs l)
  setSGR []
  where
    nodeMatches = case (HS.null claOnlyNodes, nodeFromMessageSource dlSource) of
      (False, Just n) -> dnName n `HS.member` claOnlyNodes
      _ -> True

    sgrs = case dlSource of
      Unknown     -> [SetColor Foreground Dull Black, SetColor Background Dull Red]
      TestFixture -> [SetColor Foreground Vivid White, SetConsoleIntensity BoldIntensity]
      AdvanceTime -> [SetColor Foreground Vivid Black]
      SourceNode _ sgrs' -> sgrs'
      RunTask    _ sgrs' -> SetConsoleIntensity BoldIntensity : sgrs'

    src = case dlSource of
      SourceNode n _ -> showBs $ dnName n
      RunTask    n _ -> showBs $ dnName n
      _              -> ""

    showBs :: B.ByteString -> String
    showBs = T.unpack . T.decodeUtf8
