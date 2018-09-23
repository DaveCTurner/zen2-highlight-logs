{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module DecoratedLines where

import           Control.Monad
import           Control.Monad.Except
import           Control.Monad.State.Lazy
import qualified Data.ByteString          as B
import           Data.Conduit
import           Data.Conduit.Combinators as DCC
import           Data.Void
import           Data.Word
import           System.Console.ANSI

import           CombinedLines
import           CyclicAllocator
import           DiscoveryNode
import           FirstLine
import           Parsers

data MessageSource
  = SourceNode DiscoveryNode [SGR]
  | RunTask    DiscoveryNode [SGR]
  | TestFixture
  | AdvanceTime
  | Unknown
  deriving (Show, Eq)

data DecoratedLines = DecoratedLines
  { dlMillis :: Word64
  , dlSource :: MessageSource
  , dlLines  :: CombinedLines
  } deriving (Show, Eq)

nodeSGRs :: [[SGR]]
nodeSGRs =
  [ [SetColor Foreground Dull  Red]
  , [SetColor Foreground Dull  Green]
  , [SetColor Foreground Dull  Yellow]
  , [SetColor Foreground Vivid Blue]
  , [SetColor Foreground Dull  Magenta]
  , [SetColor Foreground Dull  Cyan]
  ]

data DecoratorState = DecoratorState
  { dsCurrentNode  :: Maybe DiscoveryNode
  , dsCurrentTime  :: Word64
  , dsSGRAllocator :: CyclicAllocator B.ByteString [SGR]
  } deriving (Show, Eq)

decorateLines :: Monad m => ConduitT CombinedLines DecoratedLines m ()
decorateLines = void $ mapAccumWhile decorateLine DecoratorState
  { dsCurrentNode  = Nothing
  , dsCurrentTime  = 0
  , dsSGRAllocator = newCyclicAllocator nodeSGRs
  }

decorateLine :: CombinedLines -> DecoratorState -> Either a (DecoratorState, DecoratedLines)
decorateLine cl ds = case runState (runExceptT $ processLine cl) ds of
  (Right v, _)    -> absurd v
  (Left src, ds') -> Right (ds', DecoratedLines
                                    { dlMillis = dsCurrentTime ds'
                                    , dlSource = src
                                    , dlLines  = cl
                                    })

singleNodeSource :: (DiscoveryNode -> [SGR] -> MessageSource) -> DiscoveryNode -> ExceptT MessageSource (State DecoratorState) a
singleNodeSource c n = do
  modify $ \ds -> ds { dsCurrentNode = Just n }
  sgrs <- allocateSGRs n
  throwError $ c n sgrs

allocateSGRs :: DiscoveryNode -> ExceptT MessageSource (State DecoratorState) [SGR]
allocateSGRs n = do
  (v, a) <- gets $ allocate (dnUUID n) . dsSGRAllocator
  modify $ \ds -> ds { dsSGRAllocator = a }
  return v

setTime :: Word64 -> ExceptT MessageSource (State DecoratorState) ()
setTime t = modify $ \ds -> ds { dsCurrentTime = t }

maybeM :: Monad m => (a -> m ()) -> Maybe a -> m ()
maybeM = maybe (return ())

processLine :: CombinedLines -> ExceptT MessageSource (State DecoratorState) a
processLine CombinedLines{clFirstLine=FirstLine{..},..} = do

  when ("Tests" `B.isSuffixOf` flComponent) $ throwError TestFixture

  when ("DeterministicTaskQueue" `B.isSuffixOf` flComponent) $ do
    maybeM setTime $ tryParseAdvanceTime flMessage
    when ("advanceTime" `B.isPrefixOf` flMessage) $ throwError AdvanceTime
    maybeM (singleNodeSource RunTask) $ tryParseRunningTaskNode flMessage

  currentNode <- gets dsCurrentNode

  case (currentNode, flNodeId) of
    (Nothing, Just n) -> do
      sgrs <- allocateSGRs n
      throwError $ SourceNode n sgrs
    (Just n,  Just m) | m == n -> singleNodeSource SourceNode n
    _ -> return ()

  throwError Unknown

