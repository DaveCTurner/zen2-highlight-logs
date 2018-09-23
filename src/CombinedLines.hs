{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE RecordWildCards #-}

module CombinedLines where

import qualified Data.ByteString as B
import           Data.Conduit
import           Data.Maybe
import           Data.Word

import           FirstLine
import           NumberLines

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
