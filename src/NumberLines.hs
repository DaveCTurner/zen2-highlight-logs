module NumberLines (NumberedLine, splitAndNumberLines) where

import qualified Data.ByteString          as B
import           Data.Conduit
import           Data.Conduit.Combinators
import           Data.Word

type NumberedLine = (Word64, B.ByteString)

splitAndNumberLines :: Monad m => ConduitT B.ByteString NumberedLine m ()
splitAndNumberLines = linesUnboundedAscii .| numberLines

numberLines :: Monad m => ConduitT B.ByteString NumberedLine m ()
numberLines = go 1
  where
    go n = awaitForever $ \l -> yield (n, l) >> (go $! n+1)

