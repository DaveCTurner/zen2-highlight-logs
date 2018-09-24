module Timestamp where

import           Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as B
import           Data.Word8

bracketed :: AP.Parser a -> AP.Parser a
bracketed p = AP.word8 _bracketleft *> p <* AP.word8 _bracketright

digit :: AP.Parser Word8
digit = AP.satisfy (\w -> _0 <= w && w <= _9)

thenDigits :: Word8 -> Int -> AP.Parser B.ByteString
thenDigits w n = B.pack <$> ((:) <$> AP.word8 w <*> AP.count n digit)

timestamp :: AP.Parser B.ByteString
timestamp = B.concat <$> sequence
  [ B.pack <$> AP.count 4 digit
  , _hyphen      `thenDigits` 2
  , _hyphen      `thenDigits` 2
  , _T           `thenDigits` 2
  , _colon       `thenDigits` 2
  , _colon       `thenDigits` 2
  , _comma       `thenDigits` 3
  ]
