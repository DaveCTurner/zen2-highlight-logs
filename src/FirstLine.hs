{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module FirstLine (tryParseFirstLine, FirstLine(..), unreadableFirstLine) where

import           Control.Applicative
import           Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as B
import           Data.Word8

import           DiscoveryNode

data FirstLine = FirstLine
  { flTimestamp :: B.ByteString
  , flLevel     :: B.ByteString
  , flComponent :: B.ByteString
  , flTest      :: B.ByteString
  , flNodeId    :: Maybe DiscoveryNode
  , flMessage   :: B.ByteString
  } deriving (Show, Eq)

tryParseFirstLine :: B.ByteString -> Maybe FirstLine
tryParseFirstLine bs = either (const Nothing) Just $ parseOnly firstLine bs

firstLine :: AP.Parser FirstLine
firstLine = do
  flTimestamp        <- bracketed timestamp
  flLevel            <- bracketed level
  flComponent        <- bracketed component        <* AP.word8 _space
  (flTest, flNodeId) <- bracketed testAndMaybeNode <* AP.word8 _space
  flMessage          <- AP.takeByteString
  return FirstLine{..}

testAndMaybeNode :: AP.Parser (B.ByteString, Maybe DiscoveryNode)
testAndMaybeNode = (,)
  <$> AP.takeWhile (not . (`elem` [_bracketright, _braceleft, _space]))
  <*> optional (AP.string "{nodeId=" *> discoveryNode <* AP.word8 _braceright)
  <*  many (AP.word8 _space)

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

level :: AP.Parser B.ByteString
level = B.filter (/= _space) <$> AP.takeWhile (/= _bracketright)

component :: AP.Parser B.ByteString
component = B.filter (/= _space) <$> AP.takeWhile (/= _bracketright)

unreadableFirstLine :: B.ByteString -> FirstLine
unreadableFirstLine bs = FirstLine
  { flTimestamp = unreadable
  , flLevel     = unreadable
  , flComponent = unreadable
  , flTest      = unreadable
  , flNodeId    = Nothing
  , flMessage   = bs
  } where unreadable = B.pack [_question, _question]
