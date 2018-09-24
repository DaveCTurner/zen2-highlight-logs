{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module FirstLine (tryParseFirstLine, FirstLine(..), unreadableFirstLine) where

import           Control.Applicative
import           Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as B
import           Data.Word8

import           DiscoveryNode
import           Timestamp

data FirstLine = FirstLine
  { flTimestamp :: B.ByteString
  , flLevel     :: B.ByteString
  , flComponent :: B.ByteString
  , flTest      :: B.ByteString
  , flNode      :: Maybe DiscoveryNode
  , flMessage   :: B.ByteString
  } deriving (Show, Eq)

tryParseFirstLine :: B.ByteString -> Maybe FirstLine
tryParseFirstLine bs = either (const Nothing) Just $ parseOnly firstLine bs

firstLine :: AP.Parser FirstLine
firstLine = do
  flTimestamp      <- bracketed timestamp
  flLevel          <- bracketed level
  flComponent      <- bracketed component        <* AP.word8 _space
  (flTest, flNode) <- bracketed testAndMaybeNode <* AP.word8 _space
  flMessage        <- AP.takeByteString
  return FirstLine{..}

testAndMaybeNode :: AP.Parser (B.ByteString, Maybe DiscoveryNode)
testAndMaybeNode = (,)
  <$> AP.takeWhile (not . (`elem` [_bracketright, _braceleft, _space]))
  <*> optional (AP.string "{nodeId=" *> discoveryNode <* AP.word8 _braceright)
  <*  many (AP.word8 _space)

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
  , flNode      = Nothing
  , flMessage   = bs
  } where unreadable = B.pack [_question, _question]
