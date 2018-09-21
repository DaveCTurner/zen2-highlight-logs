{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE OverloadedStrings #-}

module Parsers (tryParseWrappedFor, tryParseAdvanceTime, DiscoveryNode(..)) where

import qualified Data.ByteString as B
import Data.Attoparsec.ByteString as AP
import Data.Word8
import Data.Word
import Control.Monad
import Data.Either
import Control.Applicative
import qualified Data.Text.Encoding as T
import qualified Data.Text as T

tryParseAdvanceTime :: B.ByteString -> Maybe Word64
tryParseAdvanceTime = either (const Nothing) Just . AP.parseOnly advanceTime

advanceTime :: AP.Parser Word64
advanceTime = do
  void $ AP.string $ T.encodeUtf8 "advanceTime: from ["
  AP.skipWhile isDigit
  void $ AP.string $ T.encodeUtf8 "ms] to ["
  ds <- AP.takeWhile isDigit
  void $ AP.string $ T.encodeUtf8 "ms]"
  endOfInput
  return $ read $ T.unpack $ T.decodeUtf8 ds

data DiscoveryNode = DiscoveryNode
  { dnName        :: B.ByteString
  , dnUUID        :: B.ByteString
  , dnHostAddress :: B.ByteString
  , dnAddress     :: B.ByteString
  } deriving (Show, Eq)

wrapperStart :: B.ByteString
wrapperStart = T.encodeUtf8 " (wrapped for "

tryParseWrappedFor :: B.ByteString -> Maybe DiscoveryNode
tryParseWrappedFor bs = case (B.null toParse, tryParseWrappedFor (B.drop 1 toParse)) of
    (True, _)    -> Nothing
    (_, Just dn) -> Just dn
    _            -> either (const Nothing) Just $ AP.parseOnly wrappedFor toParse
  where
    toParse = snd $ B.breakSubstring wrapperStart bs

wrappedFor :: AP.Parser DiscoveryNode
wrappedFor = do
  void $ AP.string wrapperStart
  DiscoveryNode
    <$> discoveryNodePart
    <*> discoveryNodePart
    <*> discoveryNodePart
    <*> discoveryNodePart
    <*  AP.word8 _parenright
    <* endOfInput

discoveryNodePart :: AP.Parser B.ByteString
discoveryNodePart = AP.word8 _braceleft *> AP.takeWhile (/= _braceright) <* AP.word8 _braceright
