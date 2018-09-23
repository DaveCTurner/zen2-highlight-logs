{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

module Parsers (tryParseAdvanceTime, tryParseRunningTaskNode) where

import           Control.Monad
import           Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as B
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import           Data.Word
import           Data.Word8

import           DiscoveryNode

skipString :: T.Text -> AP.Parser ()
skipString = void . AP.string . T.encodeUtf8

tryParseAdvanceTime :: B.ByteString -> Maybe Word64
tryParseAdvanceTime = either (const Nothing) Just . AP.parseOnly advanceTime

advanceTime :: AP.Parser Word64
advanceTime = do
  skipString "advanceTime: from ["
  AP.skipWhile isDigit
  skipString "ms] to ["
  ds <- AP.takeWhile isDigit
  skipString "ms]"
  endOfInput
  return $ read $ T.unpack $ T.decodeUtf8 ds

tryParseRunningTaskNode :: B.ByteString -> Maybe DiscoveryNode
tryParseRunningTaskNode = either (const Nothing) Just . AP.parseOnly runningTaskNode

runningTaskNode :: AP.Parser DiscoveryNode
runningTaskNode = do
  skipString "running task "
  AP.skipWhile isDigit
  skipString " of "
  AP.skipWhile isDigit
  skipString ": "
  discoveryNode <* skipString ": "
