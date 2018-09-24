{-# LANGUAGE OverloadedStrings #-}

module GradleExtractor (extractLogFromOutput) where

import           Control.Arrow              (second)
import           Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as B
import           Data.Conduit
import           Data.Conduit.Combinators   as DCC
import           Data.Either

import           NumberLines
import           Timestamp

extractLogFromOutput :: Monad m => ConduitT NumberedLine NumberedLine m ()
extractLogFromOutput = awaitForever $ \nl@(_,l) ->
  if gradleIndent `B.isPrefixOf` l
    then leftover nl >> extractLogFromGradleOutput
  else if isRight (AP.parseOnly (bracketed timestamp) l)
    then leftover nl >> awaitForever yield
    else return ()

gradleIndent :: B.ByteString
gradleIndent = "  1> "

extractLogFromGradleOutput :: Monad m => ConduitT NumberedLine NumberedLine m ()
extractLogFromGradleOutput
  =  DCC.filter (B.isPrefixOf gradleIndent . snd)
  .| DCC.map (second $ B.drop $ B.length gradleIndent)
