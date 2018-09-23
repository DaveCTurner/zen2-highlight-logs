{-# LANGUAGE OverloadedStrings #-}

module GradleExtractor (extractLogFromGradleOutput) where

import           Control.Arrow            (second)
import qualified Data.ByteString          as B
import           Data.Conduit
import           Data.Conduit.Combinators as DCC

import           NumberLines

gradleIndent :: B.ByteString
gradleIndent = "  1> "

extractLogFromGradleOutput :: Monad m => ConduitT NumberedLine NumberedLine m ()
extractLogFromGradleOutput
  =  DCC.filter (B.isPrefixOf gradleIndent . snd)
  .| DCC.map (second $ B.drop $ B.length gradleIndent)
