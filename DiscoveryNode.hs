module DiscoveryNode where

import qualified Data.ByteString as B
import Data.Attoparsec.ByteString as AP
import Data.Word8
import Data.Word
import Control.Monad
import Data.Either
import Control.Applicative
import qualified Data.Text.Encoding as T
import qualified Data.Text as T

data DiscoveryNode = DiscoveryNode
  { dnName        :: B.ByteString
  , dnUUID        :: B.ByteString
  } deriving (Show, Eq)

discoveryNode :: AP.Parser DiscoveryNode
discoveryNode = DiscoveryNode
  <$> discoveryNodePart
  <*> discoveryNodePart

discoveryNodePart :: AP.Parser B.ByteString
discoveryNodePart = AP.word8 _braceleft *> AP.takeWhile (/= _braceright) <* AP.word8 _braceright
