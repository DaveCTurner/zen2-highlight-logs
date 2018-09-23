module DiscoveryNode (DiscoveryNode(..), discoveryNode) where

import           Data.Attoparsec.ByteString as AP
import qualified Data.ByteString            as B
import           Data.Word8

data DiscoveryNode = DiscoveryNode
  { dnName :: B.ByteString
  , dnUUID :: B.ByteString
  } deriving (Show, Eq)

discoveryNode :: AP.Parser DiscoveryNode
discoveryNode = DiscoveryNode
  <$> discoveryNodePart
  <*> discoveryNodePart

discoveryNodePart :: AP.Parser B.ByteString
discoveryNodePart = AP.word8 _braceleft *> AP.takeWhile (/= _braceright) <* AP.word8 _braceright
