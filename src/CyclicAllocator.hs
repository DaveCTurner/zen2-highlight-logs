{-# LANGUAGE RecordWildCards #-}

module CyclicAllocator (CyclicAllocator, newCyclicAllocator, allocate) where

import           Data.Hashable
import qualified Data.HashMap.Strict as HM

data CyclicAllocator k v = CyclicAllocator
  { caQueue     :: [v]
  , caAllocated :: HM.HashMap k v
  } deriving (Show, Eq)

newCyclicAllocator :: [v] -> CyclicAllocator k v
newCyclicAllocator vs = CyclicAllocator
  { caQueue = vs
  , caAllocated = HM.empty
  }

allocate :: (Eq k, Hashable k) => k -> CyclicAllocator k v -> (v, CyclicAllocator k v)
allocate k ca@CyclicAllocator{..} = case HM.lookup k caAllocated of
  Just v  -> (v, ca)
  Nothing -> case caQueue of
    [] -> error "impossible"
    (v:vs) -> (v, CyclicAllocator
      { caQueue     = vs ++ [v]
      , caAllocated = HM.insert k v caAllocated
      })
