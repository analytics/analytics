module Data.Analytics.Approximate.Set.HyperLogLog
  (
  -- * HyperLogLog
    HyperLogLog
  , HasHyperLogLog(..)
  , size, intersectionSize
  , cast
  -- * Config
  , Config
  , reifyConfig
  , hll
  ) where

import Data.Analytics.Approximate.Set.HyperLogLog.Internal
