--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
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
