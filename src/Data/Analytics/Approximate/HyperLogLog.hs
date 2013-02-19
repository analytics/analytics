--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Approximate.HyperLogLog
  (
  -- * HyperLogLog
    HyperLogLog
  , HasHyperLogLog(..)
  , size
  , intersectionSize
  , cast
  -- * Config
  , Config
  , hll
  -- * ReifiesConfig
  , ReifiesConfig
  , reifyConfig
  ) where

import Data.Analytics.Approximate.HyperLogLog.Config
import Data.Analytics.Approximate.HyperLogLog.Type
