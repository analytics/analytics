--------------------------------------------------------------------
-- |
-- Module    :  Data.Analytics
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics
  ( module Data.Analytics.Match
  , module Data.Analytics.Datalog
  , module Data.Analytics.Query
  , module Data.Analytics.Relation
  ) where

import Data.Analytics.Datalog
import Data.Analytics.Match
import Data.Analytics.Query
import Data.Analytics.Relation

{-# ANN module "HLint: ignore Use import/export shortcut" #-}
