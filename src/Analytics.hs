--------------------------------------------------------------------
-- |
-- Module    :  Analytics
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Analytics
  ( module Analytics.Match
  , module Analytics.Datalog
  , module Analytics.Query
  , module Analytics.Relation
  ) where

import Analytics.Datalog
import Analytics.Match
import Analytics.Query
import Analytics.Relation

{-# ANN module "HLint: ignore Use import/export shortcut" #-}
