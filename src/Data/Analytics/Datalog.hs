--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Datalog
  ( module Data.Analytics.Datalog.Monad
  , module Data.Analytics.Datalog.Query
  , module Data.Analytics.Datalog.Table
  , module Data.Analytics.Datalog.Term
  ) where

import Data.Analytics.Datalog.Monad
import Data.Analytics.Datalog.Query
import Data.Analytics.Datalog.Table
import Data.Analytics.Datalog.Term

{-# ANN module "HLint: ignore Use import/export shortcut" #-}
