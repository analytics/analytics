--------------------------------------------------------------------
-- |
-- Module    :  Data.Analytics.Datalog
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides the 'Datalog' monad transformer.
--------------------------------------------------------------------
module Data.Analytics.Datalog
  ( Datalog
  , DatalogT((:-))
  , query
  ) where

import Data.Analytics.Internal.Datalog

{-# ANN module "HLint: ignore Use import/export shortcut" #-}
