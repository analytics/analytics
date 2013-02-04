{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------
-- |
-- Module    :  Data.Analytics.Internal.Atomic
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Internal.Atomic
  ( Atomic(..)
  ) where

import Data.Analytics.Internal.Datalog
import Data.Analytics.Internal.Query
import Data.Analytics.Internal.Atom

class Atomic r a where
  atom :: Atom a -> r

-- All Terms are forced to be Entities
instance u ~ () => Atomic (DatalogT m u) b where
  atom = Fact . atom

instance a ~ b => Atomic (Query a) b where
  atom = Select . atom

instance a ~ b => Atomic (Atom a) b where
  atom = id
