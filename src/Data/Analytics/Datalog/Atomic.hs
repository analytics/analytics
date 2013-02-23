{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Datalog.Atomic
  ( Atomic(..)
  ) where

import Control.Applicative
import Data.Analytics.Datalog.Monad
import Data.Analytics.Datalog.Atom
import Data.Analytics.Datalog.Query
import Data.Analytics.Datalog.Row

class Atomic r a b where
  atom :: Int -> Row (a -> b) -> r

-- All Terms are forced to be Entities
instance (u ~ (), v ~ ()) => Atomic (DatalogT m u) v b where
  atom t a = atom t a :- pure ()

instance a ~ b => Atomic (Query a) b c where
  atom t a = Value (atom t a)

instance (a ~ c, b ~ d) => Atomic (Atom a b) c d where
  atom = Atom
