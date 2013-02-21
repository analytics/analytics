{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
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

class Atomic r a b | r -> a b where
  atom :: Int -> Row (a -> b) -> r

-- All Terms are forced to be Entities
instance u ~ () => Atomic (DatalogT m u) () b where
  atom t a = atom t a :- pure ()

instance Atomic (Query Body a) a b where
  atom t a = Value (atom t a)

instance Atomic (Query Request b) a b where
  atom t a = Row (atom t a)

instance Atomic (Atom a b) a b where
  atom = Atom
