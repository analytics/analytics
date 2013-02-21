{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Datalog.Atom
  ( Atom(..)
  ) where

import Data.Analytics.Datalog.Row
import Data.Analytics.Datalog.Term
import Data.Typeable

data Atom :: * -> * -> * -> * where
  -- Atom :: (b -> a) -> t -> Row b -> Atom t a
  Atom :: t -> Row (a -> b) -> Atom t a b
  deriving Typeable

{-
instance Functor (Atom t) where
  fmap f (Atom k t h) = Atom (f . k) t h
-}

instance Term x => TermOf (Atom t a b) x
