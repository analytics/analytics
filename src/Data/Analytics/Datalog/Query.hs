{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------
-- |
-- Module    :  Data.Analytics.Datalog.Query
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Datalog.Query
  ( Query(..)
  , no
  ) where

import Data.Analytics.Datalog.Atom
import Data.Analytics.Datalog.Term
import Control.Applicative
import Data.Functor.Bind
import Data.Functor.Plus
import Data.Semigroup
import Data.Typeable

------------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------------

-- | This models a strongly typed query or the right hand side of a 'Data.Analytics.Datalog.Datalog' rule.
--
-- The 'Query' itself forms an 'Alternative', letting you combine them to make a robust
-- 'Data.Analytics.Datalog.query' language.
data Query :: * -> * -> * where
  Ap     :: Query t (a -> b) -> Query t a -> Query t b
  Map    :: (a -> b) -> Query t a -> Query t b
  Pure   :: a -> Query t a
  Alt    :: Query t a -> Query t a -> Query t a
  Empty  :: Query t a
  Select :: Atom t a -> Query t a
  No     :: Atom t a -> Query t ()
  deriving Typeable

instance Functor (Query t) where
  fmap = Map
  {-# INLINE fmap #-}

instance Apply (Query t) where
  (<.>) = Ap
  {-# INLINE (<.>) #-}

instance Applicative (Query t) where
  pure = Pure
  {-# INLINE pure #-}
  (<*>) = Ap
  {-# INLINE (<*>) #-}

instance Alt (Query t) where
  (<!>) = Alt
  {-# INLINE (<!>) #-}

instance Plus (Query t) where
  zero = Empty
  {-# INLINE zero #-}

instance Alternative (Query t) where
  empty = Empty
  {-# INLINE empty #-}
  (<|>) = Alt
  {-# INLINE (<|>) #-}

instance Semigroup a => Semigroup (Query t a) where
  (<>) = liftA2 (<>)
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (Query t a) where
  mempty = pure mempty
  {-# INLINE mempty #-}
  mappend = liftA2 mappend
  {-# INLINE mappend #-}

instance Term x => TermOf (Query t a) x

-- | Stratified negation.
no :: Atom t a -> Query t ()
no = No
{-# INLINE no #-}
