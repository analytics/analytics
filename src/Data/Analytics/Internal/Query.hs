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
-- Module    :  Data.Analytics.Internal.Query
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Internal.Query
  ( Query(..)
  , no
  ) where

import Data.Analytics.Internal.Atom
import Data.Analytics.Internal.Term
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
data Query a where
  Ap     :: Query (a -> b) -> Query a -> Query b
  Map    :: (a -> b) -> Query a -> Query b
  Pure   :: a -> Query a
  Alt    :: Query a -> Query a -> Query a
  Empty  :: Query a
  Select :: Atom a -> Query a
  No     :: Atom a -> Query ()
  deriving Typeable

instance Functor Query where
  fmap = Map
  {-# INLINE fmap #-}

instance Apply Query where
  (<.>) = Ap
  {-# INLINE (<.>) #-}

instance Applicative Query where
  pure = Pure
  {-# INLINE pure #-}
  (<*>) = Ap
  {-# INLINE (<*>) #-}

instance Alt Query where
  (<!>) = Alt
  {-# INLINE (<!>) #-}

instance Plus Query where
  zero = Empty
  {-# INLINE zero #-}

instance Alternative Query where
  empty = Empty
  {-# INLINE empty #-}
  (<|>) = Alt
  {-# INLINE (<|>) #-}

instance Semigroup a => Semigroup (Query a) where
  (<>) = liftA2 (<>)
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (Query a) where
  mempty = pure mempty
  {-# INLINE mempty #-}
  mappend = liftA2 mappend
  {-# INLINE mappend #-}

instance Term x => TermOf (Query a) x

-- | Stratified negation.
no :: Atom a -> Query ()
no = No
{-# INLINE no #-}


