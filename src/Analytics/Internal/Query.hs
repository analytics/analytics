{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------
-- |
-- Module    :  Analytics.Internal.Query
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Analytics.Internal.Query
  (
  -- * Datalog
    Query(..), no
  ) where

import Analytics.Match
import Analytics.Relation
import Control.Applicative
import Data.Functor.Bind
import Data.Functor.Plus
import Data.Semigroup
import Data.Typeable

------------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------------

-- | This models a strongly typed query or the right hand side of a 'Datalog' rule.
--
-- The 'Query' itself forms an 'Alternative', letting you combine them to make a robust 
-- 'query' language.
data Query v a where
  Ap     :: Query v (a -> b) -> Query v a -> Query v b
  Map    :: (a -> b) -> Query v a -> Query v b
  Pure   :: a -> Query v a
  Alt    :: Query v a -> Query v a -> Query v a
  Empty  :: Query v a
  Select :: (Typeable1 t, Match t) => t v -> Query v (t a)
  No     :: Relation v -> Query v ()
  -- TODO: aggregations
  deriving Typeable

instance Functor (Query v) where
  fmap = Map
  {-# INLINE fmap #-}

instance Apply (Query v) where
  (<.>) = Ap
  {-# INLINE (<.>) #-}

instance Applicative (Query v) where
  pure = Pure
  {-# INLINE pure #-}
  (<*>) = Ap
  {-# INLINE (<*>) #-}

instance Alt (Query v) where
  (<!>) = Alt
  {-# INLINE (<!>) #-}

instance Plus (Query v) where
  zero = Empty
  {-# INLINE zero #-}

instance Alternative (Query v) where
  empty = Empty
  {-# INLINE empty #-}
  (<|>) = Alt
  {-# INLINE (<|>) #-}

instance Semigroup a => Semigroup (Query v a) where
  (<>) = liftA2 (<>)
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (Query v a) where
  mempty = pure mempty
  {-# INLINE mempty #-}
  mappend = liftA2 mappend
  {-# INLINE mappend #-}

-- | Anything you can 'Match' can be used directly as a 'Query', too.
instance (Typeable1 t, Match t, ta ~ t a) => Rel t v (Query v ta) where
  rel ta = Select ta
  {-# INLINE rel #-}

-- | Stratified negation.
no :: Relation a -> Query a ()
no = No
{-# INLINE no #-}
