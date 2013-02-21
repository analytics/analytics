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
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Datalog.Query
  ( Query(..)
  , no, key, row, value
  , Body(), Request()
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

data Body deriving Typeable
data Request deriving Typeable

-- | This models a strongly typed query or the right hand side of a 'Data.Analytics.Datalog.Datalog' rule.
--
-- The 'Query' itself forms an 'Alternative', letting you combine them to make a robust
-- 'Data.Analytics.Datalog.query' language.
data Query :: * -> * -> * where
  Ap    :: Query m (a -> b) -> Query m a -> Query m b
  Map   :: (a -> b) -> Query m a -> Query m b
  Pure  :: a -> Query m a
  Alt   :: Query m a -> Query m a -> Query m a
  Empty :: Query m a
  Value :: Atom a b  -> Query m a
  Row   :: Atom a b  -> Query m b
  Key   :: Term a => a -> Query m (Entity a)
  No    :: Atom a b  -> Query m ()
  deriving Typeable

instance Num a => Num (Query m a) where
  (+) = liftA2 (+)
  {-# INLINE (+) #-}
  (-) = liftA2 (-)
  {-# INLINE (-) #-}
  (*) = liftA2 (*)
  {-# INLINE (*) #-}
  abs = fmap abs
  {-# INLINE abs #-}
  signum = fmap signum
  {-# INLINE signum #-}
  fromInteger = Pure . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional a => Fractional (Query m a) where
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  recip = fmap recip
  {-# INLINE recip #-}
  fromRational = Pure . fromRational
  {-# INLINE fromRational #-}

instance Functor (Query m) where
  fmap = Map
  {-# INLINE fmap #-}

instance Apply (Query m) where
  (<.>) = Ap
  {-# INLINE (<.>) #-}

instance Applicative (Query m) where
  pure = Pure
  {-# INLINE pure #-}
  (<*>) = Ap
  {-# INLINE (<*>) #-}

instance Alt (Query m) where
  (<!>) = Alt
  {-# INLINE (<!>) #-}

instance Plus (Query m) where
  zero = Empty
  {-# INLINE zero #-}

instance Alternative (Query m) where
  empty = Empty
  {-# INLINE empty #-}
  (<|>) = Alt
  {-# INLINE (<|>) #-}

instance Semigroup a => Semigroup (Query m a) where
  (<>) = liftA2 (<>)
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (Query m a) where
  mempty = pure mempty
  {-# INLINE mempty #-}
  mappend = liftA2 mappend
  {-# INLINE mappend #-}

instance Term x => TermOf (Query m a) x

-- | Stratified negation.
no :: Atom a b -> Query m ()
no = No
{-# INLINE no #-}

row :: Atom a b -> Query m b
row = Row
{-# INLINE row #-}

key :: Term a => a -> Query m (Entity a)
key = Key
{-# INLINE key #-}

value :: Atom a b -> Query m a
value = Value
{-# INLINE value #-}
