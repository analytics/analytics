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
data Query :: * -> * -> * -> * where
  Ap    :: Query m t (a -> b) -> Query m t a -> Query m t b
  Map   :: (a -> b) -> Query m t a -> Query m t b
  Pure  :: a -> Query m t a
  Alt   :: Query m t a -> Query m t a -> Query m t a
  Empty :: Query m t a
  Value :: Atom t a b  -> Query m t a
  Row   :: Atom t a b  -> Query m t b
  Key   :: Term a => a -> Query m t (Entity a)
  No    :: Atom t a b  -> Query m t ()
  deriving Typeable

instance Num a => Num (Query m t a) where
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

instance Fractional a => Fractional (Query m t a) where
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  recip = fmap recip
  {-# INLINE recip #-}
  fromRational = Pure . fromRational
  {-# INLINE fromRational #-}

instance Functor (Query m t) where
  fmap = Map
  {-# INLINE fmap #-}

instance Apply (Query m t) where
  (<.>) = Ap
  {-# INLINE (<.>) #-}

instance Applicative (Query m t) where
  pure = Pure
  {-# INLINE pure #-}
  (<*>) = Ap
  {-# INLINE (<*>) #-}

instance Alt (Query m t) where
  (<!>) = Alt
  {-# INLINE (<!>) #-}

instance Plus (Query m t) where
  zero = Empty
  {-# INLINE zero #-}

instance Alternative (Query m t) where
  empty = Empty
  {-# INLINE empty #-}
  (<|>) = Alt
  {-# INLINE (<|>) #-}

instance Semigroup a => Semigroup (Query m t a) where
  (<>) = liftA2 (<>)
  {-# INLINE (<>) #-}

instance Monoid a => Monoid (Query m t a) where
  mempty = pure mempty
  {-# INLINE mempty #-}
  mappend = liftA2 mappend
  {-# INLINE mappend #-}

instance Term x => TermOf (Query m t a) x

-- | Stratified negation.
no :: Atom t a b -> Query m t ()
no = No
{-# INLINE no #-}

row :: Atom t a b -> Query m t b
row = Row
{-# INLINE row #-}

key :: Term a => a -> Query m t (Entity a)
key = Key
{-# INLINE key #-}

value :: Atom t a b -> Query m t a
value = Value
{-# INLINE value #-}
