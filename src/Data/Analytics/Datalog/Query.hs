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
data Query :: * -> * where
  Ap    :: Query (a -> b) -> Query a -> Query b
  Map   :: (a -> b) -> Query a -> Query b
  Pure  :: a -> Query a
  Alt   :: Query a -> Query a -> Query a
  Empty :: Query a
  Value :: Atom a b -> Query a
  Row   :: Atom a b -> Query b
  Key   :: Term a => a -> Query (Entity a)
  No    :: Atom a b -> Query ()
  deriving Typeable

instance Show (Query a) where
  showsPrec d (Ap l r) = showParen (d > 4) $
    showsPrec 4 l . showString " <*> " . showsPrec 5 r
  showsPrec d (Map _ r) = showParen (d > 4) $
    showString ".. <$> " . showsPrec 5 r
  showsPrec d (Pure _) = showParen (d > 10) $ showString "pure .."
  showsPrec d (Alt l r) = showParen (d > 3) $
    showsPrec 3 l . showString " <|> " . showsPrec 4 r
  showsPrec _ Empty = showString "empty"
  showsPrec d (Value a) = showParen (d > 10) $
    showString "value " . showsPrec 11 a
  showsPrec d (Row a) = showParen (d > 10) $
    showString "row " . showsPrec 11 a
  showsPrec d (Key a) = showParen (d > 10) $
    showString "key " . showsPrec 11 a
  showsPrec d (No a) = showParen (d > 10) $
    showString "no " . showsPrec 11 a

instance Num a => Num (Query a) where
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

instance Fractional a => Fractional (Query a) where
  (/) = liftA2 (/)
  {-# INLINE (/) #-}
  recip = fmap recip
  {-# INLINE recip #-}
  fromRational = Pure . fromRational
  {-# INLINE fromRational #-}

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

instance a ~ b => Atomic (Query a) b c where
  atom = Value
  {-# INLINE atom #-}

-- | Stratified negation.
no :: Atom a b -> Query ()
no = No
{-# INLINE no #-}

row :: Atom a b -> Query b
row = Row
{-# INLINE row #-}

key :: Term a => a -> Query (Entity a)
key = Key
{-# INLINE key #-}

value :: Atom a b -> Query a
value = Value
{-# INLINE value #-}
