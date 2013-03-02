{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Datalog.Term
  ( Term(..)
{-
  , Handler(..)
  , var
  , entity
  , TermOf
  , compareTerm
  , eqTerm
-}
  ) where

import Data.Pointed
import Data.Typeable
import Unsafe.Coerce

(!) :: (f a -> b) -> a -> b
f ! e = f (point e)

(?) :: (a -> b) -> a -> b
f ? e = f e

data Term f a
  = Bottom
  | Var !(f a)
  | Entity !a
  | !(Term f a) :<> !(Term f a)
  | Star !(Term f a)
  | Top
  deriving (Eq,Ord,Show,Read)

instance Pointed (Term f) where
  point = Entity

instance IsString a => IsString (Term f a) where
  fromString = Entity.fromString

instance Functor f => Functor (Term f) where
  fmap _ Bottom = Bottom
  fmap f (Var v) = Var (fmap f v)
  fmap f (Val a) = Val (f a)
  fmap f (a :<> b) = fmap f a :<> fmap f b
  fmap f (Star a) = Star (fmap f a)
  fmap _ Top  = Top
  {-# INLINE fmap #-}

instance Foldable f => Foldable (Term f) where
  foldMap _ Bottom = mempty
  foldMap f (Var v) = foldMap f v
  foldMap f (Val a) = f a
  foldMap f (a :<> b) = foldMap f a <> foldMap f b
  foldMap f (Star a) = foldMap f a
  foldMap _ Top = mempty
  {-# INLINE foldMap #-}

instance Traversable f => Traversable (Term f) where
  traverse _ Bottom = pure Bottom
  traverse f (Var v) = Var <$> traverse f v
  traverse f (Entity a) = Entity <$> f a
  traverse f (a :<> b) = (:<>) <$> traverse f a <*> traverse f b
  traverse f (Star a) = Star <$> traverse f a
  traverse _ Top = pure Top
  {-# INLINE traverse #-}


{-

--------------------------------------------------------------------
-- Term
--------------------------------------------------------------------

data Handler a where
  IsVar :: Handler a
  IsEntity   :: Entity a ~ a => Handler a

var :: Handler a
var = IsVar

entity :: Entity a ~ a => Handler a
entity = IsEntity

#ifndef HLINT
class (Typeable (Entity a), Ord (Entity a), Show (Entity a), Typeable a, Ord a, Show a) => Term a where
  type Entity a :: *
  type Entity a = a

  term :: Handler a
  default term :: Entity a ~ a => Handler a
  term = entity
#endif

compareTerm :: (Term a, Term b) => a -> b -> Ordering
compareTerm x y = case typeOf x `compare` typeOf y of
  LT -> LT
  EQ -> unsafeCoerce x `compare` y
  GT -> GT
{-# INLINE compareTerm #-}

eqTerm :: (Term a, Term b) => a -> b -> Bool
eqTerm a b = maybe False (== b) (cast a)

class Term a => TermOf r a
-}
