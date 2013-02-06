{-# LANGUAGE DataKinds, PolyKinds, GADTs, TypeOperators, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, KindSignatures, Rank2Types, BangPatterns, DefaultSignatures #-}
module Data.Analytics.Storage.List
  ( List(..)
  , foldr
  , length
  , HFunctor(..)
  , HTraversable(..)
  ) where

import Control.Applicative
import Control.Lens
import Data.Functor.Identity
import Prelude hiding (map, foldr)

infixr 0 :->
type a :-> b = forall i. a i -> b i

class HFunctor f where
  hmap :: (a :-> b) -> f a :-> f b
  default hmap :: HTraversable f => (a :-> b) -> f a :-> f b
  hmap f as = runIdentity (htraverse (Identity . f) as)
  {-# INLINE hmap #-}

class HFunctor f => HTraversable f where
  htraverse :: Applicative h => (forall i. f i -> h (g i)) -> List f is -> h (List g is)

instance HFunctor List where
  hmap f (Cons x xs) = Cons (f x) (hmap f xs)
  hmap _ Nil         = Nil
  {-# INLINEABLE hmap #-}

instance HTraversable List where
  htraverse f (Cons x xs) = Cons <$> f x <*> traverse f xs
  htraverse _ Nil = pure Nil
  {-# INLINEABLE htraverse #-}

-- | An HList.
data List f (as :: [*]) where
  Cons :: f a -> List f as -> List f (a ': as)
  Nil  :: List f '[]

-- This instance requires lens HEAD/3.9
instance (Profunctor p, Functor f) => Cons p f (List g (a ': as)) (List g as) (List h (b ': bs)) (List h bs) (g a) (h b) where
  _Cons = iso (\(Cons x xs) -> (x,xs)) (\(x,xs) -> Cons x xs)
  {-# INLINE _Cons #-}

foldr :: (forall a as. f a -> r as -> r (a ': as)) -> r '[] -> List f :-> r
foldr f z (Cons x xs) = f x (foldr f z xs)
foldr _ z Nil = z
{-# INLINE foldr #-}

length :: List f -> Int
length = go 0 where
  go :: Int -> List f -> Int
  go !n (Cons _ xs) = go (n + 1) xs
  go !n Nil         = n
{-# INLINE length #-}
