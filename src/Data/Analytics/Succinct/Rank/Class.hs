{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Succinct.Rank.Class
  ( Rank(..)
  ) where

import Data.Vector as Vector
import Data.Vector.Primitive as Primitive
import Data.Vector.Unboxed as Unboxed
import Data.Vector.Storable as Storable
import Data.Sequence as Seq
import Data.Foldable as Foldable

class Rank a t | t -> a where
  -- | @rank x i xs@ computes the number of occurrences of @x@ in @xs@ in positions @[0..i)@
  --
  -- A default definition is supplied in terms of 'Foldable'.
  rank :: a -> Int -> t -> Int
  default rank :: (Foldable f, Eq a, t ~ f a) => a -> Int -> t -> Int
  rank x k = fst . Foldable.foldl' step (0,0) where
    step ij@(i,j) y
      | j >= k    = ij
      | !j' <- j + 1, !i' <- (if x == y then i + 1 else i) = (i',j')
  {-# INLINE rank #-}

instance Eq a => Rank a [a] where
  rank x k0 = go 0 0 where
    go !acc !p (y:ys)
      | p >= k0   = acc
      | x == y    = go (acc + 1) (p + 1) ys
      | otherwise = go acc (p + 1) ys
    go !acc _ [] = acc

instance Eq a => Rank a (Seq a)

instance Eq a => Rank a (Vector.Vector a) where

instance (Eq a, Prim a) => Rank a (Primitive.Vector a) where
  rank x k = fst . Primitive.foldl' step (0,0) where
    step ij@(i,j) y
      | j >= k    = ij
      | !j' <- j + 1, !i' <- (if x == y then i + 1 else i) = (i',j')
  {-# INLINE rank #-}

instance (Eq a, Unbox a) => Rank a (Unboxed.Vector a) where
  rank x k = fst . Unboxed.foldl' step (0,0) where
    step ij@(i,j) y
      | j >= k    = ij
      | !j' <- j + 1, !i' <- (if x == y then i + 1 else i) = (i',j')
  {-# INLINE rank #-}

instance (Eq a, Storable a) => Rank a (Storable.Vector a) where
  rank x k = fst . Storable.foldl' step (0,0) where
    step ij@(i,j) y
      | j >= k    = ij
      | !j' <- j + 1, !i' <- (if x == y then i + 1 else i) = (i',j')
  {-# INLINE rank #-}
