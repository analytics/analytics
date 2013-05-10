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
module Data.Analytics.Dictionary
  ( Dictionary(..)
  , rankBits
  , selectBits
  ) where

import Data.Bits
import Data.Foldable as Foldable
import Data.Vector as Vector
import Data.Vector.Primitive as Primitive
import Data.Vector.Unboxed as Unboxed
import Data.Vector.Storable as Storable
import Data.Sequence as Seq
import Data.Word

-- | This class is used to model 'succinct indexable dictionaries' that support 'rank' and 'select' operations.
-- Since we can always implement one in terms of the other using galloping search if we know the 'size' of the structure,
-- putting them together in one class seems appropriate.
--
-- The meaning of operations when values are supplied out of range is 'undefined'. The tools here are enough to check
-- for valid ranges.
--
-- @
-- 'rank' x xs ('select' x xs i) ≡ i
-- @
--
-- There is a Galois connection between 'rank' and 'select'. In other words:
--
-- @'rank' x xs i <= j@ iff @i <= 'select' x xs j@
--
class Dictionary a t | t -> a where
  -- | An /O(n)/ default definition is supplied in terms of 'Foldable'.
  size :: t -> Int
  default size :: (Foldable f, t ~ f b) => t -> Int
  size = Foldable.foldl' (\n _ -> n + 1) 0

  -- | @'rank' x i xs@ computes the number of occurrences of @x@ in @xs@ in positions @[0..i)@
  --
  -- This provides legal answers for @i@ in @[0..'size' xs)@. Answers for indices outside of this
  -- range are 'undefined'.
  --
  -- An /O(n)/ default definition is supplied in terms of 'Foldable'.
  rank :: a -> t -> Int -> Int
  default rank :: (Foldable f, Eq a, t ~ f a) => a -> t -> Int -> Int
  rank x xs k = fst $ Foldable.foldl' step (0,0) xs where
    step ij@(i,j) y
      | j >= k    = ij
      | !j' <- j + 1, !i' <- (if x == y then i + 1 else i) = (i',j')
  {-# INLINE rank #-}

  -- | @deltaRank x i j xs@ computes the number of occurences of @x@ in @xs@ in the interval @[i..j)@.
  --
  -- The identity
  --
  -- @'deltaRank' a i j xs = 'rank' a j xs - 'rank' a i xs@
  --
  -- is expected to hold, but it can often be implemented more efficiently.
  --
  -- The obvious default definition is supplied in terms of the identity with rank.
  deltaRank :: a -> t -> Int -> Int -> Int
  deltaRank a t i j = rank a t j - rank a t i

  -- @'select x i xs'@ returns the position of the @i@th occurence of @x@ in @xs@.
  --
  -- This provides legal answers for @i@ in @[0..'rank' x xs ('size' xs))@. Answers for indices
  -- outside of this range are 'undefined'.
  --
  -- An /O(n)/ default definition is supplied in terms of 'Foldable'.
  --
  select :: a -> t -> Int -> Int
  default select :: (Foldable f, Eq a, t ~ f a) => a -> t -> Int -> Int
  select a xs k = go k 0 $ Foldable.toList xs where
    go !i !j (b:bs)
      | a /= b = go i j bs
      | i == 0 = j
      | otherwise = go (i - 1) (j + 1) bs
    go _ _ _  = error "select: out of bounds"

instance Eq a => Dictionary a [a] where
  size = Prelude.length
  rank x xs k0 = go 0 0 xs where
    go !acc !p (y:ys)
      | p >= k0   = acc
      | x == y    = go (acc + 1) (p + 1) ys
      | otherwise = go acc (p + 1) ys
    go !acc _ [] = acc
  select a xs k = go k 0 xs where
    go !i !j (b:bs)
      | a /= b = go i j bs
      | i == 0 = j
      | otherwise = go (i - 1) (j + 1) bs
    go _ _ _  = error "select: out of bounds"

instance Eq a => Dictionary a (Seq a)

-- | This provides a valid definition for 'rank' in terms of 'popCount' for instances of 'Bits'
rankBits :: (Num a, Bits a) => Bool -> a -> Int -> Int
rankBits True xs i = popCount $ xs .&. (bit i - 1)
rankBits False xs i = i - popCount (xs .&. (bit i - 1))
{-# INLINE rankBits #-}

selectBits :: (Num a, Bits a) => Bool -> a -> Int -> Int
selectBits = error "selectBits: TODO"
{-# INLINE selectBits #-}

instance Dictionary Bool Word64 where
  size _ = 64
  {-# INLINE size #-}
  rank = rankBits
  {-# INLINE rank #-}
  select = selectBits
  {-# INLINE select #-}

instance Dictionary Bool Word32 where
  size _ = 32
  {-# INLINE size #-}
  rank = rankBits
  {-# INLINE rank #-}
  select = selectBits
  {-# INLINE select #-}

instance Dictionary Bool Word16 where
  size _ = 16
  {-# INLINE size #-}
  rank = rankBits
  {-# INLINE rank #-}
  select = selectBits
  {-# INLINE select #-}

instance Dictionary Bool Word8 where
  size _ = 8
  {-# INLINE size #-}
  rank = rankBits
  {-# INLINE rank #-}
  select = selectBits
  {-# INLINE select #-}

instance Eq a => Dictionary a (Vector.Vector a)

instance (Eq a, Prim a) => Dictionary a (Primitive.Vector a) where
  size = Primitive.length
  {-# INLINE size #-}
  rank x xs k = fst $ Primitive.foldl' step (0,0) xs where
    step ij@(i,j) y
      | j >= k    = ij
      | !j' <- j + 1, !i' <- (if x == y then i + 1 else i) = (i',j')
  {-# INLINE rank #-}
  select = undefined

instance (Eq a, Unbox a) => Dictionary a (Unboxed.Vector a) where
  size = Unboxed.length
  {-# INLINE size #-}
  rank x xs k = fst $ Unboxed.foldl' step (0,0) xs where
    step ij@(i,j) y
      | j >= k    = ij
      | !j' <- j + 1, !i' <- (if x == y then i + 1 else i) = (i',j')
  {-# INLINE rank #-}
  select = undefined

instance (Eq a, Storable a) => Dictionary a (Storable.Vector a) where
  size = Storable.length
  {-# INLINE size #-}
  rank x xs k = fst $ Storable.foldl' step (0,0) xs where
    step ij@(i,j) y
      | j >= k    = ij
      | !j' <- j + 1, !i' <- (if x == y then i + 1 else i) = (i',j')
  {-# INLINE rank #-}
  select = undefined

{-
data Drop t = Drop !Int t

instance Functor Drop where
  fmap f (Drop i a) = Drop i (f a)

instance Foldable Drop where
  foldMap f (Drop _ a) = f a

instance Traversable Drop where
  traverse f (Drop i a) = Drop i <$> f a

instance Dictionary a t => Dictionary a (Drop t) where
  rank a (Drop i xs) k = rank a xs (k + i)
  select a (Drop i xs) k = select a xs (k + rank a xs i)
-}
