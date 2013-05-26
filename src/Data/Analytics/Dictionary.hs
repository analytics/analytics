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
  , rankFoldable
  , selectFoldable
  ) where

import Data.Bits
import Data.Analytics.Broadword
import Data.Foldable as Foldable
-- import Data.Vector as Vector
-- import Data.Vector.Primitive as Primitive
-- import Data.Vector.Unboxed as Unboxed
-- import Data.Vector.Storable as Storable
import Data.Sequence as Seq
import Data.Word

-- | This class is used to model succinct indexable dictionaries that support 'rank' and 'select' operations.
--
-- Since we can always implement one in terms of the other using galloping search if we know the 'size' of the structure,
-- putting them together in one class seems appropriate.
--
-- We use an extended variant of these combinators that permits the following simple specification:
--
-- For @i@ >= @1@,
--
-- @
-- 'rank' x xs ('select' x xs i) ≡ i
-- 'select' x xs ('rank' x xs i) <= i
-- @
--
-- There is a Galois connection between 'rank' and 'select'. In fact, due to the equality, they form a \"retract situation\".
--
-- <http://ncatlab.org/nlab/show/retraction>
--
-- Given this and the well-foundedness of the total order on the natural numbers, 'select' can be defined in terms of 'rank' by
-- binary search.
--
-- To simplify the upper bound on the above We also extend the definition of 'rank' and 'select' to consider every position
-- after the end of the dictionary to match all symbols. This means we can implement 'rank' by binary search on 'select' as well.
--
-- A minimum complete definition for this class requires 'size' and either 'rank' or 'select'.
class Dictionary a t | t -> a where
  -- | An /O(n)/ default definition is supplied in terms of 'Foldable'.
  size :: t -> Int
  default size :: (Foldable f, t ~ f b) => t -> Int
  size = Foldable.foldl' (\n _ -> n + 1) 0
  {-# INLINE size #-}

  -- | @'rank' x i xs@ computes the number of occurrences of @x@ in @xs@ in positions @[0..i)@
  --
  -- If @i@ is larger than the length of the container, then every position past the end of the
  -- data set is considered to match. This simplifies the statement of the 'select' and 'rank' laws.
  --
  -- Additionally, we choose the conservative extension:
  --
  -- @rank x xs i | i <= 0 = i@
  --
  -- Between these two extensions,
  --
  -- For @i@ >= @1@ the answer should lie in the range @[0..i]@, when @i@ <= @0@ then the answer will be @i@.
  --
  -- An /O('select' cost * log n)/ default definition is supplied in terms of 'select'.
  --
  -- >>> rank 1 [1,0,1] 2
  -- 1
  --
  -- >>> rank 1 [1,0,1] 3
  -- 2
  rank :: a -> t -> Int -> Int
  rank a t i
    | i <= 0    = i
    | otherwise = search (\j -> select a t j >= i) 0 i
  {-# INLINE rank #-}

  -- | @deltaRank x xs i j@ computes the number of occurences of @x@ in @xs@ in the interval @[i..j)@.
  --
  -- The identity
  --
  -- @
  -- 'deltaRank' a xs i j = 'rank' a xs j - 'rank' a xs i
  -- @
  --
  -- is expected to hold, but it can often be implemented more efficiently.
  --
  -- The obvious default definition is supplied in terms of this identity.
  --
  -- A corrolary of this identity is that:
  --
  -- @
  -- 'deltaRank' a xs 0 i = 'rank' a xs i
  -- @
  deltaRank :: a -> t -> Int -> Int -> Int
  deltaRank a t i j = rank a t j - rank a t i
  {-# INLINE deltaRank #-}

  -- @'select x xs i'@ returns the position /after/ the @i@th occurence of @x@ in @xs@. You can also
  -- view it as the @1@-based position of the @i@th occurence.
  --
  -- This provides legal answers for @i@ in @[1..'rank' x xs ('size' xs))@ in the range
  -- @[i..size xs)@. For @i >= 'rank' xs ('size' xs)@, you'll get positions larger than @('size' xs)@
  --
  -- @
  -- select x xs i | i >= r = n + i - r where n = 'size' xs; r = 'rank' x xs n
  -- @
  --
  -- Moreover,
  --
  -- @
  -- select x xs i | i <= 0 = i
  -- @
  --
  -- This permits 'rank' to be easily implemented by binary search using 'select'.
  --
  -- An /O('rank' cost * log n)/ default definition is supplied in terms of 'rank'.
  --
  -- >>> select 1 [1,0,1] 1
  -- 1
  --
  -- >>> select 1 [1,0,1] 2
  -- 3

  -- >>> select 1 [1,0,1] 3
  -- 4
  select :: a -> t -> Int -> Int
  select a t i
    | i <= 0 = i
    | otherwise = search (\j -> rank a t j >= i) i (size t + i)
  {-# INLINE select #-}

-- | This provides a valid default definition for 'select' for a 'Foldable' container. This is useful for testing,
-- but not so practical for real world use, as it requires /O(n)/ time to execute a query.
selectFoldable :: (Foldable f, Eq a) => a -> f a -> Int -> Int
selectFoldable a xs k
  | k <= 0 = k
  | otherwise = go k 1 $ Foldable.toList xs
  where
    go !i !j (b:bs)
      | a /= b = go i (j + 1) bs
      | i <= 1 = j
      | otherwise = go (i - 1) (j + 1) bs
    go i j [] = i + j - 1
{-# INLINE selectFoldable #-}

-- | This provides a valid default definition for 'rank' for a 'Foldable' container. This is useful for testing,
-- but not so practical for real world use, as it requires /O(n)/ time to execute a query.
rankFoldable :: (Foldable f, Eq a) => a -> f a -> Int -> Int
rankFoldable x xs k
  | k <= 0    = k
  | otherwise = case Foldable.foldl' step (0,0) xs of (i,j) -> i + k - min j k
  where
    step ij@(i,j) y
      | j >= k = ij
      | !j' <- j + 1, !i' <- (if x == y then i + 1 else i) = (i',j')
{-# INLINE rankFoldable #-}

-- | /O(n)/ 'rank' and 'select'
instance Eq a => Dictionary a [a] where
  size = Prelude.length
  rank = rankFoldable
  select = selectFoldable

-- | /O(n)/ 'rank' and 'select'
instance Eq a => Dictionary a (Seq a) where
  size = Seq.length
  rank = rankFoldable
  select = selectFoldable

search :: (Int -> Bool) -> Int -> Int -> Int
search p l0 h0 = go l0 h0 where
  go !l !h
    | l >= h    = l
    | p m       = go l m
    | otherwise = go (m+1) h
    where m = l + shiftR (h - l) 1
{-# INLINE search #-}

-- | /O(1)/ 'rank' and 'select'
instance Dictionary Bool Word64 where
  size _ = 64
  {-# INLINE size #-}
  rank True xs i
    | i >= 64   = popCount xs
    | otherwise = popCount $ xs .&. (bit i - 1)
  rank False xs i
    | i >= 64   = 64 - popCount xs
    | otherwise = i  - popCount (xs .&. (bit i - 1))
  {-# INLINE rank #-}
  select True  xs i = selectWord64 xs i
  select False xs i = selectWord64 (complement xs) i
  {-# INLINE select #-}
