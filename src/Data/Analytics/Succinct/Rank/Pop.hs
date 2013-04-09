{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- A simple succinct 'Rank' structure that uses 'popCount' rather
-- than a second level of indirection.
--
-- <http://en.wikipedia.org/wiki/Succinct_data_structure>
--------------------------------------------------------------------
module Data.Analytics.Succinct.Rank.Pop
  ( Pop(..)
  , HasPop(..)
  , new
  , rank
  ) where

import Control.Lens
import Data.Bits
import Data.Vector.Unboxed as Unboxed
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Word
import Data.Analytics.Succinct.Rank.Class

-- | A very simple 'popCount'-based succinct 'Rank' structure.
data Pop = Pop
  { _popBits    :: !(Vector Word64)
  , _popCounts  :: !(Vector Int)
  }

makeClassy ''Pop

infixl 0 %!
(%!) :: (a -> b) -> a -> b
f %! (!x) = f x

-- | log of the # of Word64's in a chunk.
logChunkSize :: Int
logChunkSize = 5

-- | # of Word64's in a chunk
chunkSize :: Int
chunkSize = bit logChunkSize

-- | Create a vector of bits enumerated from least significant bit of the 0th slot upward including a Rank structureazzza
new :: Vector Word64 -> Pop
new xs = Pop xs $ create $ do
    let !wds = Unboxed.length xs
        !k = shiftR wds logChunkSize + 2
    counts <- M.replicate k 0
    let go !p !o !c = M.write counts o c >> if p + chunkSize <= wds
          then go %! p + chunkSize %! o + 1 %! c + Unboxed.foldl' (\a b -> a + popCount b) 0 (Unboxed.slice p chunkSize xs)
          else do
             let c' = c + Unboxed.foldl' (\a b -> a + popCount b) 0 (Unboxed.slice p (wds - p) xs)
             M.write counts %! o + 1 %! c'
             M.write counts %! k - 1 %! c' -- ensure the last place in the counter array is full
    go 0 0 0
    return counts

instance Rank Bool Pop where
  rank x k (Pop xs counts) = flop $ case quotRem k 64 of
    (q,r)
      | q >= Unboxed.length xs -> Unboxed.last counts
      | !block <- shiftR q chunkSize, !start <- shiftL block chunkSize ->
        (counts ! block)
      + Unboxed.foldl' (\a b -> a + popCount b) 0 (Unboxed.slice start (q - start) xs)
      + popCount (xs ! q) .&. (bit r - 1)
    where
      flop i | x == True = i
             | otherwise = min k (shiftL (Unboxed.length xs) 6) - i
