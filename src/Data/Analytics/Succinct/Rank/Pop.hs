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

new :: Vector Word64 -> Pop
new xs = Pop xs $ create $ do
    let !wds = Unboxed.length xs
        !k = shiftR wds 5 + 2
    counts <- M.replicate k 0
    let go !p !o !c = M.write counts o c >> if p + 32 <= wds
          then go %! p + 32 %! o + 1 %! c + Unboxed.foldl' (\a b -> a + popCount b) 0 (Unboxed.slice p 32 xs)
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
      | !block <- shiftR q 32, !start <- shiftL block 32 ->
        (counts ! block)
      + Unboxed.foldl' (\a b -> a + popCount b) 0 (Unboxed.slice start (q - start) xs)
      + popCount (xs ! q) .&. (bit r - 1)
    where
      flop i | x == True = i
             | otherwise = min k (shiftL (Unboxed.length xs) 6) - i
