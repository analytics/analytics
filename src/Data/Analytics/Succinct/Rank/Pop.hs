{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- A simple succinct rank structure that uses 'popCount' rather than
-- a second level of indirection.
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
import Data.Int
import Data.Vector.Unboxed as Unboxed
import qualified Data.Vector.Unboxed.Mutable as M
import Data.Word

-- | A very simple 'popCount'-based succinct rank structure.
data Pop = Pop
  { _popNumBits :: {-# UNPACK #-} !Int
  , _popBits    :: {-# UNPACK #-} !(Vector Word64)
  , _popCounts  :: {-# UNPACK #-} !(Vector Int)
  }

makeClassy ''Pop

infixl 0 %!
(%!) :: (a -> b) -> a -> b
f %! (!x) = f x

new :: Int -> Vector Word64 -> Pop
new n xs = Pop n xs $ create $ do
    let !words = quot (n + 63) 64
        !k = shiftR words 5 + 2
    counts <- M.replicate k 0
    let go !p !op !c = M.write counts op c >> if p + 32 <= words
          then go %! p + 32 %! op + 1 %! c + Unboxed.foldl' (\a b -> a + popCount b) 0 (Unboxed.slice p 32 xs)
          else M.write counts %! op + 1 %! c + Unboxed.foldl' (\a b -> a + popCount b) 0 (Unboxed.slice p (words - p) xs)
    go 0 0 0
    return counts

-- | Succinct 'rank' calculation based on 'popCount'.
rank :: Pop -> Int -> Int
rank (Pop n xs counts) k = case quotRem k 64 of
  (q,r) | !block <- shiftR q 32, !start <- shiftL block 32 ->
          (counts ! block)
        + Unboxed.foldl' (\a b -> a + popCount b) 0 (Unboxed.slice start (q - start) xs)
        + popCount ((xs ! q) .&. (bit r - 1))
