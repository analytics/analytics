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
module Data.Analytics.Dictionary.Pop
  ( Pop(..)
  , HasPop(..)
  , new
  , rank
  ) where

import Control.Lens
import Data.Bits
import qualified Data.Vector.Storable as Storable
import qualified Data.Vector.Unboxed as Unboxed
import qualified Data.Vector.Unboxed.Mutable as UM
import Data.Analytics.Bitmap
import Data.Analytics.Dictionary

-- | A very simple 'popCount'-based succinct indexed dictionary.
data Pop = Pop
  { _popBits    :: !Bitmap
  , _popCounts  :: !(Unboxed.Vector Int)
  }

makeClassy ''Pop

infixl 0 %!
(%!) :: (a -> b) -> a -> b
f %! (!x) = f x

-- | log of the # of Word64s in a chunk.
logChunkWords :: Int
logChunkWords = 5

-- | # of Word64s in a chunk
chunkWords :: Int
chunkWords = bit logChunkWords

-- | Create a vector of bits enumerated from least significant bit of the 0th slot upward including a Rank structure
new :: Bitmap -> Pop
new ys = Pop ys $ Unboxed.create $ do
    let !xs  = toVector ys
        !wds = Storable.length xs
        !k  = shiftR wds logChunkWords + 2
    counts <- UM.replicate k (0 :: Int)
    let go !p !o !c = do
          UM.write counts o c
          if p + chunkWords <= wds
            then go %! p + chunkWords %! o + 1 %! c + Storable.foldl' (\a b -> a + popCount b) 0 (Storable.slice p chunkWords xs)
            else do
              let c' = c + Storable.foldl' (\a b -> a + popCount b) 0 (Storable.slice p (wds - p) xs)
              UM.write counts %! o + 1 %! c'
              UM.write counts %! k - 1 %! c' -- ensure the last place in the counter array is full
    go 0 0 0
    return counts

instance Dictionary Bool Pop where
  size (Pop xs _) = size xs
  rank x (Pop xs counts) k = flop $
      if k >= size xs
      then Unboxed.last counts
      else let
        !block = shiftR q chunkWords
        !start = shiftL block chunkWords
      in (counts Unboxed.! block) + Storable.foldl' (\a b -> a + popCount b) 0 (Storable.slice start (q - start) ys) + popCount (ys Storable.! q) .&. (bit r - 1)
    where
      q = shiftR k 6
      r = k .&. 63
      flop i | x == True = i
             | otherwise = min k (size xs) - i
      ys = toVector xs
