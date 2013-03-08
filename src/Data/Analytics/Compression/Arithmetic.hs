{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Rank2Types #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- <http://www.stanford.edu/class/ee398a/handouts/papers/WittenACM87ArithmCoding.pdf>
-- <http://www.sable.mcgill.ca/publications/techreports/2007-5/bodden-07-arithmetic-TR.pdf>
--------------------------------------------------------------------
module Data.Analytics.Compression.Arithmetic
  (
  -- * Encoding
    Encoding, encoding, encode
  -- * Decoding
  , Decoding(..), decoding
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Analytics.Compression.Bits
import Data.Bits
import Data.Bits.Lens
import Data.Int
import Data.Monoid
import Data.Typeable

------------------------------------------------------------------------------
-- Encoding
------------------------------------------------------------------------------

data Encoding = Encoding
  { _encodingLo      :: {-# UNPACK #-} !Int64
  , _encodingHi      :: {-# UNPACK #-} !Int64
  , _encodingScale   :: {-# UNPACK #-} !Int64
  , _encodingBuilder :: !BitBuilder
  } deriving Typeable

-- | @'encode' lo hi total@ will encode a symbol in our arithmetic encoding with weight @(hi - lo) >= 1@
-- out of @total@ probability.
encode :: Int64 -> Int64 -> Int64 -> Encoding -> Encoding
encode !lc !hc !n (Encoding l h s o) = go (l + step * lc) (l + step * hc - 1) o
  where
  !step = round (fromIntegral (h-l+1) / fromIntegral n :: Double)
  go !ml !mh !mo
    | mh < q2   = go (shiftL ml 1)      (shiftL mh 1 + 1)      (mo <> putBit False <> replicateBit s True)
    | ml >= q2  = go (shiftL (ml-q2) 1) (shiftL (mh-q2) 1 + 1) (mo <> putBit True <> replicateBit s False)
    | otherwise = e3 ml mh s mo
  e3 !ml !mh !ms !mo
    | q1 <= ml && mh < q3 = e3 (shiftL (ml-q1) 1) (shiftL (mh-q1) 1 + 1) (s + 1) mo
    | otherwise           = Encoding ml mh ms mo

-- | Determine any final trailing e3 bits.
encoding :: Encoding -> BitBuilder
encoding (Encoding l _ s o)
  | l < q1    = o <> putBit False <> replicateBit (s + 1) True
  | otherwise = o <> putBit True <> replicateBit (s + 1) False
{-# INLINE encoding #-}

------------------------------------------------------------------------------
-- Decoding
------------------------------------------------------------------------------

data Decoding a = Decoding { runDecoding :: forall r. (a -> Int -> Int64 -> GetBits r) -> Int -> Int64 -> GetBits r }

instance Functor Decoding where
  fmap f (Decoding m) = Decoding $ \ k -> m (k . f)
  {-# INLINE fmap #-}

instance Applicative Decoding where
  pure a = Decoding $ \k -> k a
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad Decoding where
  return a = Decoding $ \ k -> k a
  {-# INLINE return #-}
  Decoding m >>= f = Decoding $ \ k -> m $ \a -> runDecoding (f a) k
  {-# INLINE (>>=) #-}

-- | For now fill all but the most significant bit directly.
--
-- This means we cannot arithmetic encode a sequence of bits shorter than 63 bits.
decoding :: Decoding a -> GetBits a
decoding m = do
  startBits <- replicateM 63 getBit
  runDecoding m (\a _ _ -> return a) 63 $ ifoldrOf (backwards traversed) (set . bitAt) 0 startBits

------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

q1,q2,q3 :: Int64
q1 = 0x2000000000000000
q2 = 0x6000000000000000
q3 = 0x4000000000000000

replicateBit :: Int64 -> Bool -> BitBuilder
replicateBit 0 _ = mempty
replicateBit n b = putBit b <> replicateBit (n - 1) b
{-# INLINE replicateBit #-}

