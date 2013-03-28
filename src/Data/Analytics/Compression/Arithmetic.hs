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
import Control.Monad.Trans
import Data.Bits
import Data.Bits.Coding
import Data.Bits.Lens
import Data.Bytes.Get
import Data.Bytes.Put
import Data.Int
import Data.Typeable

------------------------------------------------------------------------------
-- Encoding
------------------------------------------------------------------------------

data Encoding = Encoding
  { _encodingLo      :: {-# UNPACK #-} !Int64
  , _encodingHi      :: {-# UNPACK #-} !Int64
  , _encodingScale   :: {-# UNPACK #-} !Int64
  } deriving Typeable

-- | @'encode' lo hi total@ will encode a symbol in our arithmetic encoding with weight @(hi - lo) >= 1@
-- out of @total@ probability.
encode :: MonadPut m => Int64 -> Int64 -> Int64 -> Encoding -> Coding m Encoding
encode !lc !hc !n (Encoding l h s) = go (l + step * lc) (l + step * hc - 1)
  where
  !step = round (fromIntegral (h-l+1) / fromIntegral n :: Double)
  go !ml !mh
    | mh < q2   = do
      putBit False
      replicateBit s True
      go (shiftL ml 1) (shiftL mh 1 + 1)
    | ml >= q2  = do
      putBit True
      replicateBit s False
      go (shiftL (ml-q2) 1) (shiftL (mh-q2) 1 + 1)
    | otherwise = e3 ml mh s
  e3 !ml !mh !ms
    | q1 <= ml && mh < q3 = e3 (shiftL (ml-q1) 1) (shiftL (mh-q1) 1 + 1) (s + 1)
    | otherwise           = return $ Encoding ml mh ms

-- | Determine any final trailing e3 bits.
encoding :: MonadPut m => Encoding -> Coding m ()
encoding (Encoding l _ s)
  | l < q1    = putBit False >> replicateBit (s + 1) True
  | otherwise = putBit True >> replicateBit (s + 1) False
{-# INLINE encoding #-}


------------------------------------------------------------------------------
-- Decoding
------------------------------------------------------------------------------

newtype Decoding m a = Decoding { runDecoding :: forall r. (a -> Int -> Int64 -> Coding m r) -> Int -> Int64 -> Coding m r }

instance Functor (Decoding m) where
  fmap f (Decoding m) = Decoding $ \ k -> m (k . f)
  {-# INLINE fmap #-}

instance Monad m => Applicative (Decoding m) where
  pure a = Decoding $ \k -> k a
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad m => Monad (Decoding m) where
  return a = Decoding $ \ k -> k a
  {-# INLINE return #-}
  Decoding m >>= f = Decoding $ \ k -> m $ \a -> runDecoding (f a) k
  {-# INLINE (>>=) #-}

instance MonadTrans Decoding where
  lift m = Decoding $ \k i b -> do
    a <- lift m
    k a i b
  {-# INLINE lift #-}

-- | For now fill all but the most significant bit directly.
--
-- This means we cannot arithmetic encode a sequence of bits shorter than 63 bits.
decoding :: MonadGet m => Decoding m a -> Coding m a
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

replicateBit :: MonadPut m => Int64 -> Bool -> Coding m ()
replicateBit 0 _ = return ()
replicateBit n b = putBit b >> replicateBit (n - 1) b
{-# INLINE replicateBit #-}

