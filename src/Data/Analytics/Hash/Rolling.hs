{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ForeignFunctionInterface #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Hash.Rolling
  ( roll'
  , roll
  ) where

import Data.Bits
import Data.ByteString as B
import Data.ByteString.Lazy as L
import Data.Monoid
import Data.Int
import Data.Word
import Foreign.Storable
import Foreign.Ptr
import GHC.Base

foreign import ccall "static &rolling_lut" rolling_lut :: Ptr Int32

#ifndef HLINT
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of
  (# _, r #) -> r
{-# INLINE inlinePerformIO #-}
#endif HLINT

lut :: Word8 -> Int32
lut i = inlinePerformIO (peekElemOff rolling_lut (fromIntegral i))
{-# INLINE lut #-}

update :: Int32 -> Word8 -> Word8 -> Int32
update hash x y = rotateR hash 1 `xor` lut x `xor` lut y
{-# INLINE update #-}

-- | Take a strict 'S.ByteString' and generate a new lazy 'L.ByteString' with chunks based on a rolling
-- hash. This generates chunks with an expected size of 8k, where the sizes vary between 128 bytes and 64k each.
-- and the breakpoints are based on moments where a rolling hash function applied to the last 128 bytes of the
-- input matches a mask.
--
-- This can be used with various chunk hashing schemes to allow hashing that is fairly robust in the
-- presence of inline insertions and deletions.
--
-- The rolling hash is based on the ideas from @buzhash@, but since we have a known window size that is an
-- integral multiple of the word size we save work.
roll :: L.ByteString -> L.ByteString
roll z = L.fromChunks $ go seed 0 z (L.unpack (L.replicate window 0 <> z)) (L.unpack z) where
  go !h !c !bs (x:xs) (y:ys)
    | ((h' .&. mask == mask) && c >= minSize) || c >= maxSize = case L.splitAt (c + 1) bs of
       (l,r) -> B.concat (L.toChunks l) : go seed 0 r xs ys
    | otherwise = go h' (c + 1) bs xs ys
    where !h' = update h (if c < window then 0 else x) y
  go _ _ bs _ _ = [B.concat $ L.toChunks bs]
  mask     = 8191
  minSize  = 128
  maxSize  = 65536
  window   = 128
  seed     = 0 :: Int32
{-# INLINE roll #-}
-- TODO: use bytestring internals to optimize the concatenations with their known length
-- TODO: split out a separate init loop to run the minSize/window length to avoid the conditional in the inner loop
--       and the repeated check against the min size.
-- TODO: use bytestring internals to avoid constructing a bunch of intermediate lists.

-- |
-- Calculates @powqp m n p = 'rem' (m^n) p@ under the assumption that @m * p * p@ can fit in an @Int64@.
powqp :: Int64 -> Int64 -> Int64 -> Int64
powqp _ 0 _ = 1
powqp m 1 p = rem m p
powqp m n p = case quotRem n 2 of
  (q, 1) | k <- powqp m q p -> rem (k * k * m) p
  (q, _) | k <- powqp m q p -> rem (k * k) p
{-# INLINE powqp #-}

-- | Take a 'L.ByteString' and generate a new 'L.ByteString' with chunks based on a rolling
-- hash. This generates chunks with an expected size of 8k, where the sizes vary between 128 bytes and 64k each.
-- and the breakpoints are based on moments where a rolling hash function applied to the last 128 bytes of the
-- input matches a mask.
--
-- This can be used with various chunk hashing schemes to allow hashing that is fairly robust in the
-- presence of inline insertions and deletions.
--
-- This scheme is based on the standard Rabin-Karp rolling checksum. It is much slower than 'rolling', but
-- is provided here for comparison purposes.
roll' :: L.ByteString -> L.ByteString
roll' z = L.fromChunks $ go seed 0 z (L.unpack (L.replicate window 0 <> z)) (L.unpack z) where
  go !h !c !bs (x:xs) (y:ys)
    | ((h' .&. mask == mask) && c >= minSize) || c >= maxSize = case L.splitAt (c + 1) bs of
       (l,r) -> B.concat (L.toChunks l) : go seed 0 r xs ys
    | otherwise = go h' (c + 1) bs xs ys
    where
      !x' = if c < window then 0 else fromIntegral x
      !h' = rem (rem (h - magic_d * x') magic_q * magic_r + fromIntegral y) magic_q
  go _ _ bs _ _ = [B.concat $ L.toChunks bs]
  magic_d = powqp magic_r (window-1) magic_q
  mask    = 8191
  minSize = 128
  maxSize = 65536
  magic_r = 256
  magic_q = 32749
  window  = 128
  seed = 0
{-# INLINE roll' #-}
