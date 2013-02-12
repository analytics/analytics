{-# LANGUAGE BangPatterns #-}
module Data.Analytics.Hash.Rolling
  ( rollingPrime
  , rollingSum
  , rollingSumXor
  ) where

import Data.Bits
import Data.ByteString as B
import Data.ByteString.Lazy as L
import Data.Monoid
import Data.Int
import Data.Word

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
rollingPrime :: L.ByteString -> L.ByteString
rollingPrime z = L.fromChunks $ go seed 0 z (L.unpack (L.replicate window 0 <> z)) (L.unpack z) where
  go !h !c !bs (x:xs) (y:ys)
    | ((h' .&. mask == mask) && c >= minSize) || c >= maxSize = case L.splitAt (c + 1) bs of
       (l,r) -> B.concat (L.toChunks l) : go seed 0 r xs ys
    | otherwise = go h' (c + 1) bs xs ys
    where
      x' = if c < window then 0 else fromIntegral x
      h' = rem (rem (h - magic_d * x') magic_q * magic_r + fromIntegral y) magic_q
  go _ _ bs _ _ = [B.concat $ L.toChunks bs]
  magic_d = powqp magic_r (window-1) magic_q
  mask    = 8191
  minSize = 128
  maxSize = 65536
  magic_r = 256
  magic_q = 32749
  window  = 128
  seed = 0
{-# INLINE rollingPrime #-}

rollingSum :: L.ByteString -> L.ByteString
rollingSum z = L.fromChunks $ go seed 0 z (L.unpack (L.replicate window 0 <> z)) (L.unpack z) where
  go !h !c !bs (x:xs) (y:ys)
    | ((h' .&. mask == mask) && c >= minSize) || c >= maxSize = case L.splitAt (c + 1) bs of
       (l,r) -> B.concat (L.toChunks l) : go seed 0 r xs ys
    | otherwise = go h' (c + 1) bs xs ys
    where h' = h + fromIntegral y - if c < window then 0 else fromIntegral x
  go _ _ bs _ _ = [B.concat $ L.toChunks bs]
  mask     = 8191
  minSize  = 128
  maxSize  = 65536
  window   = 128
  seed     = 0 :: Int
{-# INLINE rollingSum #-}

rollingSumXor :: L.ByteString -> L.ByteString
rollingSumXor z = L.fromChunks $ go seed seed 0 z (L.unpack (L.replicate window 0 <> z)) (L.unpack z) where
  go !h !i !c !bs (x:xs) (y:ys)
    | ((h' .&. 127 == 127) && (i' .&. 63 == 63) && c >= minSize) || c >= maxSize = case L.splitAt (c + 1) bs of
       (l,r) -> B.concat (L.toChunks l) : go seed seed 0 r xs ys
    | otherwise = go h' i' (c + 1) bs xs ys
    where 
      x' = if c < window then 0 else x
      h' = h + y - x'
      i' = i `xor` y `xor` x'
  go _ _ _ bs _ _ = [B.concat $ L.toChunks bs]
  minSize  = 128
  maxSize  = 65536
  window   = 128
  seed     = 0 :: Word8
{-# INLINE rollingSumXor #-}
