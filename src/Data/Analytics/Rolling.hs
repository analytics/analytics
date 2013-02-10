{-# LANGUAGE BangPatterns #-}
module Data.Analytics.Rolling
  ( rolling
  ) where

import Data.Bits
import Data.ByteString as B
import Data.ByteString.Lazy as L
import Data.Monoid

-- | window size
window :: Int
window = 128

-- | prime modulus
magic_q :: Int
magic_q = 32749 -- 15 bits

-- | item size
magic_r :: Int
magic_r = 256

mask :: Int
mask = 8191

minSize :: Int
minSize = 128

maxSize :: Int
maxSize = 65536

-- | @'quot' (m^n) magic_q@
powqp :: Int -> Int -> Int
powqp m n = case quotRem n 2 of
  (0, 1) -> quot m magic_q
  (0, 0) -> 1
  (q, r) | k <- powqp m q -> quot (k * k) magic_q
{-# INLINE powqp #-}

-- how much to drop
magic_d :: Int
magic_d = powqp magic_r (window-1)

seed :: Int
seed = 0

rolling :: L.ByteString -> L.ByteString
rolling z = L.fromChunks $ Prelude.map B.pack $ go seed 0 (L.unpack (L.replicate 128 0 <> z)) (L.unpack z) [] where
  go !h !c (x:xs) (y:ys) zs
    | ((h' .&. mask == mask) && c >= minSize) || c >= maxSize = (y:Prelude.reverse zs) : go seed 0 xs ys []
    | otherwise                                               = go h' (c + 1) xs ys (y:zs)
    where 
      x' = if c < 128 then 0 else x
      h' = rem (rem (h - magic_d * fromIntegral x') magic_q * magic_r + fromIntegral y) magic_q
  go  h  _ _ _ zs = [Prelude.reverse zs]
