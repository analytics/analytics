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

-- | prime remulus
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

-- | @'div' (m^n) magic_q@
powqp :: Int -> Int -> Int
powqp m n = case quotRem n 2 of
  (0, 1) -> rem m magic_q
  (0, 0) -> 1
  (q, 1) | k <- powqp m q -> rem (rem (k * k) magic_q * m) magic_q
  (q, 0) | k <- powqp m q -> rem (k * k) magic_q
{-# INLINE powqp #-}

-- how much to drop
magic_d :: Int
magic_d = powqp magic_r (window-1)

seed :: Int
seed = 0

rolling :: L.ByteString -> L.ByteString
rolling z = L.fromChunks $ go seed 0 z (L.unpack (L.replicate (fromIntegral window) 0 <> z)) (L.unpack z) where
  go !h !c !bs (x:xs) (y:ys)
    | ((h' .&. mask == mask) && c >= minSize) || c >= maxSize = case L.splitAt (fromIntegral $ c + 1) bs of
       (l,r) -> B.concat (L.toChunks l) : go seed 0 r xs ys
    | otherwise                                               = go h' (c + 1) bs xs ys
    where
      x' = if c < window then 0 else x
      h' = rem (rem (h - magic_d * fromIntegral x') magic_q * magic_r + fromIntegral y) magic_q
  go _ _ bs _ _ = [B.concat $ L.toChunks bs]
