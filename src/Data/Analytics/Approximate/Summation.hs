--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Monoidal compensated summation based on Knuth's error free
-- transformation and various algorithms by Ogita, Rump and Oishi.
--------------------------------------------------------------------
module Data.Analytics.Approximate.Summation
  ( EFT(..)
  , runEFT
  , magic
  , split
  , sum2, sum3
  , times2
  ) where

import Data.Monoid
import Data.Ratio

data EFT = EFT {-# UNPACK #-} !Double {-# UNPACK #-} !Double deriving (Eq,Ord,Show,Read)

magic :: Double
magic = 134217729 -- 2^27+1,  2^12+1 for float
{-# INLINE magic #-}

-- | error-free split of a floating point number into two parts.
split :: Double -> EFT
split a = EFT x y where
  c = magic*a
  x = c - (c - a)
  y = a - x
{-# INLINE split #-}

-- | Knuth's error free transformation
--
-- After @'EFT' x y = 'sum2' a b@:
--
-- > a + b = x + y
-- > x = fl(a + b)
sum2 :: Double -> Double -> EFT
sum2 a b = EFT x y where
  x = a + b
  z = x - a
  y = (a - (x - z)) + (b - z)
{-# INLINE sum2 #-}

-- | Ogita, Rump and Oishi's 'VecSum'
sum3 :: Double -> Double -> Double -> EFT
sum3 a b c = EFT x4 (y3 + y4) where
  EFT x1 y1 = sum2 a b
  EFT x2 y2 = sum2 x1 c
  EFT x3 y3 = sum2 y1 y2
  EFT x4 y4 = sum2 x2 x3
{-# INLINE sum3 #-}

times2 :: Double -> Double -> EFT
times2 a b = EFT x (a2*b2 - (((x - a1*b1) - a2*b1) - a1*b2)) where
  x = a * b
  EFT a1 a2 = split a
  EFT b1 b2 = split b
{-# INLINE times2 #-}

runEFT :: EFT -> Double
runEFT (EFT x y) = x + y
{-# INLINE runEFT #-}

-- vsum :: Vector Double -> EFT

instance Monoid EFT where
  mempty = EFT 0 0
  {-# INLINE mempty #-}
  mappend = (+)
  {-# INLINE mappend #-}

instance Num EFT where
  EFT a b + EFT a' b' = EFT x5 (y3 + y4 + y5) where
    EFT x1 y1 = sum2 a  a'
    EFT x2 y2 = sum2 x1 b'
    EFT x3 y3 = sum2 b  y1
    EFT x4 y4 = sum2 x3 y2
    EFT x5 y5 = sum2 x2 x4
  {-# INLINE (+) #-}

  -- | Ogita et al.'s @DotK, K=3@ applied to the FOIL'd sums
  EFT a b * EFT c d = EFT xe (y8 + y9 + ya + yb + yc + yd + ye) where
    EFT x1 y1 = times2 a c
    EFT x2 y2 = times2 a d
    EFT x3 y3 = times2 b c
    EFT x4 y4 = times2 b d
    EFT x5 y5 = sum2 x1 x2
    EFT x6 y6 = sum2 x5 x3
    EFT x7 y7 = sum2 x6 x4
    EFT x8 y8 = sum2 y1 y2
    EFT x9 y9 = sum2 y5 x8
    EFT xa ya = sum2 y3 x9
    EFT xb yb = sum2 y6 xa
    EFT xc yc = sum2 y4 xb
    EFT xd yd = sum2 y7 xc
    EFT xe ye = sum2 x7 xd
  {-# INLINE (*) #-}

  negate (EFT a b) = EFT (negate a) (negate b)
  {-# INLINE negate #-}

  x - y = x + negate y
  {-# INLINE (-) #-}

  signum (EFT a b) = EFT (signum (a + b)) 0
  {-# INLINE signum #-}

  abs e@(EFT a b)
    | a + b < 0 = negate e
    | otherwise = e
  {-# INLINE abs #-}

  fromInteger = split . fromInteger
  {-# INLINE fromInteger #-}

instance Fractional EFT where
  recip (EFT a b) = split $ recip $ a + b -- TODO better
  {-# INLINE recip #-}
  fromRational r = fromInteger (numerator r) `times2` recip (fromInteger (denominator r))
  {-# INLINE fromRational #-}

instance Real EFT where
  toRational (EFT a b) = toRational a + toRational b
  {-# INLINE toRational #-}

instance RealFrac EFT where
  properFraction (EFT a b) = case sum2 a b of
    EFT x y -> case properFraction x of
      (w,p) -> (w, sum2 p y)
  {-# INLINE properFraction #-}
  round = round . runEFT
  {-# INLINE round #-}
  truncate = truncate . runEFT
  {-# INLINE truncate #-}
  ceiling = ceiling . runEFT
  {-# INLINE ceiling #-}
  floor = floor . runEFT
  {-# INLINE floor #-}
