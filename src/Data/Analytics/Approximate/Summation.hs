{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
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

import Control.Lens
import Data.Monoid
import Data.Ratio

{-# ANN module "hlint: ignore Use -" #-}

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

instance Monoid EFT where
  mempty = EFT 0 0
  {-# INLINE mempty #-}
  mappend = (+)
  {-# INLINE mappend #-}

instance (Bifunctor p, Profunctor p, Functor f) => Cons p f EFT EFT Double Double where
  _Cons = unto $ \(a, EFT b c) -> let y = a - c; t = b + y in EFT t ((t - b) - y)
  {-# INLINE _Cons #-}

instance (Bifunctor p, Profunctor p, Functor f) => Snoc p f EFT EFT Double Double where
  _Snoc = unto $ \(EFT b c, a) -> let y = a - c; t = b + y in EFT t ((t - b) - y)
  {-# INLINE _Snoc #-}

instance Num EFT where
  EFT a b + EFT a' b' = sum2 x4 (y2 + y3 + y4) where
    EFT x1 y1 = sum2 a a'
    EFT x2 y2 = sum2 y1 b'
    EFT x3 y3 = sum2 b x2
    EFT x4 y4 = sum2 x1 x3
  {-# INLINE (+) #-}

  EFT a b * EFT c d = sum2 x8 (b * d + y2 + y3 + y6 + y7 + y8) where
    EFT x1 y1 = times2 a c
    EFT x2 y2 = times2 b c
    EFT x3 y3 = times2 a d
    EFT x4 y4 = sum2 x1 x2
    EFT x5 y5 = sum2 x3 x4
    EFT x6 y6 = sum2 y1 y4
    EFT x7 y7 = sum2 y5 x6
    EFT x8 y8 = sum2 x5 x7
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

  fromInteger i = sum2 x (fromInteger (i - round x)) where
    x = fromInteger i
  {-# INLINE fromInteger #-}

instance Fractional EFT where
  -- recip (EFT a b) = split $ recip $ a + b

  -- > 1/(a + b) = 1/a + e where
  -- > e = 1/(a + b) - 1/a = -b/(a^2 + a b) = -b/a^2 + e_2
  -- > so e_2 = b^2/a^3 + e_3 is vanishingly small
  recip (EFT a b) = sum2 (recip a) (negate b / (a*a))
  {-# INLINE recip #-}
  -- EFT a b / EFT c d = sum2 (a / (c + d)) (b / (c + d))
  -- EFT a b / EFT c d = sum2 (a / c) ((b*c-a*d)/(c*c))
  -- EFT a b / EFT c d = sum3 (a/c) (b/c) (-a*d/(c*c))
  EFT a b / EFT c d = sum2 x3 (y1 + y2 + y3) where
    aoc = a/c
    EFT x1 y1 = times2 aoc (d/c)
    EFT x2 y2 = sum2 x1 (b/c)
    EFT x3 y3 = sum2 aoc x2
  {-# INLINE (/) #-}
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
