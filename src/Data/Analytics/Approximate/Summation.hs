{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
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
  , _Compensated
  , Overcompensated
  , primal
  , residual
  , uncompensated
  , add
  , times
  , split
  ) where

import Control.Applicative
import Control.Lens
import Data.Monoid
import Data.Ratio

{-# ANN module "hlint: ignore Use -" #-}

-- | @'add' a b k@ computes @k x y@ such that
--
-- > x + y = a + b
-- > x = fl(a + b)
--
-- Which is to say that @x@ is the floating point image of @(a + b)@ and
-- @y@ stores the residual error term.
add :: Num a => a -> a -> (a -> a -> r) -> r
add a b k = k x y where
  x = a + b
  z = x - a
  y = (a - (x - z)) + (b - z)
{-# INLINE add #-}

-- | @'times' a b k@ computes @k x y@ such that
--
-- > x + y = a * b
-- > x = fl(a * b)
--
-- Which is to say that @x@ is the floating point image of @(a * b)@ and
-- @y@ stores the residual error term.
times :: EFT a => a -> a -> (a -> a -> r) -> r
times a b k =
  split a $ \a1 a2 ->
  split b $ \b1 b2 ->
  let x = a * b in k x (a2*b2 - (((x - a1*b1) - a2*b1) - a1*b2))
{-# INLINE times #-}

-- | error-free split of a floating point number into two parts.
--
-- Note: these parts do not satisfy the `compensated` contract
split :: EFT a => a -> (a -> a -> r) -> r
split a k = k x y where
  c = magic*a
  x = c - (c - a)
  y = a - x
{-# INLINE split #-}

class RealFrac a => EFT a where
  data Compensated a
  with :: EFT a => Compensated a -> (a -> a -> r) -> r
  compensated :: EFT a => a -> a -> Compensated a
  magic :: a


_Compensated :: EFT a => Iso' (Compensated a) (a, a)
_Compensated = iso (`with` (,)) (uncurry compensated)
{-# INLINE _Compensated #-}

instance EFT Double where
  data Compensated Double = CD {-# UNPACK #-} !Double {-# UNPACK #-} !Double
    deriving (Show,Read)
  with (CD a b) k = k a b
  {-# INLINE with #-}
  compensated = CD
  {-# INLINE compensated #-}
  magic = 134217729
  {-# INLINE magic #-}

instance EFT Float where
  data Compensated Float = CF {-# UNPACK #-} !Float {-# UNPACK #-} !Float
    deriving (Show,Read)
  with (CF a b) k = k a b
  {-# INLINE with #-}
  compensated = CF
  {-# INLINE compensated #-}
  magic = 4097
  {-# INLINE magic #-}

instance EFT a => EFT (Compensated a) where
  data Compensated (Compensated a) = CC !(Compensated a) !(Compensated a)
  with (CC a b) k = k a b
  {-# INLINE with #-}
  compensated = CC
  {-# INLINE compensated #-}
  magic = times magic magic compensated
  {-# INLINE magic #-}

type Overcompensated a = Compensated (Compensated a)

primal :: EFT a => Lens' (Compensated a) a
primal f c = with c $ \a b -> f a <&> \a' -> compensated a' b
{-# INLINE primal #-}

residual :: EFT a => Lens' (Compensated a) a
residual f c = with c $ \a b -> compensated a <$> f b
{-# INLINE residual #-}

uncompensated :: EFT a => Compensated a -> a
uncompensated c = with c const
{-# INLINE uncompensated #-}

instance EFT a => Eq (Compensated a) where
  m == n = with m $ \a b -> with n $ \c d -> a == c && b == d

instance EFT a => Ord (Compensated a) where
  compare m n = with m $ \a b -> with n $ \c d -> compare a c <> compare b d

instance EFT a => Monoid (Compensated a) where
  mempty = compensated 0 0
  {-# INLINE mempty #-}
  mappend = (+)
  {-# INLINE mappend #-}

instance (Bifunctor p, Profunctor p, Functor f, EFT a, a ~ b) => Cons p f (Compensated a) (Compensated b) a b where
  _Cons = unto $ \(a, e) -> with e $ \b c -> let y = a - c; t = b + y in compensated t ((t - b) - y)
  {-# INLINE _Cons #-}

instance (Bifunctor p, Profunctor p, Functor f, EFT a, a ~ b) => Snoc p f (Compensated a) (Compensated b) a b where
  _Snoc = unto $ \(e, a) -> with e $ \b c -> let y = a - c; t = b + y in compensated t ((t - b) - y)
  {-# INLINE _Snoc #-}

instance EFT a => Num (Compensated a) where
  m + n =
    with m $ \a  b  ->
    with n $ \c d ->
    add  a  c $ \x1 y1 ->
    add y1  d $ \x2 y2 ->
    add  b x2 $ \x3 y3 ->
    add x1 x3 $ \x4 y4 ->
    add x4 (y2 + y3 + y4) compensated
  {-# INLINE (+) #-}

  m * n =
    with m $ \a b ->
    with n $ \c d ->
    times a c $ \x1 y1 ->
    times b c $ \x2 y2 ->
    times a d $ \x3 y3 ->
    add x1 x2 $ \x4 y4 ->
    add x3 x4 $ \x5 y5 ->
    add y1 y4 $ \x6 y6 ->
    add y5 x6 $ \x7 y7 ->
    add x5 x7 $ \x8 y8 ->
    add x8 (b*d + y2 + y3 + y6 + y7 + y8) compensated
  {-# INLINE (*) #-}

  negate m = with m $ \a b -> compensated (negate a) (negate b)
  {-# INLINE negate #-}

  x - y = x + negate y
  {-# INLINE (-) #-}

  signum m = with m $ \a _ -> compensated (signum a) 0
  {-# INLINE signum #-}

  abs m = with m $ \a b ->
    if a < 0
    then compensated (negate a) (negate b)
    else compensated a b
  {-# INLINE abs #-}

  fromInteger i = add x (fromInteger (i - round x)) compensated where
    x = fromInteger i
  {-# INLINE fromInteger #-}

instance EFT a => Fractional (Compensated a) where
  recip m = with m $ \a b -> add (recip a) (-b / (a * a)) compensated
  {-# INLINE recip #-}

  m / n =
    with m $ \a b ->
    with n $ \c d ->
    times (a/c) (d/c) $ \x1 y1 ->
    add x1 (b/c) $ \x2 y2 ->
    add x2 (a/c) $ \x3 y3 ->
    add x3 (y1 + y2 + y3) compensated
  {-# INLINE (/) #-}

  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
  {-# INLINE fromRational #-}

instance EFT a => Real (Compensated a) where
  toRational m = with m $ \a b -> toRational a + toRational b
  {-# INLINE toRational #-}

instance EFT a => RealFrac (Compensated a) where
  properFraction m = with m $ \a b -> case properFraction a of
    (w, p) -> add p b $ \ x y -> case properFraction x of
      (w',q) -> (w + w', add q y compensated)
  {-# INLINE properFraction #-}
