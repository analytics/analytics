{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeFamilies #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- Compensated floating point summation based on Knuth's error free
-- transformation, various algorithms by Ogita, Rump and Oishi and
-- Kahan summation, with custom compensated arithetic circuits to
-- do multiplication, division, etc. of compensated numbers.
--
-- In general if @a@ has x bits of significand, @Compensated a@ gives
-- you twice that. You can iterate this construction for arbitrary
-- precision.
--------------------------------------------------------------------
module Data.Analytics.Approximate.Summation
  ( EFT(..)
  , _Compensated
  , Overcompensated
  , primal
  , residual
  , uncompensated
  , kahan
  , add
  , times
  , split
  ) where

import Control.Applicative
import Control.Lens
import Data.Foldable as Foldable
import Data.Ratio
import Data.Semigroup
import Text.Read
-- import Data.Vector.Generic as Generic

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
  -- | This provides a numeric data type with effectively doubled precision by
  -- using Knuth's error free transform and a number of custom compensated
  -- arithmetic circuits.
  --
  -- This construction can be iterated, doubling precision each time.
  --
  -- >>> round (product [2..100] :: Compensated (Compensated (Compensated Double)))
  -- 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
  --
  -- >>> product [2..100]
  -- 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
  data Compensated a

  -- | This extracts both the 'primal' and 'residual' components of a 'Compensated'
  -- number.
  with :: EFT a => Compensated a -> (a -> a -> r) -> r

  -- | Used internally to construct 'compensated' values that satisfy our residual contract.
  --
  -- When in doubt, use @'add' a b 'compensated'@ instead of @'compensated' a b@
  compensated :: EFT a => a -> a -> Compensated a

  -- | This 'magic' number is used to 'split' the significand in half, so we can multiply
  -- them separately without losing precision in 'times'.
  magic :: a

-- | This provides the isomorphism between the compact representation we store these in internally
-- and the naive pair of the 'primal' and 'residual' components.
_Compensated :: EFT a => Iso' (Compensated a) (a, a)
_Compensated = iso (`with` (,)) (uncurry compensated)
{-# INLINE _Compensated #-}

instance EFT Double where
  data Compensated Double = CD {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  with (CD a b) k = k a b
  {-# INLINE with #-}
  compensated = CD
  {-# INLINE compensated #-}
  magic = 134217729
  {-# INLINE magic #-}

instance EFT Float where
  data Compensated Float = CF {-# UNPACK #-} !Float {-# UNPACK #-} !Float
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

instance (EFT a, Show a) => Show (Compensated a) where
  showsPrec d m = with m $ \a b -> showParen (d > 10) $
    showString "compensated " . showsPrec 11 a . showChar ' ' . showsPrec 11 b

instance (EFT a, Read a) => Read (Compensated a) where
  readPrec = parens $ prec 10 $ do
    Ident "compensated" <- lexP
    a <- step readPrec
    b <- step readPrec
    return $ compensated a b

-- | This 'Lens' lets us edit the 'primal' directly, leaving the 'residual' untouched.
primal :: EFT a => Lens' (Compensated a) a
primal f c = with c $ \a b -> f a <&> \a' -> compensated a' b
{-# INLINE primal #-}

-- | This 'Lens' lets us edit the 'residual' directly, leaving the 'primal' untouched.
residual :: EFT a => Lens' (Compensated a) a
residual f c = with c $ \a b -> compensated a <$> f b
{-# INLINE residual #-}

-- | Extract the 'primal' component of a 'compensated' value, when and if compensation
-- is no longer required.
uncompensated :: EFT a => Compensated a -> a
uncompensated c = with c const
{-# INLINE uncompensated #-}

instance EFT a => Eq (Compensated a) where
  m == n = with m $ \a b -> with n $ \c d -> a == c && b == d
  m /= n = with m $ \a b -> with n $ \c d -> a /= c && b /= d
  {-# INLINE (==) #-}

instance EFT a => Ord (Compensated a) where
  compare m n = with m $ \a b -> with n $ \c d -> compare a c <> compare b d
  {-# INLINE compare #-}
  m <= n = with m $ \a b -> with n $ \c d -> case compare a c of
    LT -> True
    EQ -> b <= d
    GT -> False
  {-# INLINE (<=) #-}
  m >= n = with m $ \a b -> with n $ \c d -> case compare a c of
    LT -> False
    EQ -> b >= d
    GT -> a >= c -- @compare x NaN@ and @compare naN x@ always return 'GT', but @m >= n@ should be 'False'
  {-# INLINE (>=) #-}
  m > n = with m $ \a b -> with n $ \c d -> case compare a c of
    LT -> False
    EQ -> b > d
    GT -> a > c -- @compare x NaN@ and @compare naN x@ always return 'GT', but @m >= n@ should be 'False'
  {-# INLINE (>) #-}
  m < n = with m $ \a b -> with n $ \c d -> case compare a c of
    LT -> True
    EQ -> b < d
    GT -> False
  {-# INLINE (<) #-}

instance EFT a => Semigroup (Compensated a) where
  (<>) = (+)
  {-# INLINE (<>) #-}

instance EFT a => Monoid (Compensated a) where
  mempty = compensated 0 0
  {-# INLINE mempty #-}
  mappend = (+)
  {-# INLINE mappend #-}

-- | Perform Kahan summation over a list.
kahan :: (Foldable f, EFT a) => f a -> Compensated a
kahan = Foldable.foldr cons mempty
{-# INLINE kahan #-}

-- dot :: Vector v a => v a -> v a -> Compensated a
-- dot = Generic.zipWith

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

instance EFT a => Enum (Compensated a) where
  succ a = a + 1
  {-# INLINE succ #-}
  pred a = a - 1
  {-# INLINE pred #-}
  toEnum i = add x (fromIntegral (i - round x)) compensated where
    x = fromIntegral i
  {-# INLINE toEnum #-}
  fromEnum = round
  {-# INLINE fromEnum #-}
  enumFrom a = a : enumFrom (a + 1)
  {-# INLINE enumFrom #-}
  enumFromThen a b = a : enumFromThen b (b - a + b)
  {-# INLINE enumFromThen #-}
  enumFromTo a b
    | a <= b = a : enumFromTo (a + 1) b
    | otherwise = []
  {-# INLINE enumFromTo #-}
  enumFromThenTo a b c
    | a <= b    = up a
    | otherwise = down a
    where
     delta = b - a
     up x | x <= c    = x : up (x + delta)
          | otherwise = []
     down x | c <= x    = x : down (x + delta)
            | otherwise = []
  {-# INLINE enumFromThenTo #-}

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
