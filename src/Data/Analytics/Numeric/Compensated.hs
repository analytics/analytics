{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveDataTypeable #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module provides a fairly extensive API for compensated
-- floating point arithmetic based on Knuth's error free
-- transformation, various algorithms by Ogita, Rump and Oishi,
-- Hida, Li and Bailey, Kahan summation, etc. with custom compensated
-- arithetic circuits to do multiplication, division, etc. of compensated
-- numbers.
--
-- In general if @a@ has x bits of significand, @Compensated a@ gives
-- you twice that. You can iterate this construction for arbitrary
-- precision.
--
-- References:
--
-- * <http://web.mit.edu/tabbott/Public/quaddouble-debian/qd-2.3.4-old/docs/qd.pdf>
-- * <http://www.ti3.tuhh.de/paper/rump/OgRuOi05.pdf>
-- * Donald Knuth's \"The Art of Computer Programming, Volume 2: Seminumerical Algorithms\"
-- * <http://en.wikipedia.org/wiki/Kahan_summation_algorithm>
--------------------------------------------------------------------
module Data.Analytics.Numeric.Compensated
  ( Compensable(..)
  , _Compensated
  , Overcompensated
  , primal
  , residual
  , uncompensated
  , fadd
  -- * lifting scalars
  , add, times, squared, divide, split
  , kahan, (+^), (*^)
  -- * compensated operators
  , square
  ) where

import Control.Applicative
import Control.Lens as L
import Control.Monad
import Data.Analytics.Numeric.Log
import Data.Foldable as Foldable
import Data.Function (on)
import Data.Ratio
import Data.Semigroup
import Data.Vector.Unboxed as U
import Data.Vector.Generic as G
import Data.Vector.Generic.Mutable as M
import Data.Data
import Foreign.Ptr
import Foreign.Storable
import Text.Read as T
import Text.Show as T

{-# ANN module "hlint: ignore Use -" #-}
{-# ANN module "hlint: ignore Use curry" #-}

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

-- | @'fadd' a b k@ computes @k x y@ such that
--
-- > x + y = a + b
-- > x = fl(a + b)
--
-- but only under the assumption that @'abs' a '>=' 'abs' b@. If you
-- aren't sure, use 'add'.
--
-- Which is to say that @x@ is the floating point image of @(a + b)@ and
-- @y@ stores the residual error term.
fadd :: Num a => a -> a -> (a -> a -> r) -> r
fadd a b k = k x (b - (x - a)) where
  x = a + b
{-# INLINE fadd #-}

-- | @'times' a b k@ computes @k x y@ such that
--
-- > x + y = a * b
-- > x = fl(a * b)
--
-- Which is to say that @x@ is the floating point image of @(a * b)@ and
-- @y@ stores the residual error term.
--
-- This could be nicer if we had access to a hardware fused multiply-add.
times :: Compensable a => a -> a -> (a -> a -> r) -> r
times a b k =
  split a $ \a1 a2 ->
  split b $ \b1 b2 ->
  let x = a * b in k x (((a1*b1-x)+a1*b2+b2*b1)+a2*b2)
{-# INLINEABLE times #-}

-- this is a variant on a division algorithm by Liddicoat and Flynn
divide :: Compensable a => a -> a -> (a -> a -> r) -> r
divide a b = with (aX * ms) where
  x0   = recip b
  aX   = times a x0 compensated -- calculate aX
  m    = 1 <| negate (times b x0 compensated)
  mm   = m*m
  ms   = 1+((m+mm)+m*mm)
{-# INLINEABLE divide #-}

-- | Priest's renormalization algorithm
--
-- @renorm a b c@ generates a 'Compensated' number assuming @a '>=' b '>=' c@.
renorm :: Compensable a => a -> a -> a -> Compensated a
renorm a b c =
  fadd b c $ \x1 y1 ->
  fadd a x1 $ \x2 y2 ->
  fadd x2 (y1 + y2) compensated
{-# INLINE renorm #-}

-- | @'squared' a k@ computes @k x y@ such that
--
-- > x + y = a * a
-- > x = fl(a * a)
--
-- Which is to say that @x@ is the floating point image of @(a * a)@ and
-- @y@ stores the residual error term.
squared :: Compensable a => a -> (a -> a -> r) -> r
squared a k =
  split a $ \a1 a2 ->
  let x = a * a in k x (a2*a2 - ((x - a1*a1) - 2*(a2*a1)))
{-# INLINE squared #-}

-- | Calculate a fast square of a compensated number.
square :: Compensable a => Compensated a -> Compensated a
square m =
  with m $ \a b ->
  squared a $ \x1 y1 ->
  times a b $ \x2 y2 ->
  add y1 (x2*2) $ \x3 y3 ->
  renorm x1 x3 (b*b + 2*y2 + y3)
{-# INLINE square #-}

-- | error-free split of a floating point number into two parts.
--
-- Note: these parts do not satisfy the `compensated` contract
split :: Compensable a => a -> (a -> a -> r) -> r
split a k = k x y where
  c = magic*a
  x = c - (c - a)
  y = a - x
{-# INLINEABLE split #-}

-- | Calculate a scalar + compensated sum with Kahan summation.
(+^) :: Compensable a => a -> Compensated a -> Compensated a
a +^ m = with m $ \b c -> let y = a - c; t = b + y in compensated t ((t - b) - y)
{-# INLINE (+^) #-}

-- | Compute @a * 'Compensated' a@
(*^) :: Compensable a => a -> Compensated a -> Compensated a
c *^ m =
  with m $ \ a b ->
  times c a $ \x1 y1 ->
  times c b $ \x2 y2 ->
  fadd x1 x2 $ \x3 y3 ->
  add y1 y3 $ \x4 y4 ->
  renorm x3 x4 (y4 + y2)
{-# INLINE (*^) #-}

class (RealFrac a, Precise a, Floating a) => Compensable a where
  -- | This provides a numeric data type with effectively doubled precision by
  -- using Knuth's error free transform and a number of custom compensated
  -- arithmetic circuits.
  --
  -- This construction can be iterated, doubling precision each time.
  --
  -- >>> round (Prelude.product [2..100] :: Compensated (Compensated (Compensated Double)))
  -- 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
  --
  -- >>> Prelude.product [2..100]
  -- 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
  data Compensated a

  -- | This extracts both the 'primal' and 'residual' components of a 'Compensated'
  -- number.
  with :: Compensable a => Compensated a -> (a -> a -> r) -> r

  -- | Used internally to construct 'compensated' values that satisfy our residual contract.
  --
  -- When in doubt, use @'add' a b 'compensated'@ instead of @'compensated' a b@
  compensated :: Compensable a => a -> a -> Compensated a

  -- | This 'magic' number is used to 'split' the significand in half, so we can multiply
  -- them separately without losing precision in 'times'.
  magic :: a

instance Compensable Double where
  data Compensated Double = CD {-# UNPACK #-} !Double {-# UNPACK #-} !Double
  with (CD a b) k = k a b
  {-# INLINE with #-}
  compensated = CD
  {-# INLINE compensated #-}
  magic = 134217729
  {-# INLINE magic #-}

instance Compensable Float where
  data Compensated Float = CF {-# UNPACK #-} !Float {-# UNPACK #-} !Float
  with (CF a b) k = k a b
  {-# INLINE with #-}
  compensated = CF
  {-# INLINE compensated #-}
  magic = 4097
  {-# INLINE magic #-}

instance Compensable a => Compensable (Compensated a) where
  data Compensated (Compensated a) = CC !(Compensated a) !(Compensated a)
  with (CC a b) k = k a b
  {-# INLINE with #-}
  compensated = CC
  {-# INLINE compensated #-}
  magic = times (magic - 1) (magic - 1) $ \ x y -> compensated x (y + 1)
  {-# INLINE magic #-}

instance Typeable1 Compensated where
  typeOf1 _ = mkTyConApp (mkTyCon3 "analytics" "Data.Analytics.Numeric.Compensated" "Compensated") []

instance (Compensable a, Data a) => Data (Compensated a) where
  gfoldl f z m = with m $ \a b -> z compensated `f` a `f` b
  toConstr _ = compensatedConstr
  gunfold k z c = case constrIndex c of
    1 -> k (k (z compensated))
    _ -> error "gunfold"
  dataTypeOf _ = compensatedDataType
  dataCast1 f = gcast1 f

compensatedConstr :: Constr
compensatedConstr = mkConstr compensatedDataType "compensated" [] Prefix
{-# NOINLINE compensatedConstr #-}

compensatedDataType :: DataType
compensatedDataType = mkDataType "Data.Analytics.Numeric.Compensated" [compensatedConstr]
{-# NOINLINE compensatedDataType #-}

instance (Compensable a, Show a) => Show (Compensated a) where
  showsPrec d m = with m $ \a b -> showParen (d > 10) $
    showString "compensated " . T.showsPrec 11 a . showChar ' ' . T.showsPrec 11 b

instance (Compensable a, Read a) => Read (Compensated a) where
  readPrec = parens $ prec 10 $ do
    Ident "compensated" <- lexP
    a <- step T.readPrec
    b <- step T.readPrec
    return $ compensated a b

type Overcompensated a = Compensated (Compensated a)

-- | This provides the isomorphism between the compact representation we store these in internally
-- and the naive pair of the 'primal' and 'residual' components.
_Compensated :: Compensable a => Iso' (Compensated a) (a, a)
_Compensated = iso (`with` (,)) (uncurry compensated)
{-# INLINE _Compensated #-}

-- | This 'Lens' lets us edit the 'primal' directly, leaving the 'residual' untouched.
primal :: Compensable a => Lens' (Compensated a) a
primal f c = with c $ \a b -> f a <&> \a' -> compensated a' b
{-# INLINE primal #-}

-- | This 'Lens' lets us edit the 'residual' directly, leaving the 'primal' untouched.
residual :: Compensable a => Lens' (Compensated a) a
residual f c = with c $ \a b -> compensated a <$> f b
{-# INLINE residual #-}

-- | Extract the 'primal' component of a 'compensated' value, when and if compensation
-- is no longer required.
uncompensated :: Compensable a => Compensated a -> a
uncompensated c = with c const
{-# INLINE uncompensated #-}

type instance Index (Compensated a) = Int
instance (Applicative f, Compensable a, Compensable b) => Each f (Compensated a) (Compensated b) a b where
  each f m = with m $ \a b -> compensated <$> L.indexed f (0 :: Int) a <*> L.indexed f (1 :: Int) b
  {-# INLINE each #-}

instance Compensable a => Eq (Compensated a) where
  m == n = with m $ \a b -> with n $ \c d -> a == c && b == d
  m /= n = with m $ \a b -> with n $ \c d -> a /= c && b /= d
  {-# INLINE (==) #-}

instance Compensable a => Ord (Compensated a) where
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

instance Compensable a => Semigroup (Compensated a) where
  (<>) = (+)
  {-# INLINE (<>) #-}

instance Compensable a => Monoid (Compensated a) where
  mempty = compensated 0 0
  {-# INLINE mempty #-}
  mappend = (+)
  {-# INLINE mappend #-}

-- | Perform Kahan summation over a list.
kahan :: (Foldable f, Compensable a) => f a -> Compensated a
kahan = Foldable.foldr (+^) mempty
{-# INLINE kahan #-}

-- (<|) = (+^)
instance (Bifunctor p, Profunctor p, Functor f, Compensable a, a ~ b) => Cons p f (Compensated a) (Compensated b) a b where
  _Cons = unto $ \(a, e) -> with e $ \b c -> let y = a - c; t = b + y in compensated t ((t - b) - y)
  {-# INLINE _Cons #-}

-- (|>) = (+^)
instance (Bifunctor p, Profunctor p, Functor f, Compensable a, a ~ b) => Snoc p f (Compensated a) (Compensated b) a b where
  _Snoc = unto $ \(e, a) -> with e $ \b c -> let y = a - c; t = b + y in compensated t ((t - b) - y)
  {-# INLINE _Snoc #-}

instance Compensable a => Num (Compensated a) where
  m + n =
    with m $ \a b ->
    with n $ \c d ->
    add a c $ \x1 y1 ->
    add b d $ \x2 y2 ->
    renorm x1 x2 (y1 + y2)
  {-# INLINE (+) #-}

  m * n =
    with m $ \a b ->
    with n $ \c d ->
    times a c $ \x1 y1 ->
    times b c $ \x2 y2 ->
    times a d $ \x3 y3 ->
    add y1 x2 $ \x4 y4 ->
    add x3 x4 $ \x5 y5 ->
    renorm x1 x5 (b * d + y2 + y4 + y3 + y5)
  {-# INLINE (*) #-}

  negate m = with m (on compensated negate)
  -- {-# INLINE negate #-}

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

instance Compensable a => Enum (Compensated a) where
  succ a = a + 1
  {-# INLINE succ #-}
  pred a = a - 1
  {-# INLINE pred #-}
  toEnum i = add x (fromIntegral (i - round x)) compensated where
    x = fromIntegral i
  {-# INLINE toEnum #-}
  fromEnum = round
  {-# INLINE fromEnum #-}
  enumFrom a = a : Prelude.enumFrom (a + 1)
  {-# INLINE enumFrom #-}
  enumFromThen a b = a : Prelude.enumFromThen b (b - a + b)
  {-# INLINE enumFromThen #-}
  enumFromTo a b
    | a <= b = a : Prelude.enumFromTo (a + 1) b
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

instance Compensable a => Fractional (Compensated a) where
  recip m = with m $ \a b -> add (recip a) (-b / (a * a)) compensated
  {-# INLINE recip #-}

  -- | A variant on a hardware division algorithm by Liddicoat and Flynn
  a / b = (a*x0) * (1+((m+mm)+m*mm)) where
    x0  = recip b
    m   = 1 - b*x0
    mm  = m*m
  -- {-# INLINE (/) #-}

  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)
  -- {-# INLINE fromRational #-}

instance Compensable a => Real (Compensated a) where
  toRational m = with m (on (+) toRational)
  -- {-# INLINE toRational #-}

instance Compensable a => RealFrac (Compensated a) where
  properFraction m = with m $ \a b -> case properFraction a of
    (w, p) -> add p b $ \ x y -> case properFraction x of
      (w',q) -> (w + w', add q y compensated)
  -- {-# INLINE properFraction #-}

instance (Compensable a, Storable a) => Storable (Compensated a) where
  sizeOf _ = sizeOf (undefined :: a) * 2
  -- {-# INLINE sizeOf #-}
  alignment _ = alignment (undefined :: a)
  -- {-# INLINE alignment #-}
  peekElemOff p o | q <- castPtr p, o2 <- o * 2 =
    compensated <$> peekElemOff q o2 <*> peekElemOff q (o2+1)
  -- {-# INLINE peekElemOff #-}
  pokeElemOff p o m | q <- castPtr p, o2 <- o * 2 = with m $ \a b -> do
    pokeElemOff q o2 a
    pokeElemOff q (o2+1) b
  -- {-# INLINE pokeElemOff #-}
  peekByteOff p o | q <- castPtr p =
    compensated <$> peekByteOff q o <*> peekByteOff q (o + sizeOf (undefined :: a))
  -- {-# INLINE peekByteOff #-}
  pokeByteOff p o m | q <- castPtr p = with m $ \a b -> do
    pokeByteOff q o a
    pokeByteOff q (o+sizeOf (undefined :: a)) b
  -- {-# INLINE pokeByteOff #-}
  peek p | q <- castPtr p = compensated <$> peek q <*> peekElemOff q 1
  -- {-# INLINE peek #-}
  poke p m | q <- castPtr p = with m $ \a b -> do
    poke q a
    pokeElemOff q 1 b
  -- {-# INLINE poke #-}

newtype instance U.MVector s (Compensated a) = MV_Compensated (U.MVector s (a,a))
newtype instance U.Vector (Compensated a) = V_Compensated (U.Vector (a, a))

instance (Compensable a, Unbox a) => M.MVector U.MVector (Compensated a) where
  basicLength (MV_Compensated v) = M.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (MV_Compensated v) = MV_Compensated $ M.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicOverlaps (MV_Compensated v1) (MV_Compensated v2) = M.basicOverlaps v1 v2
  {-# INLINE basicOverlaps #-}
  basicUnsafeNew n = MV_Compensated `liftM` M.basicUnsafeNew n
  {-# INLINE basicUnsafeNew #-}
  basicUnsafeReplicate n m = with m $ \x y -> MV_Compensated `liftM` M.basicUnsafeReplicate n (x,y)
  {-# INLINE basicUnsafeReplicate #-}
  basicUnsafeRead (MV_Compensated v) i = uncurry compensated `liftM` M.basicUnsafeRead v i
  {-# INLINE basicUnsafeRead #-}
  basicUnsafeWrite (MV_Compensated v) i m = with m $ \ x y -> M.basicUnsafeWrite v i (x,y)
  {-# INLINE basicUnsafeWrite #-}
  basicClear (MV_Compensated v) = M.basicClear v
  {-# INLINE basicClear #-}
  basicSet (MV_Compensated v) m = with m $ \ x y -> M.basicSet v (x,y)
  {-# INLINE basicSet #-}
  basicUnsafeCopy (MV_Compensated v1) (MV_Compensated v2) = M.basicUnsafeCopy v1 v2
  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeMove (MV_Compensated v1) (MV_Compensated v2) = M.basicUnsafeMove v1 v2
  {-# INLINE basicUnsafeMove #-}
  basicUnsafeGrow (MV_Compensated v) n = MV_Compensated `liftM` M.basicUnsafeGrow v n
  {-# INLINE basicUnsafeGrow #-}

instance (Compensable a, Unbox a) => G.Vector U.Vector (Compensated a) where
  basicUnsafeFreeze (MV_Compensated v) = V_Compensated `liftM` G.basicUnsafeFreeze v
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeThaw (V_Compensated v) = MV_Compensated `liftM` G.basicUnsafeThaw v
  {-# INLINE basicUnsafeThaw #-}
  basicLength (V_Compensated v) = G.basicLength v
  {-# INLINE basicLength #-}
  basicUnsafeSlice i n (V_Compensated v) = V_Compensated $ G.basicUnsafeSlice i n v
  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeIndexM (V_Compensated v) i
                = uncurry compensated `liftM` G.basicUnsafeIndexM v i
  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeCopy (MV_Compensated mv) (V_Compensated v)
                = G.basicUnsafeCopy mv v
  {-# INLINE basicUnsafeCopy #-}
  elemseq _ m z = with m $ \x y -> G.elemseq (undefined :: U.Vector a) x
                                 $ G.elemseq (undefined :: U.Vector a) y z
  {-# INLINE elemseq #-}

-- | /NB:/ Experimental and partially implemented.
--
-- Other than sqrt, the accuracy of these is basically uncalculated! In fact many of these are known to be wrong! Patches and improvements are welcome.
instance Compensable a => Floating (Compensated a) where
#ifdef SPECIALIZE_INSTANCES
  {-# SPECIALIZE instance Floating (Compensated Double) #-}
  {-# SPECIALIZE instance Floating (Compensated Float) #-}
  {-# SPECIALIZE instance Compensable a => Floating (Compensated (Compensated a)) #-}
#endif

  exp m =
    with m $ \a b ->
    times (exp a) (exp b) compensated

  sin m =
    with m $ \a b ->
    times (sin a) (cos b) $ \x1 y1 ->
    times (sin b) (cos a) $ \x2 y2 ->
    add x1 x2 $ \x3 y3 ->
    add y1 y2 $ \x4 y4 ->
    add x4 y3 $ \x5 y5 ->
    add x5 x3 $ \x6 y6 ->
    add (y4 + y5 + y6) x6 compensated

  cos m =
    with m $ \a b ->
    times (cos a) (cos b) $ \x1 y1 ->
    times (-sin b) (sin a) $ \x2 y2 ->
    add x1 x2 $ \x3 y3 ->
    add y1 y2 $ \x4 y4 ->
    add x4 y3 $ \x5 y5 ->
    add x5 x3 $ \x6 y6 ->
    add (y4 + y5 + y6) x6 compensated

  tan m =
    with m $ \a b ->
    add (tan a) (tan b) compensated /
    (1 <| times (tan a) (tan b) compensated)

  sinh m =
    with m $ \a b ->
    times (sinh a) (cosh b) $ \x1 y1 ->
    times (cosh a) (sinh b) $ \x2 y2 ->
    add x1 x2 $ \x3 y3 ->
    add y1 y2 $ \x4 y4 ->
    add x4 y3 $ \x5 y5 ->
    add x5 x3 $ \x6 y6 ->
    add (y4 + y5 + y6) x6 compensated

  cosh m =
    with m $ \a b ->
    times (cosh a) (cosh b) $ \x1 y1 ->
    times (sinh b) (sinh a) $ \x2 y2 ->
    add x1 x2 $ \x3 y3 ->
    add y1 y2 $ \x4 y4 ->
    add x4 y3 $ \x5 y5 ->
    add x5 x3 $ \x6 y6 ->
    add (y4 + y5 + y6) x6 compensated

  tanh m =
    with m $ \a b ->
    add (tanh a) (tanh b) compensated /
    (1 +^ times (tanh a) (tanh b) compensated)

  -- This requires an accurate 'exp', which we currently lack.
  log m =
    with m $ \ a b -> let
      xy1 = add (log a) (b/a) compensated
      xy2 = xy1 + m * exp (-xy1) - 1 -- Newton Raphson step 1
    in xy2 + m * exp (-xy2) - 1      -- Newton Raphson step 2

  -- | Hardware sqrt improved by the Babylonian algorithm (Newton Raphson)
  sqrt m = with (z4 + m/z4) $ on compensated (/2) where
    z0 = sqrt (m^.primal)
    z1 = with (z0 <| (m / compensated z0 0)) $ on compensated (/2)
    z2 = with (z1 + m/z1) $ on compensated (/2)
    z3 = with (z2 + m/z2) $ on compensated (/2)
    z4 = with (z3 + m/z3) $ on compensated (/2)

  -- (**)    = error "TODO"
  pi      = error "TODO"
  asin    = error "TODO"
  atan    = error "TODO"
  acos    = error "TODO"
  asinh   = error "TODO"
  atanh   = error "TODO"
  acosh   = error "TODO"

-- | TODO: do this right!
instance (Compensable a, Precise a) => Precise (Compensated a) where
  log1p a = log (1 + a)
  {-# INLINE log1p #-}
  expm1 a = exp a - 1
  {-# INLINE expm1 #-}
