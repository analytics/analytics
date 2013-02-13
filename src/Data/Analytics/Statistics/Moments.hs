{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Analytics.Statistics.Moments
  ( Moments(..)
  , HasMoments(..)
  , variance
  , stddev
  , skewness
  , kurtosis
  , singleton
  , momentsOf
  ) where

import Control.Applicative
import Control.Lens
import Data.Int
import Data.Semigroup
import Data.Data
import Generics.Deriving
import GHC.Exts

-- | The first few central moments.
data Moments = Moments
  { _count   :: {-# UNPACK #-} !Int64
  , _mean    :: {-# UNPACK #-} !Double
  , _moment2 :: {-# UNPACK #-} !Double
  , _moment3 :: {-# UNPACK #-} !Double
  , _moment4 :: {-# UNPACK #-} !Double
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

makeClassy ''Moments

instance Monoid Moments where
  mempty = Moments 0 0 0 0 0
  {-# INLINE mempty #-}
  Moments i a b c d `mappend` Moments j e f g h
    | k == 0    = mempty
    | otherwise = Moments k (combinedMean i a j e) (b + f + z2*ij/dk)
      (c + g + z3*ij*imj/k2 + 3*z*(di*f - dj*b)/dk)
      (d + h + z4*ij*(i2 - ij + j2)/k3 + 6*z2*(i2*f + j2*c)/k2 + 4*z*(di*h - dj*d)/dk)
    where !k   = i + j
          !k2  = dk*dk
          !k3  = k2*dk
          !i2  = di*di
          !j2  = dj*dj
          !z   = e - a
          !z2  = z*z
          !z3  = z2*z
          !z4  = z2*z2
          !ij  = di*dj
          !imj = di-dj
          !dk  = fromIntegral k
          !di  = fromIntegral i
          !dj  = fromIntegral j
  {-# INLINE mappend #-}

instance Semigroup Moments where
  (<>) = mappend
  {-# INLINE (<>) #-}

-- | Aggregate weighted mean.
combinedMean :: Int64 -> Double -> Int64 -> Double -> Double
combinedMean !m !x !n !y
  | m <= n    = inline go m x n y
  | otherwise = inline go n y m x
  where
    go 0 _ _ b = b
    go i a j b
      | abs scale < threshold = b + (a - b) * scale
      | otherwise = (fromIntegral i * a + fromIntegral j * b) / fromIntegral k
      where k = i + j
            scale = fromIntegral i / fromIntegral k
            threshold = 0.1
{-# INLINE combinedMean #-}

singleton :: Real a => a -> Moments
singleton a = Moments 1 (realToFrac a) 0 0 0
{-# INLINE singleton #-}

momentsOf :: Real a => Getting Moments s t a b -> s -> Moments
momentsOf l = foldMapOf l singleton
{-# INLINE momentsOf #-}

-- this lets us use 'cons' to add a moment to the mix.
instance (Bifunctor p, Profunctor p, Functor f, Real a, a ~ b) => Cons p f Moments Moments a b where
  _Cons = unto $ \(d,m) -> singleton d <> m
  {-# INLINE _Cons #-}

instance (Bifunctor p, Profunctor p, Functor f, Real a, a ~ b) => Snoc p f Moments Moments a b where
  _Snoc = unto $ \(m,d) -> m <> singleton d
  {-# INLINE _Snoc #-}

-- | Extract the 'variance' as a moment around the 'mean'.
--
-- /NB:/ setting this will render the higher 'Moments' inconsistent.
variance :: HasMoments t => Traversal' t Double
variance f = moments go where
  go m@(Moments 0 _ _ _ _) = pure m
  go (Moments i a b c d)   = f (b / fromIntegral i) <&> \v -> Moments i a (v * fromIntegral i) c d
{-# INLINE variance #-}

-- | /NB:/ setting this will render the higher 'Moments' inconsistent.
stddev :: HasMoments t => Traversal' t Double
stddev = variance.iso sqrt square where
  square x = x * x
{-# INLINE stddev #-}

-- | Calculate 'skewness'.
--  This is a 'Fold' because 'skewness' may not be defined for some combinations of 'Moments'.
skewness :: HasMoments t => Fold t Double
skewness f = moments go where
  go (Moments i _ b c _)
    | abs denom <= 1e-12 = coerce $ pure ()
    | otherwise = coerce $ f (sqrt (fromIntegral i) * c / denom)
    where denom = b**1.5
{-# INLINE skewness #-}

-- | Calculate 'kurtosis'.
-- This is a 'Fold' because 'kurtosis' may not be defined for some combinations of 'Moments'.
kurtosis :: HasMoments t => Fold t Double
kurtosis f = moments go where
  go (Moments i _ b _ d)
    | abs denom <= 1e-12 = coerce $ pure ()
    | otherwise = coerce $ f (fromIntegral i * d / denom - 3)
    where denom = b*b
{-# INLINE kurtosis #-}

instance Field1 Moments Moments Double Double where
  _1 f (Moments i a b c d) = indexed f (1 :: Int) a <&> \a' -> Moments i a' b c d
  {-# INLINE _1 #-}

instance Field2 Moments Moments Double Double where
  _2 f (Moments i a b c d) = indexed f (2 :: Int) b <&> \b' -> Moments i a b' c d
  {-# INLINE _2 #-}

instance Field3 Moments Moments Double Double where
  _3 f (Moments i a b c d) = indexed f (3 :: Int) c <&> \c' -> Moments i a b c' d
  {-# INLINE _3 #-}

instance Field4 Moments Moments Double Double where
  _4 f (Moments i a b c d) = indexed f (4 :: Int) d <&> \d' -> Moments i a b c d'
  {-# INLINE _4 #-}

type instance Index Moments = Int

instance (Contravariant f, Applicative f) => Each f Moments Moments Double Double where
  each f (Moments i a b c d) = coerce $ indexed f (1 :: Int) (fromIntegral i) *> indexed f (1 :: Int) a *> indexed f (2 :: Int) b *> indexed f (3 :: Int) c *> indexed f (4 :: Int) d
  {-# INLINE each #-}

type instance IxValue Moments = Double

instance (Contravariant f, Applicative f) => Ixed f Moments where
  ix 0 = \f (Moments i _ _ _ _) -> coerce $ indexed f (0 :: Int) (fromIntegral i :: Double)
  ix 1 = \f (Moments _ a _ _ _) -> coerce $ indexed f (1 :: Int) a
  ix 2 = \f (Moments _ _ b _ _) -> coerce $ indexed f (2 :: Int) b
  ix 3 = \f (Moments _ _ _ c _) -> coerce $ indexed f (3 :: Int) c
  ix 4 = \f (Moments _ _ _ _ d) -> coerce $ indexed f (4 :: Int) d
  ix _ = \_ _                   -> coerce $ pure ()
  {-# INLINE ix #-}

instance (Contravariant f, Applicative f) => Contains f Moments where
  contains = containsN 5
  {-# INLINE contains #-}

