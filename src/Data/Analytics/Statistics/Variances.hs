{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Analytics.Statistics.Variances
  ( Variances(Variances)
  , HasVariances(..)
  , doubleton
  , variancesOf
  , covariance
  , correlation
  , varianceX
  , varianceY
  , stddevX
  , stddevY
  ) where

import Control.Applicative
import Control.Lens
import Data.Int
import Data.Semigroup
import Data.Data
import Generics.Deriving
import GHC.Exts

-- TODO: Use an nxn matrix and include skewness, kurtosis for the diagonal, then this can subsume 'Moments'.

data Variances = Variances
  { _points   :: {-# UNPACK #-} !Int64
  , _meanX    :: {-# UNPACK #-} !Double
  , _meanY    :: {-# UNPACK #-} !Double
  , _covXY    :: {-# UNPACK #-} !Double
  , _moment2X :: {-# UNPACK #-} !Double
  , _moment2Y :: {-# UNPACK #-} !Double
  } deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

makeClassy ''Variances

instance Monoid Variances where
  mempty = Variances 0 0 0 0 0 0
  {-# INLINE mempty #-}
  Variances i xa ya ca vxa vya `mappend` Variances j xb yb cb vxb vyb
    | k == 0 = mempty
    | otherwise = Variances k (combinedMean i xa j xb) (combinedMean i ya j yb)
      (ca + cb + dx * dy * ij / n)
      (vxa + vxb + dx * dx * ij / n)
      (vya + vyb + dy * dy * ij / n)
    where !dx = xa - xb
          !dy = ya - yb
          !k  = i + j
          !n  = fromIntegral k
          !ij = fromIntegral i * fromIntegral j
  {-# INLINE mappend #-}

instance Semigroup Variances where
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

doubleton :: (Real a, Real b) => a -> b -> Variances
doubleton a b = Variances 1 (realToFrac a) (realToFrac b) 0 0 0
{-# INLINE doubleton #-}

variancesOf :: (Real a, Real b) => Getting Variances s t (a,b) (a,b) -> s -> Variances
variancesOf l = foldMapOf l (uncurry doubleton)
{-# INLINE variancesOf #-}

instance (Bifunctor p, Profunctor p, Functor f, Real a, Real b, a ~ c, b ~ d) => Cons p f Variances Variances (a,b) (c,d) where
  _Cons = unto $ \(d,m) -> uncurry doubleton d <> m
  {-# INLINE _Cons #-}

instance (Bifunctor p, Profunctor p, Functor f, Real a, Real b, a ~ c, b ~ d) => Snoc p f Variances Variances (a,b) (c,d) where
  _Snoc = unto $ \(m,d) -> m <> uncurry doubleton d
  {-# INLINE _Snoc #-}

covariance :: HasVariances t => Traversal' t Double
covariance f = variances go where
  go m@(Variances 0 _ _ _ _ _) = pure m
  go (Variances i x y c vx vy) = f (c / fromIntegral i) <&> \v -> Variances i x y (v * fromIntegral i) vx vy
{-# INLINE covariance #-}

-- | Extract the correlation coefficient
correlation :: HasVariances t => Traversal' t Double
correlation f = variances go where
  go m@(Variances i x y c vx vy)
    | i == 0 || abs vxy <= 1e-12 = pure m
    | otherwise = f (c / vxy) <&> \v -> Variances i x y (v * vxy) vx vy
    where vxy = sqrt vx * sqrt vy
{-# INLINE correlation #-}

-- | Extract the 'variance' as a moment around the 'mean'.
varianceX :: HasVariances t => Traversal' t Double
varianceX f = variances go where
  go m@(Variances 0 _ _ _ _ _) = pure m
  go (Variances i x y c vx vy) = f (vx / fromIntegral i) <&> \v -> Variances i x y c (v * fromIntegral i) vy
{-# INLINE varianceX #-}

-- | Extract the 'variance' as a moment around the 'mean'.
varianceY :: HasVariances t => Traversal' t Double
varianceY f = variances go where
  go m@(Variances 0 _ _ _ _ _) = pure m
  go (Variances i x y c vx vy) = f (vy / fromIntegral i) <&> \v -> Variances i x y c vx (v * fromIntegral i)
{-# INLINE varianceY #-}

stddevX :: HasVariances t => Traversal' t Double
stddevX = varianceX.iso sqrt (\ x -> x * x)
{-# INLINE stddevX #-}

stddevY :: HasVariances t => Traversal' t Double
stddevY = varianceY.iso sqrt (\ x -> x * x)
{-# INLINE stddevY #-}
