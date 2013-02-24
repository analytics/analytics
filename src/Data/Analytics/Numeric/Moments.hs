{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Numeric.Moments
  ( Moments(..)
  , HasMoments(..)
  , dim
  , variances, variance
  , stddevs, stddev
  , skewnesses, skewness
  , kurtoses, kurtosis
  , trimmed
  , singleton
  , momentsOf
  , combinedMean
  ) where

import Control.Applicative
import Control.Lens as L
import Control.Seq
import Data.Int
import Data.Foldable as F
import Data.Semigroup
import Data.Data
import Generics.Deriving
import GHC.Exts
import Data.Vector.Unboxed as U
import qualified Data.Vector as V

-- | The first few central moments and covariances

-- TODO: Consider storing intermediate results in compensated form for better precision
data Moments a
  = NoMoments -- this has no shape information
  | Moments
  { _rawCount       :: {-# UNPACK #-} !Int64 -- when 0, this may still carry shape information from 'dim' or from trimming the vector to remove skewness and kurtoses we don't want
  , _rawMeans       :: !(U.Vector a) -- means
  , _rawVariances   :: !(U.Vector a) -- variances
  , _rawCovariances :: {-# UNPACK #-} !(V.Vector (U.Vector a)) -- covariances
  , _rawSkewnesses  :: !(U.Vector a) -- skews
  , _rawKurtoses    :: !(U.Vector a) -- kurtoses
  } deriving (Show,Read,Data,Typeable,Generic)

makePrisms ''Moments
makeClassy ''Moments

instance (Unbox a, Fractional a, Ord a) => Monoid (Moments a) where
  mempty = NoMoments
  {-# INLINE mempty #-}
  NoMoments `mappend` x = x
  x `mappend` NoMoments = x
  Moments i mas vas cas sas kas `mappend` Moments j mbs vbs cbs sbs kbs = Moments
    (i+j)                                                                            -- count
    (U.zipWith (combinedMean i j) mas mbs)                                           -- means
    (U.zipWith3 calcVariance ds vas vbs)                                             -- variances
    (V.izipWith calcCovariance cas cbs `using` seqFoldable rseq)                     -- covariances
    (U.zipWith5 calcSkewness ds vas sas vbs sbs)                                     -- skewness
    (U.zipWith3 calcKurtosis2 kas kbs (U.zipWith5 calcKurtosis1 ds vas sas vbs sbs)) -- kurtoses
    where !ds = U.zipWith (-) mbs mas                                                -- delta means
          !i' = fromIntegral i
          !ii = i'*i'
          !j' = fromIntegral j
          !jj = j'*j'
          !k' = i'+j'
          !kk = k'*k'
          !ijk = i'*j'/k'
          !imj = i'-j'
          calcVariance !d !va !vb = va + vb + d*d*ijk
          calcCovariance !r | dy <- ds U.! r = U.zipWith3 (\dx ca cb -> ca + cb + dx*dy*ijk) ds
          calcSkewness !d !va !sa !vb !sb = sa + sb + d*d*d*ijk*imj/k' + 3*d*(i'*vb + j'*va)/k'
          calcKurtosis1 !d !va !sa !vb !sb = d*d*d*d*(ii-i'*j'+jj)*ijk/kk + 6*d*d*(ii*vb+jj*va)/kk + 4*d*(i'*sb-j'*sa)/k'
          calcKurtosis2 !ka !kb !dk = ka + kb + dk
  {-# INLINE mappend #-}

instance (Unbox a, Fractional a, Ord a) => Semigroup (Moments a) where
  (<>) = mappend
  {-# INLINE (<>) #-}

-- | Calculate an aggregate weighted mean.
combinedMean :: (Fractional a, Ord a) => Int64 -> Int64 -> a -> a -> a
combinedMean !m !n
  | m <= n    = inline go m n
  | otherwise = flip (inline go n m)
  where
    go 0 _ _ b = b
    go i j a b
      | abs scale < threshold = b + (a - b) * scale
      | otherwise = (fromIntegral i * a + fromIntegral j * b) / fromIntegral k
      where k = i + j
            scale = fromIntegral i / fromIntegral k
            threshold = 0.1
{-# INLINE combinedMean #-}

row :: (Unbox a, Num a) => U.Vector a -> Moments a
row as = Moments 1 as zs cvs zs zs where
  !n = U.length as
  !cvs = V.generate n (\i -> U.take i zs) -- upper triangular
  !zs = U.replicate n 0
{-# INLINE row #-}

-- | Generate an empty set of moments for a restricted number of dimensions
dim :: (Unbox a, Num a) => Int -> Moments a
dim n = Moments 0 zs zs cvs zs zs where
  !cvs = V.generate n (\i -> U.take i zs) -- upper triangular
  !zs = U.replicate n 0

momentsOf :: (Foldable f, Unbox a, Num a) => Getting (Moments a) s (f a) -> s -> Moments a
momentsOf l = foldMapOf l (row . U.fromList . F.toList)
{-# INLINE momentsOf #-}

-- | @'trimmed' n v@ will insert the vector @v@ into 'Moments'.
-- If 'Moments' summarizes at least @n@ elements, this will start rejecting
-- extreme components of the vector, replacing them with the @mean@.
trimmed :: (Unbox a, Floating a, Ord a) => Int64 -> U.Vector a -> Moments a -> Moments a
trimmed k as r@(Moments n ms vs _ _ _)
  | n >= k = U.zipWith3 go as ms vs <| r where
  n' = fromIntegral n
  go a m v
    | amm <- a - m, n'*amm*amm >= 9*v = m
    | otherwise = a
trimmed _ as r = as <| r

-- this lets us use 'cons' to add a moment to the mix.
instance (Bifunctor p, Profunctor p, Functor f, Unbox a, Fractional a, Ord a) => Cons p f (Moments a) (Moments a) (U.Vector a) (U.Vector a) where
  -- TODO: Use Welford's algorithm directly (extended by Terriberry)
  _Cons = unto $ \(d,m) -> row d <> m
  {-# INLINE _Cons #-}

instance (Bifunctor p, Profunctor p, Functor f, Unbox a, Fractional a, Ord a) => Snoc p f (Moments a) (Moments a) (U.Vector a) (U.Vector a) where
  -- TODO: Use Welford's algorithm directly (extended by Terriberry)
  _Snoc = unto $ \(m,d) -> m <> row d
  {-# INLINE _Snoc #-}

dividedBy :: Fractional a => a -> Iso' a a
dividedBy s = iso (/s) (*s)

root :: Floating a => Iso' a a
root = iso sqrt (\x -> x * x)

-- | Extract the 'variance' as a moment around the 'mean'.
--
-- /NB:/ setting this will render the higher 'Moments' inconsistent.
--
variances :: (HasMoments t a, Unbox a, Fractional a) => IndexedFold Int t a
variances f = moments ago where
  ago (Moments i _ vs _ _ _) | i > 0 = coerce $ each (dividedBy (fromIntegral i) f) vs
  ago m = pure m
{-# INLINE variances #-}

variance :: (HasMoments t a, Unbox a, Fractional a) => Int -> IndexedFold Int t a
variance k f = moments ago where
  ago (Moments i _ vs _ _ _) | i > 0, k >= 0 && k < U.length vs = coerce $ L.indexed f k ((vs U.! k) / fromIntegral i)
  ago m = pure m
{-# INLINE variance #-}

-- | /NB:/ setting this will render the higher 'Moments' inconsistent.
stddevs :: (HasMoments t a, Unbox a, Floating a) => IndexedFold Int t a
stddevs = variances.root
{-# INLINE stddevs #-}

-- | Retrieve the nth std deviation from our set.
stddev :: (HasMoments t a, Unbox a, Floating a) => Int -> IndexedFold Int t a
stddev k = variance k . root
{-# INLINE stddev #-}

-- | Calculate 'skewness'.
--  This is a 'Fold' because 'skewness' may not be defined for some combinations of 'Moments'.
skewnesses :: (HasMoments t a, Unbox a, Floating a, Ord a) => IndexedFold Int t a
skewnesses f = moments ago where
  ago (Moments n _ bs _ cs _) | n > 0 = coerce $ each (Indexed go) $ U.zip bs cs where
    sqn = sqrt (fromIntegral n)
    go i (b,c)
      | ood >= 1e12 = pure 0 -- TODO: condition the epsilon better
      | otherwise   = L.indexed f i (sqn * c * ood)
      where ood = b ** (-1.5)
  ago m = pure m
{-# INLINE skewnesses #-}

-- | Calculate a 'skewness'.
--
-- This is a 'Fold' because 'skewness' may not be defined for some combinations of 'Moments'.
skewness :: (HasMoments t a, Unbox a, Floating a, Ord a) => Int -> IndexedFold Int t a
skewness k f = moments ago where
  ago (Moments n _ bs _ cs _) | k >= 0, k < min (U.length bs) (U.length cs) = coerce $ go (bs U.! k) (cs U.! k) where
    sqn = sqrt (fromIntegral n)
    go b c
      | ood >= 1e12 = pure 0 -- TODO: condition the epsilon better
      | otherwise   = L.indexed f k (sqn * c * ood)
      where !ood = b ** (-1.5)
  ago m = pure m
{-# INLINE skewness #-}

-- | Calculate 'kurtosis'.
-- This is a 'Fold' because 'kurtosis' may not be defined for some combinations of 'Moments'.
kurtoses :: (HasMoments t a, Unbox a, Floating a, Ord a) => IndexedFold Int t a
kurtoses f = moments ago where
  ago NoMoments = pure NoMoments
  ago (Moments n _ bs _ _ ds) = coerce $ each (Indexed go) $ U.zip bs ds where
    !m = fromIntegral n
    go i (b,d)
      | b <= 1e-6 = pure 0
      | otherwise = L.indexed f i (m*d/(b*b) - 3)
{-# INLINE kurtoses #-}

-- | Calculate 'kurtosis'.
-- This is a 'Fold' because 'kurtosis' may not be defined for some combinations of 'Moments'.
kurtosis :: (HasMoments t a, Unbox a, Floating a, Ord a) => Int -> IndexedFold Int t a
kurtosis k f = moments ago where
  ago (Moments n _ bs _ _ ds) | k >= 0 && k < min (U.length bs) (U.length ds) = coerce $ go (bs U.! k) (ds U.! k) where
    !m = fromIntegral n
    go b d
      | b <= 1e-6 = pure 0
      | otherwise = L.indexed f k (m*d/(b*b) - 3)
  ago m = pure m
{-# INLINE kurtosis #-}

{-
covariances :: HasMoments t => Traversal' t (U.Array (Int,Int) Double)
covariances f = moments ago where
  ago NoMoments               = pure NoMoments
  ago m@(Moments 0 _ _ _ _ _) = pure m
  ago (Moments i ms vs cvs ss ks) = listArray bds [ getCV i j | i <- [0..nvs-1], j <- [0..nvs]] where
    !bds = (0,0),(nvs-1,nvs-1)
    zipWith (range bds) 
    !nvs = length vs
    getCV i j = case compare i j of
      LT -> inline go i j
      EQ -> vs


covariance :: HasMoments t => Int -> Int -> Traversal' t Double
covariance i j = case compare i j of
  LT -> inline go i j
  EQ -> \f -> variances $ ix i . L.indexed f (i,j)
  GT -> inline go j i
  where
    go _  _  _ NoMoments = pure NoMoments
    go lo hi f (Moments i ms vs cvs ss ks) = (ix hi<.>ix lo.oi) cvs <&> \cvs' -> Moments i ms vs cvs' ss ks
      where oi = iso (/fromIntegral i) (*fromIntegral i)
-}
