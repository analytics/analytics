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
  , trimmed
  -- , skewness
  -- , kurtosis
  , singleton
  , momentsOf
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
data Moments
  = NoMoments -- this has no shape information
  | Moments
  { _rawCount       :: {-# UNPACK #-} !Int64 -- when 0, this may still carry shape information from 'dim' or from trimming the vector to remove skewness and kurtoses we don't want
  , _rawMeans       :: !(U.Vector Double) -- means
  , _rawVariances   :: !(U.Vector Double) -- variances
  , _rawCovariances :: {-# UNPACK #-} !(V.Vector (U.Vector Double)) -- covariances
  , _rawSkewnesses  :: !(U.Vector Double) -- skews
  , _rawKurtoses    :: !(U.Vector Double) -- kurtoses
  } deriving (Show,Read,Data,Typeable,Generic)

makePrisms ''Moments
makeClassy ''Moments

instance Monoid Moments where
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

instance Semigroup Moments where
  (<>) = mappend
  {-# INLINE (<>) #-}

-- | Aggregate weighted mean.
combinedMean :: Int64 -> Int64 -> Double -> Double -> Double
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

row :: U.Vector Double -> Moments
row as = Moments 1 as zs cvs zs zs where
  !n = U.length as
  !cvs = V.generate n (\i -> U.take i zs) -- upper triangular
  !zs = U.replicate n 0
{-# INLINE row #-}

-- | Generate an empty set of moments for a restricted number of dimensions
dim :: Int -> Moments
dim n = Moments 0 zs zs cvs zs zs where
  !cvs = V.generate n (\i -> U.take i zs) -- upper triangular
  !zs = U.replicate n 0

momentsOf :: Foldable f => Getting Moments s (f Double) -> s -> Moments
momentsOf l = foldMapOf l (row . U.fromList . F.toList)
{-# INLINE momentsOf #-}

-- | @'trimmed' n v@ will insert the vector @v@ into 'Moments'. If 'Moments' summarizes at least @n@ elements, this will start rejecting extreme components of the vector, replacing them with the @mean@.
trimmed :: Int64 -> U.Vector Double -> Moments -> Moments
trimmed k as r@(Moments n ms vs _ _ _)
  | n >= k = U.zipWith3 go as ms vs <| r where
  n' = fromIntegral n
  go a m v
    | tsd <- 3*sqrt (v/n'), a >= m + tsd || a <= m - tsd = m -- clamp instead?
    | otherwise                    = a
trimmed _ as r = as <| r

-- this lets us use 'cons' to add a moment to the mix.
instance (Bifunctor p, Profunctor p, Functor f) => Cons p f Moments Moments (U.Vector Double) (U.Vector Double) where
  -- TODO: Use Welford's algorithm directly (extended by Terriberry)
  _Cons = unto $ \(d,m) -> row d <> m
  {-# INLINE _Cons #-}

instance (Bifunctor p, Profunctor p, Functor f) => Snoc p f Moments Moments (U.Vector Double) (U.Vector Double) where
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
variances :: HasMoments t => IndexedTraversal' Int t Double
variances f = moments ago where
  ago NoMoments                   = pure NoMoments
  ago m@(Moments 0 _ _ _ _ _)     = pure m
  ago (Moments i ms vs cvs ss ks) = each (dividedBy (fromIntegral i) f) vs <&> \us -> Moments i ms us cvs ss ks
{-# INLINE variances #-}

variance :: HasMoments t => Int -> IndexedTraversal' Int t Double
variance k f = moments ago where
  ago NoMoments               = pure NoMoments
  ago m@(Moments 0 _ _ _ _ _) = pure m
  ago (Moments i ms vs cvs ss ks) = L.indexed f k ((vs U.! k) / fromIntegral i) <&> \v -> Moments i ms (vs U.// [(k,v*fromIntegral i)]) cvs ss ks
{-# INLINE variance #-}

-- | /NB:/ setting this will render the higher 'Moments' inconsistent.
stddevs :: HasMoments t => IndexedTraversal' Int t Double
stddevs = variances.root
{-# INLINE stddevs #-}

-- | Retrieve the nth std deviation from our set.
stddev :: HasMoments t => Int -> IndexedTraversal' Int t Double
stddev k = variance k . root
{-# INLINE stddev #-}

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

{-
-- | Calculate 'skewness'.
--  This is a 'Fold' because 'skewness' may not be defined for some combinations of 'Moments'.
skewness :: HasMoments t => Int -> Fold t Double
skewness f = moments go where
  go (Moments i _ bs cs _)
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

-}
