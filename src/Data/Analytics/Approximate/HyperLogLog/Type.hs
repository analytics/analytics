{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE PolyKinds                 #-}
#endif

--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Approximate.HyperLogLog.Type
  (
  -- * HyperLogLog
    HyperLogLog(..)
  , HasHyperLogLog(..)
  , size
  , intersectionSize
  , cast
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Analytics.Approximate.HyperLogLog.Config
import           Data.Analytics.Approximate.Type
import           Data.Analytics.Bits
import           Data.Bits
import           Data.Hashable
import           Data.Proxy
import           Data.Semigroup
import           Data.Serialize
import qualified Data.Vector.Unboxed                           as V
import qualified Data.Vector.Unboxed.Mutable                   as MV
import           Generics.Deriving                             hiding (D, to)
import           GHC.Int

------------------------------------------------------------------------------
-- HyperLogLog
------------------------------------------------------------------------------


-- | An approximate streaming (constant space) unique object counter.
--
-- See paper for details:
-- @http:\/\/algo.inria.fr\/flajolet\/Publications\/FlFuGaMe07.pdf@
--
-- Usage:
--
-- >>> :set -XTemplateHaskell
-- >>> import Control.Lens
-- >>> import Data.Analytics.Approximate.HyperLogLog
-- >>> import Data.Analytics.Reflection
-- >>> import Data.Monoid
--
-- Initialize a new counter:
--
-- >>> mempty :: HyperLogLog $(nat 3)
-- HyperLogLog {runHyperLogLog = fromList [0,0,0,0,0,0,0,0]}
--
-- Please note how you specify a counter size with the @$(nat n)@
-- invocation. Sizes of up to 16 are valid, with 7 being a
-- likely good minimum for decent accuracy.
--
-- Let's count a list of unique items and get the latest estimate:
--
-- >>> size $ foldr cons (mempty :: HyperLogLog $(nat 4)) [1..10]
-- Approximate {_confidence = 0.9972, _lo = 2, _estimate = 11, _hi = 20}
--
-- Note how 'cons' can be used to add new observations to the
-- approximate counter.
newtype HyperLogLog p = HyperLogLog { runHyperLogLog :: V.Vector Rank }
    deriving (Eq, Show, Generic)

instance Serialize (HyperLogLog p)

makeClassy ''HyperLogLog

_HyperLogLog :: Iso' (HyperLogLog p) (V.Vector Rank)
_HyperLogLog = iso runHyperLogLog HyperLogLog
{-# INLINE _HyperLogLog #-}

instance ReifiesConfig p => HasConfig (HyperLogLog p) where
  config = to reflectConfig
  {-# INLINE config #-}

instance Semigroup (HyperLogLog p) where
  HyperLogLog a <> HyperLogLog b = HyperLogLog (V.zipWith max a b)
  {-# INLINE (<>) #-}

-- The 'Monoid' instance \"should\" just work. Give me two estimators and I
-- can give you an estimator for the union set of the two.
instance ReifiesConfig p => Monoid (HyperLogLog p) where
  mempty = HyperLogLog $ V.replicate (reflectConfig (Proxy :: Proxy p) ^. numBuckets) 0
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

instance (Profunctor p, Bifunctor p, Functor f, ReifiesConfig s, Hashable a, s ~ t, a ~ b) => Cons p f (HyperLogLog s) (HyperLogLog t) a b where
  _Cons = unto go where
    go (a,m@(HyperLogLog v)) = HyperLogLog $ V.modify (\x -> do old <- MV.read x bk; when (rnk > old) $ MV.write x bk rnk) v where
      !h = w32 (hash a)
      !bk = calcBucket m h
      !rnk = calcRank m h
  {-# INLINE _Cons #-}

instance (Profunctor p, Bifunctor p, Functor f, ReifiesConfig s, Hashable a, s ~ t, a ~ b) => Snoc p f (HyperLogLog s) (HyperLogLog t) a b where
  _Snoc = unto go where
    go (m@(HyperLogLog v), a) = HyperLogLog $ V.modify (\x -> do old <- MV.read x bk; when (rnk > old) $ MV.write x bk rnk) v where
      !h = w32 (hash a)
      !bk = calcBucket m h
      !rnk = calcRank m h
  {-# INLINE _Snoc #-}

-- | Approximate size of our set
size :: ReifiesConfig p => HyperLogLog p -> Approximate Int64
size m@(HyperLogLog bs) = Approximate 0.9972 l expected h where
  m' = fromIntegral (m^.numBuckets)
  numZeros = fromIntegral . V.length . V.filter (== 0) $ bs
  res = case raw < m^.smallRange of
    True | numZeros > 0 -> m' * log (m' / numZeros)
         | otherwise -> raw
    False | raw <= m^.interRange -> raw
          | otherwise -> -1 * lim32 * log (1 - raw / lim32)
  raw = m^.rawFact * (1 / sm)
  sm = V.sum $ V.map (\x -> 1 / (2 ^^ x)) bs
  expected = round res
  sd = err (m^.numBits)
  err n = 1.04 / sqrt (fromInteger (bit n))
  l = floor $ max (res*(1-3*sd)) 0
  h = ceiling $ res*(1+3*sd)
{-# INLINE size #-}

intersectionSize :: ReifiesConfig p => [HyperLogLog p] -> Approximate Int64
intersectionSize [] = 0
intersectionSize (x:xs) = withMin 0 $ size x + intersectionSize xs - intersectionSize (mappend x <$> xs)
{-# INLINE intersectionSize #-}

cast :: forall p q. (ReifiesConfig p, ReifiesConfig q) => HyperLogLog p -> Maybe (HyperLogLog q)
cast old
  | newBuckets <= oldBuckets = Just $ over _HyperLogLog ?? mempty $ V.modify $ \m ->
    V.forM_ (V.indexed $ old^._HyperLogLog) $ \ (i,o) -> do
      let j = mod i newBuckets
      a <- MV.read m j
      MV.write m j (max o a)
  | otherwise = Nothing -- TODO?
  where
  newConfig = reflectConfig (Proxy :: Proxy q)
  newBuckets = newConfig^.numBuckets
  oldBuckets = old^.numBuckets
{-# INLINE cast #-}
