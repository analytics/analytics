{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Analytics.Approximate.HyperLogLog
  ( HyperLogLog(..)
  , HasHyperLogLog(..)
  , numBits, numBuckets, smallRange, interRange, rawFact, alpha, bucketMask
  , size
  -- * Config
  , HyperLogLogConfig(..)
  , HasHyperLogLogConfig(..)
  , reifyHyperLogLogConfig
  , config
  -- * Testing
  , HLL10
  ) where

import Control.Lens
import Control.Monad
import Data.Analytics.Bits
import Data.Bits
import Data.Hashable
import Data.Proxy
import Data.Reflection
import Data.Semigroup
import Data.Serialize
import Data.Vector.Serialize ()
import Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable as MV
import Generics.Deriving hiding (to)
import GHC.Int
import GHC.Word

type Rank = Int8

------------------------------------------------------------------------------
-- HyperLogLogConfig
------------------------------------------------------------------------------

-- | Constants required for a bucketing factor b
data HyperLogLogConfig = HyperLogLogConfig
  { _numBits    :: {-# UNPACK #-} !Int
  , _numBuckets :: {-# UNPACK #-} !Int
  , _smallRange :: {-# UNPACK #-} !Double
  , _interRange :: {-# UNPACK #-} !Double
  , _rawFact    :: {-# UNPACK #-} !Double
  , _alpha      :: {-# UNPACK #-} !Double
  , _bucketMask :: {-# UNPACK #-} !Word32
  } deriving (Eq, Show, Generic)

class HasHyperLogLogConfig t where
  hyperLogLogConfig :: Getter t HyperLogLogConfig

makeLensesWith ?? ''HyperLogLogConfig $ classyRules
  & generateSignatures .~ False
  & createClass .~ False
  & createInstance .~ False

instance HasHyperLogLogConfig HyperLogLogConfig where
  hyperLogLogConfig = id
  {-# INLINE hyperLogLogConfig #-}

instance Serialize HyperLogLogConfig -- serialize as a number?

-- | Precalculate constants for a given bucketing factor b
config :: Int -> HyperLogLogConfig
config b = HyperLogLogConfig
  { _numBits = b
  , _numBuckets = m
  , _smallRange = 5/2 * m'
  , _interRange = lim32 / 30
  , _rawFact = a * m' * m'
  , _alpha = a
  , _bucketMask = bit b - 1
  } where
  m = bit b
  m' = fromIntegral m
  a = 0.7213 / (1 + 1.079 / m')
{-# INLINE config #-}

reifyHyperLogLogConfig :: Int -> (forall s. Reifies s HyperLogLogConfig => Proxy s -> r) -> r
reifyHyperLogLogConfig i f = reify (config i) f
{-# INLINE reifyHyperLogLogConfig #-}

------------------------------------------------------------------------------
-- Util
------------------------------------------------------------------------------

calcBucket :: HasHyperLogLogConfig t => t -> Word32 -> Int
calcBucket t w = fromIntegral (w .&. t^.bucketMask)
{-# INLINE calcBucket #-}

calcRank :: HasHyperLogLogConfig t => t -> Word32 -> Int8
calcRank t w = fromIntegral $ rank $ shiftR w $ t^.numBits
{-# INLINE calcRank #-}

lim32 :: Double
lim32 = fromInteger (bit 32)
{-# INLINE lim32 #-}

------------------------------------------------------------------------------
-- HyperLogLog
------------------------------------------------------------------------------

newtype HyperLogLog p = HyperLogLog { runHyperLogLog :: Vector Rank }  deriving (Eq, Show, Generic)

instance Serialize (HyperLogLog p)

makeClassy ''HyperLogLog

instance Reifies p HyperLogLogConfig => HasHyperLogLogConfig (HyperLogLog p) where
  hyperLogLogConfig = to reflect
  {-# INLINE hyperLogLogConfig #-}

instance Semigroup (HyperLogLog p) where
  HyperLogLog a <> HyperLogLog b = HyperLogLog (V.zipWith max a b)
  {-# INLINE (<>) #-}

-- | Monoid instance 'should' just work. Give me two estimators and I
-- can give you an estimator for the union set of the two.
instance Reifies p HyperLogLogConfig => Monoid (HyperLogLog p) where
  mempty = HyperLogLog $ V.replicate (reflect (undefined :: [p]) ^. numBuckets) 0
  {-# INLINE mempty #-}
  mappend = (<>)
  {-# INLINE mappend #-}

instance (Profunctor p, Bifunctor p, Functor f, Reifies s HyperLogLogConfig, Hashable a, s ~ t, a ~ b) => Cons p f (HyperLogLog s) (HyperLogLog t) a b where
  _Cons = unto go where
    go (a,m@(HyperLogLog v)) = HyperLogLog $ V.modify (\x -> do old <- MV.read x bk; when (rnk > old) $ MV.write x bk rnk) v where
      !h = w32 (hash a)
      !bk = calcBucket m h
      !rnk = calcRank m h
  {-# INLINE _Cons #-}

instance (Profunctor p, Bifunctor p, Functor f, Reifies s HyperLogLogConfig, Hashable a, s ~ t, a ~ b) => Snoc p f (HyperLogLog s) (HyperLogLog t) a b where
  _Snoc = unto go where
    go (m@(HyperLogLog v), a) = HyperLogLog $ V.modify (\x -> do old <- MV.read x bk; when (rnk > old) $ MV.write x bk rnk) v where
      !h = w32 (hash a)
      !bk = calcBucket m h
      !rnk = calcRank m h
  {-# INLINE _Snoc #-}

-- | Estimate the size of the dataset so far
--
-- TODO: use Approximate Integer
size :: Reifies p HyperLogLogConfig => HyperLogLog p -> Integer
size m@(HyperLogLog bs) = round res where
  m' = fromIntegral (m^.numBuckets)
  numZeros = fromIntegral . V.length . V.filter (== 0) $ bs
  res = case raw < m^.smallRange of
    True | numZeros > 0 -> m' * log (m' / numZeros)
         | otherwise -> raw
    False | raw <= m^.interRange -> raw
          | otherwise -> -1 * lim32 * log (1 - raw / lim32)
  raw = m^.rawFact * (1 / sm)
  sm = V.sum $ V.map (\x -> 1 / (2 ^ x)) bs
{-# INLINE size #-}

data HLL10

instance Reifies HLL10 HyperLogLogConfig where
  reflect _ = config 10

{-
hllBits = bits . meta
hllNumBuckets = numBuckets . meta

-- | Convert HyperLogLog counter into a new length, trying to preserve
-- its estimation capabilities in the process.
castHyperLogLog :: forall n m. (Reifies n Constants, Reifies m Constants) => HyperLogLog n -> HyperLogLog m
castHyperLogLog old = new { buckets = newV } where
 (new :: HyperLogLog m) = initHyperLogLog
 oldlen = numBuckets $ meta old
 newlen = numBuckets $ meta new
 fact = newlen `div` oldlen

 newV = V.modify update $ buckets new
 oldV = V.toList $ V.indexed (buckets old)

      -- | take each index of the old vector, map it to new vector.
 update m = forM_ oldV $ \ (i, oval) -> do
   let upWith n rank = do
       curval <- MV.read m n
       MV.write m n (max rank curval)

   let (h:rest) = targetIndexes i

   upWith h oval
   forM_ rest $ \ n -> upWith n (oval-1)

 -- | each index in the old vector maps to 1 or more indexes in
 -- the new vector.
 targetIndexes i
   | newlen <= oldlen = [i `mod` newlen]
   | otherwise = error "castHyperLogLog: Can't upcast HyperLogLog to a higher accuracy yet."
   -- else map (\x -> i + oldlen * x) [0.. (fact - 1)]
-}
