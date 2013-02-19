{-# LANGUAGE CPP #-}
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

#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#define USE_TYPE_LITS 1
#endif

{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Data.Analytics.Approximate.Set.HyperLogLog.Internal
  (
  -- * HyperLogLog
    HyperLogLog(..)
  , HasHyperLogLog(..)
  , numBits, numBuckets, smallRange, interRange, rawFact, alpha, bucketMask
  , size
  , intersectionSize
  -- * Config
  , Config(..)
  , HasConfig(..)
  , hll
  -- * ReifiesConfig
  , ReifiesConfig(..)
  , reifyConfig
  ) where


import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Analytics.Approximate.Type
import Data.Analytics.Bits
import Data.Analytics.Reflection
import Data.Bits
import Data.Hashable
import Data.Proxy
import Data.Reflection
import Data.Semigroup
import Data.Serialize
import Data.Vector.Serialize ()
import Data.Vector.Unboxed as V
import Data.Vector.Unboxed.Mutable as MV
import GHC.Int
import GHC.Word
import Generics.Deriving hiding (to, D)
#ifdef USE_TYPELITS
import GHC.TypeLits
#endif

type Rank = Int8

------------------------------------------------------------------------------
-- Config
------------------------------------------------------------------------------

-- | Constants required for a bucketing factor b
data Config = Config
  { _numBits    :: {-# UNPACK #-} !Int
  , _numBuckets :: {-# UNPACK #-} !Int
  , _smallRange :: {-# UNPACK #-} !Double
  , _interRange :: {-# UNPACK #-} !Double
  , _rawFact    :: {-# UNPACK #-} !Double
  , _alpha      :: {-# UNPACK #-} !Double
  , _bucketMask :: {-# UNPACK #-} !Word32
  } deriving (Eq, Show, Generic)

class HasConfig t where
  config :: Getter t Config

makeLensesWith ?? ''Config $ classyRules
  & generateSignatures .~ False
  & createClass        .~ False
  & createInstance     .~ False

instance HasConfig Config where
  config = id
  {-# INLINE config #-}

instance Serialize Config -- serialize as a number?

-- | Precalculate constants for a given bucketing factor b
hll :: Int -> Config
hll b = Config
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
{-# INLINE hll #-}

------------------------------------------------------------------------------
-- ReifiesConfig
------------------------------------------------------------------------------

class ReifiesConfig o where
  reflectConfig :: p o -> Config

#ifdef USE_TYPE_LITS
instance SingRep n Integer => ReifiesConfig (n :: Nat) where
  reflectConfig = hll $ fromInteger $ withSing $ \(x :: Sing n) -> fromSing x
  {-# INLINE reflectConfig #-}
#endif

data ReifiedConfig (s :: *)

retagReifiedConfig :: (Proxy s -> a) -> proxy (ReifiedConfig s) -> a
retagReifiedConfig f _ = f Proxy
{-# INLINE retagReifiedConfig #-}

instance Reifies s Config => ReifiesConfig (ReifiedConfig s) where
  reflectConfig = retagReifiedConfig reflect
  {-# INLINE reflectConfig #-}

reifyConfig :: Int -> (forall o. ReifiesConfig o => Proxy o -> r) -> r
reifyConfig i f = reify (hll i) (go f) where
  go :: Reifies o Config => (Proxy (ReifiedConfig o) -> a) -> proxy o -> a
  go g _ = g Proxy
{-# INLINE reifyConfig #-}

instance Reifies n Int => ReifiesConfig (D n) where
  reflectConfig = reflect <&> \n -> hll (n+n)
  {-# INLINE reflectConfig #-}

-- this way we only get instances for positive natural numbers
instance Reifies n Int => ReifiesConfig (SD n) where
  reflectConfig = reflect <&> \n -> hll (n+n+1)
  {-# INLINE reflectConfig #-}

{-
-- By disabling these it becomes a compile time error to try to generate a config for a non-positive number with $(int n)

instance ReifiesConfig Z where
  reflectConfig _ = hll 0
  {-# INLINE reflectConfig #-}

instance Reifies n Int => ReifiesConfig (PD n) where
  reflectConfig = reflect <&> \n -> hll (n+n-1)
  {-# INLINE reflectConfig #-}
-}

------------------------------------------------------------------------------
-- Util
------------------------------------------------------------------------------

calcBucket :: HasConfig t => t -> Word32 -> Int
calcBucket t w = fromIntegral (w .&. t^.bucketMask)
{-# INLINE calcBucket #-}

calcRank :: HasConfig t => t -> Word32 -> Int8
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

instance ReifiesConfig p => HasConfig (HyperLogLog p) where
  config = to reflectConfig
  {-# INLINE config #-}

instance Semigroup (HyperLogLog p) where
  HyperLogLog a <> HyperLogLog b = HyperLogLog (V.zipWith max a b)
  {-# INLINE (<>) #-}

-- | Monoid instance 'should' just work. Give me two estimators and I
-- can give you an estimator for the union set of the two.
instance ReifiesConfig p => Monoid (HyperLogLog p) where
  mempty = HyperLogLog $ V.replicate (reflectConfig (undefined :: [p]) ^. numBuckets) 0
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
  sm = V.sum $ V.map (\x -> 1 / (2 ^ x)) bs
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
