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
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Approximate.HyperLogLog.Config
  (
  -- * Config
    Config
  , HasConfig(..)
  , hll
  , numBits, numBuckets, smallRange, interRange, rawFact, alpha, bucketMask
  -- * ReifiesConfig
  , ReifiesConfig(..)
  , reifyConfig
  -- * Rank
  , Rank
  , calcBucket
  , calcRank
  , lim32
  ) where

import Control.Lens
import Data.Analytics.Reflection
import Data.Bits
import Data.Bits.Extras
import Data.Proxy
import Data.Reflection
import Data.Serialize
import Data.Vector.Serialize ()
import GHC.Int
import GHC.Word
import Generics.Deriving hiding (to, D)
#ifdef USE_TYPE_LITS
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
  reflectConfig _ = hll $ fromInteger $ withSing $ \(x :: Sing n) -> fromSing x
  {-# INLINE reflectConfig #-}
#endif

data ReifiedConfig (s :: *)

retagReifiedConfig :: (Proxy s -> a) -> proxy (ReifiedConfig s) -> a
retagReifiedConfig f _ = f Proxy
{-# INLINE retagReifiedConfig #-}

instance Reifies s Config => ReifiesConfig (ReifiedConfig s) where
  reflectConfig = retagReifiedConfig reflect
  {-# INLINE reflectConfig #-}

reifyConfig :: Int -> (forall (o :: *). ReifiesConfig o => Proxy o -> r) -> r
reifyConfig i f = reify (hll i) (go f) where
  go :: Reifies o Config => (Proxy (ReifiedConfig o) -> a) -> proxy o -> a
  go g _ = g Proxy

{-# INLINE reifyConfig #-}

instance Reifies n Int => ReifiesConfig (D n) where
  reflectConfig = hll . reflect
  {-# INLINE reflectConfig #-}

-- this way we only get instances for positive natural numbers
instance Reifies n Int => ReifiesConfig (SD n) where
  reflectConfig = hll . reflect
  {-# INLINE reflectConfig #-}

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
