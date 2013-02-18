{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Counters.HyperLogLog.Internal where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Bits
import           Data.BloomFilter.Hash       (Hashable, hash32)
import qualified Data.ByteString             as B
import qualified Data.HashMap.Strict         as M
import           Data.List
import           Data.Monoid
import           Data.Serialize
import           Data.TypeLevel.Num.Aliases
import           Data.TypeLevel.Num.Reps
import           Data.TypeLevel.Num.Sets
import           Data.Vector.Serialize
import qualified Data.Vector.Unboxed         as V
import qualified Data.Vector.Unboxed.Mutable as MV
import           GHC.Generics
import           GHC.Int
import           GHC.Word


data HLL n = HLL {
      buckets :: V.Vector Rank
    , meta    :: Constants n
    } deriving (Eq, Show, Generic)


hllBits = bits . meta


hllNumBuckets = numBuckets . meta


instance Serialize (HLL n)

type Rank = Int8


-- | Convert HyperLogLog counter into a new length, trying to preserve
-- its estimation capabilities in the process.
castHLL :: forall n m. (Nat n, Nat m) => HLL n -> HLL m
castHLL old = new { buckets = newV }
    where
      (new :: HLL m) = initHLL
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
      targetIndexes i =
          if newlen <= oldlen
          then [i `mod` newlen]
          else error "castHLL: Can't upcast HLL to a higher accuracy yet."
          -- else map (\x -> i + oldlen * x) [0.. (fact - 1)]





-- | Monoid instance 'should' just work. Give me two estimators and I
-- can give you an estimator for the union set of the two.
instance Nat n => Monoid (HLL n) where
    mempty = (initHLL :: HLL n)
    mappend a b = HLL (V.zipWith max (buckets a) (buckets b)) (meta a)



-- | Initialize an HLL of given size 'n'
initHLL :: forall n. Nat n => HLL n
initHLL =
    let (cs :: Constants n) = mkConst
    in HLL (V.replicate (numBuckets cs) 0) cs



initHLL7 :: HLL D7
initHLL7 = initHLL

initHLL8 :: HLL D8
initHLL8 = initHLL

initHLL9 :: HLL D9
initHLL9 = initHLL

initHLL10 :: HLL D10
initHLL10 = initHLL

initHLL11 :: HLL D11
initHLL11 = initHLL

initHLL12 :: HLL D12
initHLL12 = initHLL

initHLL13 :: HLL D13
initHLL13 = initHLL

initHLL14 :: HLL D14
initHLL14 = initHLL

initHLL15 :: HLL D15
initHLL15 = initHLL

initHLL16 :: HLL D16
initHLL16 = initHLL



-- | update the counter with a new observation
updateHLL :: Hashable a => HLL n -> a -> HLL n
updateHLL (HLL !v cs) w = HLL v' cs
    where
      (!bk, !rnk) = bucketAndRank cs $ hash32 w -- asWord32 $ hash32 w
      modFn x = do
        old <- MV.read x bk
        when (rnk > old) $ MV.write x bk rnk
      !v' = V.modify modFn v


-- | Estimate the cardinality of the dataset so far
cardinality :: HLL n -> Integer
cardinality (HLL bs Constants{..}) = round res
    where
      m' = fromIntegral numBuckets
      numZeros = fromIntegral . V.length . V.filter (== 0) $ bs
      res = case raw < smallRange of
              True ->
                  if numZeros > 0
                  then m' * log (m' / numZeros)
                  else raw
              False ->
                case raw <= interRange of
                  True -> raw
                  False -> -1 * lim32 * log (1 - raw / lim32)

      raw = rawfact * (1 / sm)
      sm = V.sum $ V.map (\x -> 1 / (2 ^ x)) bs


-- | Constants required for a bucketing factor b
data Constants n = Constants {
      bits       :: {-# UNPACK #-} !Int
    , numBuckets :: {-# UNPACK #-} !Int
    , smallRange :: {-# UNPACK #-} !Double
    , interRange :: {-# UNPACK #-} !Double
    , rawfact    :: {-# UNPACK #-} !Double
    , alpha      :: {-# UNPACK #-} !Double
    , bucketMask :: {-# UNPACK #-} !Word32
    } deriving (Eq, Show, Generic)

instance Serialize (Constants n)

-- | Precalculate constants for a given bucketing factor b
mkConst :: forall n. Nat n => Constants n
mkConst = Constants {
               bits = b'
             , numBuckets = m'
             , smallRange = 5/2 * m''
             , interRange = (2 ^ 32) / 30
             , rawfact = a * (m'' ^ 2)
             , alpha = a
             , bucketMask = (2 ^ b') - 1
             }
    where
      b' = toInt (undefined :: n)
      m' = 2 ^ b'
      m'' = fromIntegral m'
      a = 0.7213 / (1 + 1.079 / m'')


-- | produce a 32 bit
-- hash32 :: [Word8] -> Word32
-- hash32 = fromInteger . (bitmask32 .&.) . Sha.toInteger . Sha.hash

alpha16 = 0.673
alpha32 = 0.697
alpha64 = 0.709


getBucket :: Word32 -> Word32 -> Int
getBucket mask i = fromIntegral $ i .&. mask
{-# INLINE getBucket #-}

getRest :: Int -> Word32 -> Word32
getRest b i = i `shiftR` b
{-# INLINE getRest #-}

getRank :: Word32 -> Int8
getRank i = fromIntegral $ maybe 0 (+1) $ findIndex (> 0) $ map (.&. i) rankMasks
{-# INLINE getRank #-}

-- | Try these masks in order; index of first one to return 1 is a go.
rankMasks :: [Word32]
rankMasks = map (\x -> 2^x -1) [1..32]


bucketAndRank :: Constants n -> Word32 -> (Int, Int8)
bucketAndRank Constants{..} x = (bk, rank)
    where
      bk = getBucket bucketMask x
      rank = getRank $ getRest bits x
{-# INLINE bucketAndRank #-}


lim32 :: Double
lim32 = 2 ^ 32

bitmask32 :: Num a => a
bitmask32 = 2 ^ 32 - 1
