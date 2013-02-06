{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Data.Analytics.Storage.Schedule
  ( Schedule(..)
  , HasSchedule(..)
  , Scheduled(..)
  , magic
  , integral
  , bits
  , hashed
  ) where

import Control.Lens
import Data.Hashable
import Data.Int
import Data.Word
import Foreign.Storable

-- This is used to schedule how we interleave our keys.

data Schedule a = Schedule
  { _schedulePriority    :: {-# UNPACK #-} !Int
  , _scheduleStride      :: {-# UNPACK #-} !Int
  , _scheduleBits        :: {-# UNPACK #-} !Int
  , _scheduleOrdered     :: Bool
  , _scheduleApproximate :: Bool
  , _scheduleEncoder     :: a -> Int64
  }

makeClassy ''Schedule

instance Contravariant Schedule where
  contramap f (Schedule p s c o a e) = Schedule p s c o a (e . f)
  {-# INLINE contramap #-}

-- |
-- >>> foldr lcm 2 [3,4,5,6,7,8,9,10,11,12,13,14,15,20,23,31,63]
-- 256936680
magic :: Int
magic = 256936680

integral :: forall a. (Storable a, Integral a) => Schedule a
integral = contramap fromIntegral $ bits (sizeOf (undefined :: a))

bits :: Int -> Schedule Int64
bits n = Schedule 0 (div magic (n - 1)) n True False id

hashed :: Hashable a => Schedule a
hashed = contramap hash integral
  & scheduleOrdered .~ False
  & scheduleApproximate .~ True

-- | Retrieve the default key schedule for a type
class Scheduled a where
  scheduled :: Schedule a

instance Scheduled Int where scheduled = integral
instance Scheduled Int8 where scheduled = integral
instance Scheduled Int16 where scheduled = integral
instance Scheduled Int32 where scheduled = integral
instance Scheduled Int64 where scheduled = bits 64
instance Scheduled Word where scheduled = integral
instance Scheduled Word8 where scheduled = integral
instance Scheduled Word16 where scheduled = integral
instance Scheduled Word32 where scheduled = integral
instance Scheduled Word64 where scheduled = integral
instance Scheduled Char where
  scheduled = contramap (fromIntegral . fromEnum) (bits 21)

-- TODO: enumerated :: (Enum a, Bounded a) => Schedule a
