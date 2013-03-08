{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE GADTs #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Morton.Schedule
  ( Schedule(Schedule)
  , HasSchedule(..)
  , Scheduled(..)
  , magic
  , integral
  , bits
  , hashed
  -- * Character schedules
  , ascii
  , iso8859_1
  ) where

import Control.Lens
import Data.Bits
import Data.Hashable
import Data.Int
import Data.Word
import Foreign.Storable

-- | This is used to schedule how we interleave our keys.
data Schedule a = Schedule
  { _schedulePriority    :: {-# UNPACK #-} !Int
  , _scheduleStride      :: {-# UNPACK #-} !Int
  , _scheduleBits        :: {-# UNPACK #-} !Int
  , _scheduleOrdered     :: !Bool
  , _scheduleApproximate :: !Bool
  , _scheduleEncoder     :: a -> Int -> Bool
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
{-# INLINE magic #-}

integral :: forall a. (Storable a, Bits a) => Schedule a
integral = bits (sizeOf (undefined :: a) * 8)
{-# INLINE integral #-}

bits :: Bits a => Int -> Schedule a
bits n = Schedule 0 (if n <= 1 then 0 else div magic (n - 1)) n True False testBit
{-# INLINE bits #-}

-- We conservatively approximate characters above of this range by the upper bound to preserve ordering.
ascii :: Schedule Char
ascii = contramap (clamp 127 . fromEnum) (bits 7)
{-# INLINE ascii #-}

clamp :: Int -> Int -> Int
clamp n k
  | k >= n = n
  | otherwise = k
{-# INLINE clamp #-}

-- | We conservatively approximate characters above of this range by the upper bound to preserve ordering.
iso8859_1 :: Schedule Char
iso8859_1 = contramap (clamp 255 . fromEnum) (bits 8)
{-# INLINE iso8859_1 #-}

hashed :: Hashable a => Schedule a
hashed = contramap hash integral
  & scheduleOrdered .~ False
  & scheduleApproximate .~ True
{-# INLINE hashed #-}

-- | Retrieve the default key schedule for a type
class Scheduled a where
  scheduled :: Schedule a
#ifndef HLINT
  default scheduled :: (Storable a, Bits a, Integral a) => Schedule a
  scheduled = integral
#endif

instance Scheduled Int where
  scheduled = integral
  {-# INLINE scheduled #-}

instance Scheduled Int8 where
  scheduled = integral
  {-# INLINE scheduled #-}

instance Scheduled Int16 where
  scheduled = integral
  {-# INLINE scheduled #-}

instance Scheduled Int32 where
  scheduled = integral
  {-# INLINE scheduled #-}

instance Scheduled Int64 where
  scheduled = bits 64
  {-# INLINE scheduled #-}

instance Scheduled Word where
  scheduled = integral
  {-# INLINE scheduled #-}

instance Scheduled Word8 where
  scheduled = integral
  {-# INLINE scheduled #-}

instance Scheduled Word16 where
  scheduled = integral
  {-# INLINE scheduled #-}

instance Scheduled Word32 where
  scheduled = integral
  {-# INLINE scheduled #-}

instance Scheduled Word64 where
  scheduled = integral
  {-# INLINE scheduled #-}

-- | You can also use 'ascii' and 'iso8859_1'
instance Scheduled Char where
  scheduled = contramap fromEnum (bits 21)
  {-# INLINE scheduled #-}

-- TODO: enumerated :: (Enum a, Bounded a) => Schedule a
