{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Analytics.Compression.Bits
  ( GetBits(..), getAligned, getBit
  , Buildable(..)
  , BitBuilder(..)
  , PutBits(..)
  ) where

import Control.Applicative
import Control.Comonad
import Control.Lens
import Control.Monad
import Data.Binary.Builder
import Data.Binary.Get
import Data.Binary.Put
import Data.Bits
import Data.Bits.Lens
import Data.Foldable
import Data.Functor.Bind
import Data.Semigroup
import Data.Typeable
import Data.Word

------------------------------------------------------------------------------
-- GetBits
------------------------------------------------------------------------------

newtype GetBits a = GetBits
  { runGetBits :: forall r. (a -> Int -> Word8 -> Get r) -> Int -> Word8 -> Get r
  } deriving Typeable

instance Functor GetBits where
  fmap f (GetBits m) = GetBits $ \ k -> m (k . f)
  {-# INLINE fmap #-}

instance Applicative GetBits where
  pure a = GetBits $ \k -> k a
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad GetBits where
  return a = GetBits $ \ k -> k a
  {-# INLINE return #-}
  GetBits m >>= f = GetBits $ \ k -> m $ \a -> runGetBits (f a) k
  {-# INLINE (>>=) #-}

-- | 'Get' something from byte-aligned storage, starting on the next byte.
getAligned :: Get a -> GetBits a
getAligned m = GetBits $ \k _ _ -> m >>= \ a -> k a 0 0
{-# INLINE getAligned #-}

getBit :: GetBits Bool
getBit = GetBits $ \ k i b ->
  if i == 0
  then getWord8 >>= \b' -> ((k $! testBit b' 7) $! 7) $! shiftR b' 1
  else ((k $! testBit b 7) $! i - 1) $! shiftR b 1
{-# INLINE getBit #-}

------------------------------------------------------------------------------
-- BitBuilder
------------------------------------------------------------------------------

#ifndef HLINT
newtype BitBuilder = BitBuilder { runBitBuilder :: Int -> Word8 -> (# Int, Word8, Builder #) }
  deriving Typeable
#endif

instance Semigroup BitBuilder where
  (<>) = mappend
  {-# INLINE (<>) #-}

instance Monoid BitBuilder where
#ifndef HLINT
  mempty = BitBuilder $ \i b -> (# i, b, mempty #)
  {-# INLINE mempty #-}
  BitBuilder x `mappend` BitBuilder y = BitBuilder $ \i b -> case x i b of
    (# j, c, m  #) -> case y j c of
      (# k, d, n #) -> (# k, d, mappend m n #)
  {-# INLINE mappend #-}
#endif

------------------------------------------------------------------------------
-- PutBits
------------------------------------------------------------------------------

data PutBits a = PutBits !BitBuilder a
  deriving Typeable

instance Functor PutBits where
  fmap f (PutBits m a) = PutBits m (f a)
  {-# INLINE fmap #-}

instance Applicative PutBits where
  pure = PutBits mempty
  {-# INLINE pure #-}
  PutBits m f <*> PutBits n a = PutBits (m <> n) (f a)
  {-# INLINE (<*>) #-}

instance Monad PutBits where
  return = PutBits mempty
  {-# INLINE return #-}
  PutBits m a >>= f = case f a of
    PutBits n b -> PutBits (m <> n) b
  {-# INLINE (>>=) #-}

instance Foldable PutBits where
  foldMap f (PutBits _ a) = f a
  {-# INLINE foldMap #-}

instance Traversable PutBits where
  traverse f (PutBits m a) = PutBits m <$> f a
  {-# INLINE traverse #-}

instance Comonad PutBits where
  extract (PutBits _ a) = a
  {-# INLINE extract #-}
  duplicate w@(PutBits m _) = PutBits m w
  {-# INLINE duplicate #-}

instance ComonadApply PutBits where
  PutBits m f <@> PutBits n a = PutBits (m <> n) (f a)
  {-# INLINE (<@>) #-}

instance Apply PutBits where
  PutBits m f <.> PutBits n a = PutBits (m <> n) (f a)
  {-# INLINE (<.>) #-}

instance Bind PutBits where
  PutBits m a >>- f = case f a of
    PutBits n b -> PutBits (m <> n) b
  {-# INLINE (>>-) #-}

------------------------------------------------------------------------------
-- Buildable
------------------------------------------------------------------------------

class Buildable t where
  flushBits :: t
  flushBits = bitBuilder flushBits
  {-# INLINE flushBits #-}

  builder :: Builder -> t
  builder = bitBuilder . builder
  {-# INLINE builder #-}

  putBit :: Bool -> t
  putBit = bitBuilder . putBit
  {-# INLINE putBit #-}

  putAligned :: PutM a -> t
  putAligned = builder . execPut
  {-# INLINE putAligned #-}

  bitBuilder :: BitBuilder -> t

instance Buildable BitBuilder where
#ifndef HLINT
  flushBits = BitBuilder $ \i b ->
    if i == 0
    then (# 0, 0, mempty #)
    else (# 0, 0, singleton b #)
  {-# INLINE flushBits #-}

  builder m = BitBuilder $ \i b ->
    if i == 0
    then (# 0, 0, m #)
    else let !r = singleton b `mappend` m in (# 0, 0, r #)
  {-# INLINE builder #-}

  putBit v = BitBuilder $ \i b ->
    if i == 7
    then let !bb = singleton (shiftL b 1 & bitAt 0 .~ v)
         in (# 0, 0, bb #)
    else let !i' = i + 1
             !b' = shiftL b 1 & bitAt 0 .~ v
         in (# i', b', mempty #)
  {-# INLINE putBit #-}
#endif

  bitBuilder = id
  {-# INLINE bitBuilder #-}

instance a ~ () => Buildable (PutBits a) where
  bitBuilder b = PutBits b ()
  {-# INLINE bitBuilder #-}
