{-# LANGUAGE CPP, ForeignFunctionInterface, MagicHash, UnboxedTuples, BangPatterns #-}
module Data.Analytics.Bits
  ( Ranked(..)
  , log2
  , w32
  , w64
  ) where

import Data.Bits
import Data.Int
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import GHC.Base

-- TODO: generalize to 64 bits, etc.
log2 :: Word32 -> Int
log2 !n0 = fromIntegral $ go (shiftR (n5 * 0x7C4ACDD) 27) where
  go :: Word32 -> Word8
  go !i = inlinePerformIO $ peekElemOff debruijn_log32 (fromIntegral i)
  !n1 = n0 .|. shiftR n0 1
  !n2 = n1 .|. shiftR n1 2
  !n3 = n2 .|. shiftR n2 4
  !n4 = n3 .|. shiftR n3 8
  !n5 = n4 .|. shiftR n4 16
{-# INLINE log2 #-}

class (Num t, Bits t) => Ranked t where
  -- | Calculate the least significant set bit using a debruijn multiplication table.
  -- /NB:/ The result of this function is undefined when given 0.
  lsb :: t -> Int
  lsb n = rank n - 1

  -- | Calculate the number of trailing 0 bits.
  rank :: t -> Int
  rank 0 = 0
  rank n = lsb n + 1

instance Ranked Word64 where
  lsb n = fromIntegral $ go (shiftR ((n .&. (-n)) * 0x07EDD5E59A4E28C2) 58) where
    go :: Word64 -> Word8
    go i = inlinePerformIO $ peekElemOff debruijn_lsb64 (fromIntegral i)
  {-# INLINE lsb #-}

instance Ranked Word32 where
  lsb n = fromIntegral $ go (shiftR ((n .&. (-n)) * 0x077CB531) 27) where
    go :: Word32 -> Word8
    go i = inlinePerformIO $ peekElemOff debruijn_lsb32 (fromIntegral i)
  rank n = fromIntegral $ go (shiftR ((n .&. (-n)) * 0x4279976B) 26) where
    go :: Word32 -> Word8
    go i = inlinePerformIO $ peekElemOff debruijn_rank32 (fromIntegral i)
  {-# INLINE rank #-}

instance Ranked Word16 where
  lsb = lsb . w32
  {-# INLINE lsb #-}
  rank = lsb . w32
  {-# INLINE rank #-}

instance Ranked Word8 where
  lsb = lsb . w32
  {-# INLINE lsb #-}
  rank = lsb . w32
  {-# INLINE rank #-}

instance Ranked Int64 where
  lsb = lsb . w64
  {-# INLINE lsb #-}
  rank = lsb . w64
  {-# INLINE rank #-}

instance Ranked Int32 where
  lsb = lsb . w32
  {-# INLINE lsb #-}
  rank = lsb . w32
  {-# INLINE rank #-}

instance Ranked Int16 where
  lsb = lsb . w32
  {-# INLINE lsb #-}
  rank = lsb . w32
  {-# INLINE rank #-}

instance Ranked Int8 where
  lsb = lsb . w32
  {-# INLINE lsb #-}
  rank = lsb . w32
  {-# INLINE rank #-}

------------------------------------------------------------------------------
-- Util
------------------------------------------------------------------------------

w32 :: Integral a => a -> Word32
w32 = fromIntegral
{-# INLINE w32 #-}

w64 :: Integral a => a -> Word64
w64 = fromIntegral
{-# INLINE w64 #-}

------------------------------------------------------------------------------
-- de Bruijn Multiplication Tables
------------------------------------------------------------------------------

foreign import ccall "static &debruijn_lsb64"  debruijn_lsb64  :: Ptr Word8
foreign import ccall "static &debruijn_lsb32"  debruijn_lsb32  :: Ptr Word8
foreign import ccall "static &debruijn_rank32" debruijn_rank32 :: Ptr Word8
foreign import ccall "static &debruijn_log32"  debruijn_log32  :: Ptr Word8

#ifndef HLINT
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of
  (# _, r #) -> r
{-# INLINE inlinePerformIO #-}
#endif HLINT
