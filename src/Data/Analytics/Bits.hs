{-# LANGUAGE CPP, ForeignFunctionInterface, MagicHash, UnboxedTuples #-}
module Data.Analytics.Bits
  ( lsb
  , rank
  ) where

import Data.Bits
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import GHC.Base

foreign import ccall "static &debruijn_lut" debruijn_lut :: Ptr Word8

#ifndef HLINT
inlinePerformIO :: IO a -> a
inlinePerformIO (IO m) = case m realWorld# of
  (# _, r #) -> r
{-# INLINE inlinePerformIO #-}
#endif HLINT

debruijn :: Word64 -> Word8
debruijn i = inlinePerformIO (peekElemOff debruijn_lut (fromIntegral i))
{-# INLINE debruijn #-}

-- | Calculate the least significant set bit using a debruijn multiplication table
lsb :: Word64 -> Int
lsb n = fromIntegral $ debruijn (shiftR ((n .&. (-n)) * 0x07EDD5E59A4E28C2) 58)
{-# INLINE lsb #-}

-- | Calculate the 1 + the index of the least significant bit, returning 0 if no bit is set
rank :: Word64 -> Int
rank 0 = 0
rank n = lsb n + 1
{-# INLINE rank #-}
