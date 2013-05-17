module Data.Analytics.Bits
  ( reverseBits
  , reverseBytes
  ) where

import Data.Bits
import Data.Word

reverseBits :: Word64 -> Word64
reverseBits a = reverseBytes d where
  b = (shiftR a 1 .&. m1) .|. shiftR (a .&. m1) 1
  c = (shiftR b 2 .&. m2) .|. shiftR (b .&. m2) 2
  d = (shiftR c 4 .&. m3) .|. shiftR (c .&. m3) 4
  m1 = 0x5555555555555555
  m2 = 0x3333333333333333
  m3 = 0x0F0F0F0F0F0F0F0F;

foreign import ccall reverseBytes :: Word64 -> Word64
