module Data.Analytics.Bits
  ( reverseBits
  , reverseBytes
  , byteSum
  , byteCounts
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
  m3 = 0x0F0F0F0F0F0F0F0F

foreign import ccall reverseBytes :: Word64 -> Word64

-- | Calculate the total of the unsigned bytes of a 64-bit word
byteSum :: Word64 -> Int
byteSum a = fromIntegral $ shiftR (a * 0x0101010101010101) 56

-- | Convert a word of various bits into a word where each byte contains the count of bits in the corresponding original byte
--
-- @'popCount' = 'byteSum' . 'byteCounts'@
byteCounts :: Word64 -> Word64
byteCounts a = d where
  b = a - shiftR (a .&. (0xA * nybbles)) 1
  c = (b .&. (3 * nybbles)) + (shiftR b 2 .&. (3 * nybbles))
  d = (c + shiftR c 4) .&. (0x0f * bytes)
  nybbles = 0x1111111111111111
  bytes = 0x0101010101010101
