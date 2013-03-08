{-# LANGUAGE BangPatterns, EmptyDataDecls, ForeignFunctionInterface #-}

-- |
-- Module:      Codec.Compression.Snappy
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- This module provides fast, pure zero-copy compression and
-- decompression of lazy 'ByteString' data using the Snappy format.
--
-- Although these functions operate on lazy 'ByteString's, they
-- consume the data /strictly/: they do not produce any output until
-- they have consumed all of the input, and they produce the output in
-- a single large chunk.
--
-- If your data is already in the form of a lazy 'ByteString', it is
-- likely more efficient to use these functions than to convert your
-- data to and from strict ByteStrings, as you can avoid the
-- additional allocation and copying that would entail.

module Data.Analytics.Compression.Snappy.Lazy
    (
      compress
    , decompress
    ) where

#include "hs_snappy.h"

import Data.Analytics.Compression.Snappy.Internal (check, maxCompressedLength)
import Control.Exception (bracket)
import Data.ByteString.Internal hiding (ByteString)
import Data.ByteString.Lazy.Internal (ByteString(..))
import Data.Word (Word8, Word32)
import Foreign.C.Types (CInt(..), CSize(..))
import Foreign.ForeignPtr (touchForeignPtr, withForeignPtr)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (withArray)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, plusPtr)
import Foreign.Storable (Storable(..))
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

newtype BS = BS B.ByteString

data BSSource

instance Storable BS where
    sizeOf _    = (#size struct BS)
    alignment _ = alignment (undefined :: Ptr CInt)
    poke ptr (BS (PS fp off len)) = withForeignPtr fp $ \p -> do
      (#poke struct BS, ptr) ptr (p `plusPtr` off)
      (#poke struct BS, off) ptr (0::CSize)
      (#poke struct BS, len) ptr len
    {-# INLINE poke #-}

-- | Compress data into the Snappy format.
compress :: ByteString -> ByteString
compress bs = unsafePerformIO . withChunks bs $ \chunkPtr numChunks len -> do
  let dlen0 = maxCompressedLength len
  dfp <- mallocByteString dlen0
  withForeignPtr dfp $ \dptr -> do
    with (fromIntegral dlen0) $ \dlenPtr -> do
      c_CompressChunks chunkPtr (fromIntegral numChunks)
                       (fromIntegral len) dptr dlenPtr
      dlen <- fromIntegral `fmap` peek dlenPtr
      if dlen == 0
        then return Empty
        else return (Chunk (PS dfp 0 dlen) Empty)

-- | Decompress data in the Snappy format.
--
-- If the input is not compressed or is corrupt, an exception will be
-- thrown.
decompress :: ByteString -> ByteString
decompress bs = unsafePerformIO . withChunks bs $ \chunkPtr numChunks len ->
  bracket (c_NewSource chunkPtr (fromIntegral numChunks) (fromIntegral len))
          c_DeleteSource $ \srcPtr -> do
    alloca $ \dlenPtr -> do
      check "Lazy.decompress" $ c_GetUncompressedLengthChunks srcPtr dlenPtr
      dlen <- fromIntegral `fmap` peek dlenPtr
      if dlen == 0
        then return L.empty
        else do
          dfp <- mallocByteString dlen
          withForeignPtr dfp $ \dptr -> do
            check "Lazy.decompress" $ c_UncompressChunks srcPtr dptr
            return (Chunk (PS dfp 0 dlen) Empty)

withChunks :: ByteString -> (Ptr BS -> Int -> Int -> IO a) -> IO a
withChunks bs act = do
  let len = fromIntegral (L.length bs)
  let chunks = L.toChunks bs
  r <- withArray (map BS chunks) $ \chunkPtr ->
       act chunkPtr (length chunks) len
  foldr (\(PS fp _ _) _ -> touchForeignPtr fp) (return ()) chunks
  return r

foreign import ccall unsafe "hs_snappy.h _hsnappy_CompressChunks"
    c_CompressChunks :: Ptr BS -> CSize -> CSize -> Ptr Word8 -> Ptr CSize
                     -> IO ()

foreign import ccall unsafe "hs_snappy.h _hsnappy_NewSource"
    c_NewSource :: Ptr BS -> CSize -> CSize -> IO (Ptr BSSource)

foreign import ccall unsafe "hs_snappy.h _hsnappy_DeleteSource"
    c_DeleteSource :: Ptr BSSource -> IO ()

foreign import ccall unsafe "hs_snappy.h _hsnappy_UncompressChunks"
    c_UncompressChunks :: Ptr BSSource -> Ptr Word8 -> IO Int

foreign import ccall unsafe "hs_snappy.h _hsnappy_GetUncompressedLengthChunks"
    c_GetUncompressedLengthChunks :: Ptr BSSource -> Ptr Word32 -> IO Int
