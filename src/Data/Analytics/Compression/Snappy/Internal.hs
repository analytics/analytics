{-# LANGUAGE ForeignFunctionInterface #-}

-- |
-- Module:      Codec.Compression.Snappy
-- Copyright:   (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- This module provides fast, pure Haskell bindings to Google's
-- Snappy compression and decompression library:
-- <http://code.google.com/p/snappy/>
--
-- These functions operate on strict bytestrings, and thus use as much
-- memory as both the entire compressed and uncompressed data.

module Data.Analytics.Compression.Snappy.Internal
    (
      check
    , maxCompressedLength
    ) where

import Control.Monad (when)
import Foreign.C.Types (CSize(..))

maxCompressedLength :: Int -> Int
maxCompressedLength = fromIntegral . c_MaxCompressedLength . fromIntegral
{-# INLINE maxCompressedLength #-}

check :: (Integral a) => String -> IO a -> IO ()
check func act = do
  ok <- act
  when (ok == 0) . fail $ "Codec.Compression.Snappy." ++ func ++
                          ": corrupt input "
{-# INLINE check #-}

foreign import ccall unsafe "hs_snappy.h _hsnappy_MaxCompressedLength"
    c_MaxCompressedLength :: CSize -> CSize
