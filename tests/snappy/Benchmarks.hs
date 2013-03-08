{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.DeepSeq (NFData(..))
import Control.Exception (finally)
import Criterion.Main
import Functions (rechunk)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (hClose, openBinaryTempFile)
import qualified Codec.Compression.Snappy as S
import qualified Codec.Compression.Snappy.Lazy as L
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L

strictCompressFile name = do
  bs <- S.readFile name
  return $! S.compress bs

lazyCompressFile name = do
  bs <- L.readFile name
  return $! rnf (L.compress bs)

strictDecompressFile name = do
  bs <- S.readFile name
  return $! S.decompress bs

lazyDecompressFile name = do
  bs <- L.readFile name
  return $! rnf (L.decompress bs)

instance NFData L.ByteString where
    rnf (L.Chunk _ cs) = rnf cs
    rnf _              = ()
    {-# INLINE rnf #-}

main = do
  let rawName = "test-data/huge.json"
  tmpDir <- getTemporaryDirectory
  (compName, h) <- openBinaryTempFile tmpDir "compressed"
  sraw <- S.readFile rawName
  let scomp = S.compress sraw
      lraw = rechunk 16384 sraw
      lcomp = rechunk 16384 scomp
  S.hPut h scomp
  hClose h
  flip finally (removeFile compName) $ defaultMain [
      bgroup "file" [
        bgroup "compress" [
          bench "strict" $ strictCompressFile rawName
        , bench "lazy" $ lazyCompressFile rawName
        ]
      , bgroup "decompress" [
          bench "strict" $ strictDecompressFile compName
        , bench "lazy" $ lazyDecompressFile compName
        ]
      ]
    , bgroup "pure" [
        bgroup "compress" [
          bench "strict" $ whnf S.compress sraw
        , bench "lazy" $ nf L.compress lraw
        ]
      , bgroup "decompress" [
          bench "strict" $ whnf S.decompress scomp
        , bench "lazy" $ nf L.decompress lcomp
        ]
      ]
    ]
