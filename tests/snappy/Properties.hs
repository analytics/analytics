{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

import Control.Applicative ((<$>), (<*>))
import Functions (rechunk)
import Test.Framework (defaultMain)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (Arbitrary(..))
import qualified Codec.Compression.Snappy as B
import qualified Codec.Compression.Snappy.Lazy as L
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

instance Arbitrary B.ByteString where
    arbitrary = B.pack <$> arbitrary

instance Arbitrary L.ByteString where
    arbitrary = smallChunk <$> arbitrary <*> arbitrary

s_roundtrip bs = B.decompress (B.compress bs) == bs

newtype Compressed a = Compressed { compressed :: a }
    deriving (Eq, Ord)

instance Show a => Show (Compressed a)
    where show (Compressed a) = "Compressed " ++ show a

instance Arbitrary (Compressed B.ByteString) where
    arbitrary = (Compressed . B.compress) <$> arbitrary

compress_eq n bs = L.fromChunks [B.compress bs] == L.compress (smallChunk n bs)
decompress_eq n (Compressed bs) =
    L.fromChunks [B.decompress bs] == L.decompress (smallChunk n bs)

t_rechunk n bs = L.fromChunks [bs] == smallChunk n bs

l_roundtrip bs = L.decompress (L.compress bs) == bs

smallChunk n = rechunk ((n `mod` 63) + 1)

main = defaultMain tests

tests = [
    testProperty "s_roundtrip" s_roundtrip
  , testProperty "t_rechunk" t_rechunk
  , testProperty "compress_eq" compress_eq
  , testProperty "decompress_eq" decompress_eq
  , testProperty "l_roundtrip" l_roundtrip
  ]
