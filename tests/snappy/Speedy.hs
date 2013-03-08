{-# LANGUAGE BangPatterns, DeriveDataTypeable, OverloadedStrings,
    RecordWildCards #-}

import Control.Exception
import Data.Maybe
import Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L
import System.IO
import System.Exit
import qualified Codec.Compression.GZip as G
import qualified Codec.Compression.Snappy as S
import System.Console.CmdArgs
import Data.Data
import Data.Typeable
import Data.Time.Clock

data Codec = Snappy | GZip
             deriving (Eq, Show, Typeable, Data)

data Action = Compress | Decompress
              deriving (Eq, Show, Typeable, Data)

data Command = Command {
      action :: Action
    , codec :: Codec
    , level :: Maybe Int
    , number :: Maybe Int
    , files :: [FilePath]
    } deriving (Show, Typeable, Data)

command = Command { action = enum [Compress, Decompress]
                  , codec = enum [Snappy, GZip]
                  , level = def
                  , number = def
                  , files = def &= args
                  }

rnf (L.Chunk _ cs) = rnf cs
rnf _              = ()

snappy Command{..} f = do
  bs0 <- B.readFile f
  let bs | action == Compress = bs0
         | otherwise          = S.compress bs0
      count = fromMaybe (200000000 `div` B.length bs) number
      c !i s | i >= count = ()
             | otherwise  = S.compress s `seq` c (i+1) s
      d !i s | i >= count = ()
             | otherwise  = S.decompress s `seq` d (i+1) s
  start <- getCurrentTime
  evaluate $ if action == Compress then c 0 bs else d 0 bs
  time <- (fromRational . toRational . flip diffUTCTime start) `fmap`
          getCurrentTime
  return (fromIntegral (B.length bs) * fromIntegral count / (time * 1048576.0),
          (B.length (S.compress bs0) * 100) `div` B.length bs0)

gzip Command{..} f = do
  bs0 <- L.readFile f
  let bs | action == Compress = bs0
         | otherwise          = compress bs0
      compress = G.compressWith G.defaultCompressParams {
                   G.compressLevel = G.CompressionLevel $ fromMaybe 3 level
                 }
      len = L.length bs
      count = fromMaybe (25000000 `div` fromIntegral len) number
      c !i s | i >= count = ()
             | otherwise  = rnf (compress s) `seq` c (i+1) s
      d !i s | i >= count = ()
             | otherwise  = rnf (G.decompress s) `seq` d (i+1) s
  start <- getCurrentTime
  evaluate $ if action == Compress then c 0 bs else d 0 bs
  time <- (fromRational . toRational . flip diffUTCTime start) `fmap`
          getCurrentTime
  return (fromIntegral len * fromIntegral count / (time * 1048576.0),
          fromIntegral $ (L.length (compress bs0) * 100) `div` L.length bs0)

main = do
  c@Command{..} <- cmdArgs command
  forM_ files $ \f -> do
    (mbSec, ratio) <- (if codec == Snappy then snappy else gzip) c f
    putStrLn $ show codec ++ " " ++ show action ++ " " ++
               show f ++ ": " ++ show (round mbSec) ++ " MB/sec, " ++
               show (100 - ratio) ++ "% smaller"
