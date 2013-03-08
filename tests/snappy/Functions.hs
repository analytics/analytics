module Functions
    (
      rechunk
    ) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

rechunk :: Int -> B.ByteString -> L.ByteString
rechunk n
    | n <= 0    = error "rechunk: wtf!?"
    | otherwise = L.fromChunks . go
  where go bs | B.null bs = []
              | otherwise = case B.splitAt n bs of
                              (x,y) -> x : go y
