{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}
module Data.Analytics.File
  ( InsufficientDiskSpace
  ) where

import Control.Exception
import Control.Exception.Lens
import Control.Lens
import Data.Int
import Data.Typeable
import Foreign.C.Types
import System.Posix.IO
import System.Posix.Types
import System.IO

data InsufficientDiskSpace = InsufficientDiskSpace Handle {-# UNPACK #-} !Int64 String deriving (Typeable)
instance Show InsufficientDiskSpace where
  show (InsufficientDiskSpace _ n d) =
    "InsufficientDiskSpace: Unable to expand " ++ d ++ " to " ++ show n ++ " bytes"

instance Exception InsufficientDiskSpace

class AsInsufficientDiskSpace t where
  _InsufficientDiskSpace :: Prism' t InsufficientDiskSpace

instance AsInsufficientDiskSpace InsufficientDiskSpace where
  _InsufficientDiskSpace = id

instance AsInsufficientDiskSpace SomeException where
  _InsufficientDiskSpace = exception

fallocate :: Handle -> Int64 -> IO ()
fallocate h n = do
  fd <- handleToFd h
  i <- c_fallocate fd (fromIntegral n)
  case i of
    0 -> do
      desc <- hShow h
      throwIO (InsufficientDiskSpace h n desc)
    _ -> return ()

foreign import ccall c_fallocate :: Fd -> CSize -> IO Int
