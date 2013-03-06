{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable #-}
module Data.Analytics.File
  ( InsufficientDiskSpace
  , AsInsufficientDiskSpace(..)
  , allocate
  , prefetch
  , sync
  ) where

import Control.Exception
import Control.Exception.Lens
import Control.Lens
import Data.Functor
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

-- | Expand a file with contiguous disk storage where possible.
allocate :: Handle -> Int64 -> IO ()
allocate h n = do
  fd <- handleToFd h
  i <- c_fallocate fd (fromIntegral n)
  case i of
    0 -> do
      desc <- hShow h
      throwIO (InsufficientDiskSpace h n desc)
    _ -> return ()

-- | Prefetch a range of a file from disk.
--  `prefetch h o c` signals the OS that the bytes(?) 
---  in the range o ... o+c will be accessed soon.
prefetch :: Handle -> Int64 -> Int64 -> IO ()
prefetch h o c = do
  fd <- handleToFd h
  () <$ c_prefetch fd (fromIntegral o) (fromIntegral c)

-- | Sync a file to disk.
sync :: Handle -> IO ()
sync h = do
  fd <- handleToFd h
  () <$ c_sync fd

foreign import ccall c_fallocate :: Fd -> CSize -> IO Int
foreign import ccall c_sync      :: Fd -> IO Int
foreign import ccall c_prefetch  :: Fd -> CSize -> CSize -> IO Int


