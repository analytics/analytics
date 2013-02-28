{-# LANGUAGE DeriveDataTypeable #-}
module Data.Analytics.Active.Promise
  ( Promise(..)
  , promise
  , future
  ) where

-- import Control.Concurrent.STM
-- import Control.Monad
-- import Control.Monad.Cont
-- import Data.Analytics.Active.STM
import Data.Analytics.Active.Task
import Data.Profunctor
-- import Data.Traversable
import Data.Typeable

data Promise a b = Promise
  { fulfill :: a -> Task ()
  , await   :: Task b
  } deriving Typeable

instance Profunctor Promise where
  dimap f g (Promise h b) = Promise (h . f) (fmap g b)
  {-# INLINE dimap #-}

instance Functor (Promise a) where
  fmap f (Promise h b) = Promise h (fmap f b)
  {-# INLINE fmap #-}

promise :: Task (Promise a a)
promise = undefined
{-
promise = do
  queue <- newTChan
  result <- newEmptyTMV
  return $ Promise
   { fulfill   = \a -> do
     q <- stm $ do
       putTMVar result a
       readTChan queue
     forM_ q $ \k -> spawn (k a)
   , await = callCC $ \k -> join $ stm $ do
       tryReadTMVar result >>= \ ma -> case ma of
         Just a  -> return (k a)
         Nothing -> do writeTChan queue k; return undefined -- TODO: make some blocked arg for task to pass me to call instead
   }
-}

future :: Task a -> Task (Task a)
future m = do
  p <- promise
  spawn $ m >>= fulfill p
  return (await p)
{-# INLINE future #-}
