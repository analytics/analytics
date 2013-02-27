{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE Trustworthy #-}
module Data.Analytics.Active.Task
  ( Task(..)
  , MonadTask(..)
  , run
  , (|>>)
  , Job
  ) where

import Control.Applicative
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Monad.CatchIO as E
import Control.Monad.Cont
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import Control.Monad.Writer.Lazy as Lazy
import Control.Monad.Writer.Strict as Strict
import Control.Monad.Reader as Reader
import Data.Analytics.Active.STM
import Data.Typeable
import System.IO.Unsafe

type Job = Task ()

jobs :: TChan Job
jobs = unsafePerformIO newTChanIO
{-# NOINLINE jobs #-}

theQ :: Job -> ContT () IO ()
theQ = stm . writeTChan jobs

-- | Eventually we can do work stealing. For now we just push work onto the channel
-- and keep taking the next item. Other workers can take from the job pool too
run :: MonadIO m => Task a -> m a
run t0 = liftIO $ do
  result <- newEmptyMVar
  let 

      trampoline = stm (tryReadTChan jobs) >>= \ma -> case ma of
        Nothing -> return ()
        Just r -> runTask r theQ >> trampoline
  runContT (runTask t0 theQ <* trampoline) (putMVar result)
  takeMVar result

{-
run t0 = liftIO $ do
  q <- newIORef (mempty :: Deque (Task ()))
  result <- newEmptyMVar
  let enq t = liftIO $ atomicModifyIORef q $ \pq -> let pq' = snoc pq t in pq' `seq` (pq', ())
      loop = join $ liftIO $ atomicModifyIORef q $ \pq -> case uncons pq of
          Nothing       -> (pq , return ())
          Just (r, pq') -> (pq', runTask r enq >> loop)
  runContT (runTask t0 enq <* loop) (putMVar result)
  takeMVar result
-}

data Task a = Task
  { runTask :: (Job -> ContT () IO ()) -> ContT () IO a
  }
  deriving (Functor, Typeable)

instance Applicative Task where
  pure = Task . const . pure
  {-# INLINE pure #-}

  Task mf <*> Task ma = Task $ \q -> do
    mf q <*> ma q
  {-# INLINE (<*>) #-}

instance Monad Task where
  return = Task . const . return
  {-# INLINE return #-}

  Task m >>= k = Task $ \q -> do
    a <- m q
    runTask (k a) q
  {-# INLINE (>>=) #-}

instance MonadCatchIO Task where
  Task m `catch` k = Task $ \ q -> m q `E.catch` \a -> runTask (k a) q
  {-# INLINE catch #-}
  block (Task m) = Task (block . m)
  {-# INLINE block #-}
  unblock (Task m) = Task (unblock . m)
  {-# INLINE unblock #-}

instance MonadIO Task where
  liftIO = Task . const . liftIO
  {-# INLINE liftIO #-}

instance MonadSTM Task where
  stm = liftIO . stm
  {-# INLINE stm #-}
  newTV = liftIO . newTV
  {-# INLINE newTV #-}
  readTV = liftIO . readTV
  {-# INLINE readTV #-}
  newTMV = liftIO . newTMV
  {-# INLINE newTMV #-}
  newEmptyTMV = liftIO newEmptyTMV
  {-# INLINE newEmptyTMV #-}
  newTC = liftIO newTC
  {-# INLINE newTC #-}

instance MonadCont Task where
  callCC f = Task $ \ r ->
    callCC $ \ c ->
    runTask (f (Task . const . c)) r
  {-# INLINE callCC #-}

class MonadIO m => MonadTask m where
  spawn :: Task () -> m ()
#ifndef HLINT
  default spawn :: (MonadTrans t, MonadTask n, m ~ t n) => Task () -> t n ()
  spawn = lift . spawn
#endif

-- | Spawn a background task and continue
(|>>) :: Task a -> Task b -> Task b
m |>> r = spawn (() <$ m) >> r

instance MonadTask Task where
  spawn t = Task $ \ q -> q t
  {-# INLINE spawn #-}

instance MonadTask m => MonadTask (Strict.StateT s m)
instance MonadTask m => MonadTask (Lazy.StateT s m)
instance MonadTask m => MonadTask (ReaderT e m)
instance (MonadTask m, Monoid w) => MonadTask (Strict.WriterT w m)
instance (MonadTask m, Monoid w) => MonadTask (Lazy.WriterT w m)
instance MonadTask m => MonadTask (ContT r m)
