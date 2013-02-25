{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Analytics.Active.Task
  ( Task(..)
  , MonadTask(..)
  , run
  ) where

import Control.Applicative
import Control.Lens
import Control.Lens.Internal.Deque
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import Control.Monad.Writer.Lazy as Lazy
import Control.Monad.Writer.Strict as Strict
import Control.Monad.Reader as Reader
import Data.Typeable
import Data.IORef

-- | Eventually we can do work stealing. For now we just push work onto a banker's queue
-- and take the next item.
run :: MonadIO m => Task a -> m a
run t0 = liftIO $ do
  q <- newIORef (mempty :: Deque (Task ()))
  let enq t = atomicModifyIORef q $ \pq -> let pq' = snoc pq t in pq' `seq` (pq', ())
      loop = join $ atomicModifyIORef q $ \pq -> case uncons pq of
          Nothing       -> (pq , return ())
          Just (r, pq') -> (pq', runTask r enq >> loop)
  runTask t0 enq <* loop

data Task a = Task { runTask :: (Task () -> IO ()) -> IO a }
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

instance MonadIO Task where
  liftIO = Task . const
  {-# INLINE liftIO #-}

class MonadIO m => MonadTask m where
  spawn :: Task () -> m ()
  default spawn :: (MonadTrans t, MonadTask n, m ~ t n) => Task () -> t n ()
  spawn = lift . spawn

instance MonadTask Task where
  spawn t = Task $ \ q -> q t
  {-# INLINE spawn #-}

instance MonadTask m => MonadTask (Strict.StateT s m)
instance MonadTask m => MonadTask (Lazy.StateT s m)
instance MonadTask m => MonadTask (ReaderT e m)
instance (MonadTask m, Monoid w) => MonadTask (Strict.WriterT w m)
instance (MonadTask m, Monoid w) => MonadTask (Lazy.WriterT w m)
