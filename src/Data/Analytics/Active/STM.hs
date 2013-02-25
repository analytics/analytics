{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Analytics.Active.STM
  ( MonadSTM(..)
  ) where

import Control.Concurrent.STM
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import Control.Monad.Reader
import Control.Monad.Writer.Lazy as Lazy
import Control.Monad.Writer.Strict as Strict

class Monad m => MonadSTM m where
  stm :: STM a -> m a
  default stm :: (MonadTrans t, MonadSTM n, m ~ t n) => STM a -> m a
  stm = lift . stm
  {-# INLINE stm #-}

  newTV :: a -> m (TVar a)
  default newTV :: (MonadTrans t, MonadSTM n, m ~ t n) => a -> m (TVar a)
  newTV = lift . newTV
  {-# INLINE newTV #-}

  readTV :: TVar a -> m a
  default readTV :: (MonadTrans t, MonadSTM n, m ~ t n) => TVar a -> m a
  readTV = lift . readTV
  {-# INLINE readTV #-}

  newTMV :: a -> m (TMVar a)
  default newTMV :: (MonadTrans t, MonadSTM n, m ~ t n) => a -> m (TMVar a)
  newTMV = lift . newTMV
  {-# INLINE newTMV #-}

  newEmptyTMV :: m (TMVar a)
  default newEmptyTMV :: (MonadTrans t, MonadSTM n, m ~ t n) => m (TMVar a)
  newEmptyTMV = lift newEmptyTMV
  {-# INLINE newEmptyTMV #-}

  newTC :: m (TChan a)
  default newTC :: (MonadTrans t, MonadSTM n, m ~ t n) => m (TChan a)
  newTC = lift newTC
  {-# INLINE newTC #-}

instance MonadSTM STM where
  stm = id
  {-# INLINE stm #-}
  newTV = newTVar
  {-# INLINE newTV #-}
  readTV = readTVar
  {-# INLINE readTV #-}
  newTMV = newTMVar
  {-# INLINE newTMV #-}
  newEmptyTMV = newEmptyTMVar
  {-# INLINE newEmptyTMV #-}
  newTC = newTChan
  {-# INLINE newTC #-}

instance MonadSTM IO where
  stm = atomically
  {-# INLINE stm #-}
  newTV = newTVarIO
  {-# INLINE newTV #-}
  readTV = readTVarIO
  {-# INLINE readTV #-}
  newTMV = newTMVarIO
  {-# INLINE newTMV #-}
  newEmptyTMV = newEmptyTMVarIO
  {-# INLINE newEmptyTMV #-}
  newTC = newTChanIO
  {-# INLINE newTC #-}

instance MonadSTM m => MonadSTM (Lazy.StateT s m)
instance MonadSTM m => MonadSTM (Strict.StateT s m)
instance MonadSTM m => MonadSTM (ReaderT e m)
instance (MonadSTM m, Monoid w) => MonadSTM (Lazy.WriterT w m)
instance (MonadSTM m, Monoid w) => MonadSTM (Strict.WriterT w m)
