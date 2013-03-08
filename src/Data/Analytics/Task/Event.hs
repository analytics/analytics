--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Task.Event
  ( Event(..)
  , newEvent
  , before
  , after
  , causing
  ) where

import Control.Monad
import Control.Concurrent.STM
import Control.Lens
import Data.Analytics.Task.STM

newtype Event a = Event { getEvent :: TVar (Maybe a) }

newEvent :: MonadSTM m => m (Event a)
newEvent = Event `liftM` newTV Nothing
{-# INLINE newEvent #-}

before :: MonadSTM m => Event a -> m Bool
before (Event e) = isn't _Just `liftM` readTV e
{-# INLINE before #-}

causing :: MonadSTM m => Event a -> a -> m Bool
causing (Event e) a = stm $ readTVar e >>= \ mb -> case mb of
  Nothing -> do writeTVar e (Just a); return True
  Just _  -> return False
{-# INLINE causing #-}

after :: MonadSTM m => Event a -> m Bool
after (Event e) = stm $ isn't _Nothing `liftM` readTVar e
{-# INLINE after #-}
