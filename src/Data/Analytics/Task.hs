module Data.Analytics.Task
  (
  -- * Task
    Task(..)
  , MonadTask(..)
  , MonadSTM(..)
  -- , run
  , (|>>)
  -- * Observer
  , Observer(..)
  , foreach
  -- * Observable
  , Observable(..)
  , safe, fby, never, observe
  -- * Subscription
  , Subscription(..)
  ) where

import Data.Analytics.Task.Monad
import Data.Analytics.Task.STM
import Data.Analytics.Task.Observer
import Data.Analytics.Task.Observable
import Data.Analytics.Task.Subscription
