{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
module Data.Analytics.Storage.Schedule
  ( Schedule(..)
  , HasSchedule(..)
  ) where

import Control.Lens
import Data.Int

-- This is used to schedule how we interleave our keys.

data Schedule a = Schedule
  { _priority  :: {-# UNPACK #-} !Int
  , _stride    :: {-# UNPACK #-} !Int
  , _count     :: {-# UNPACK #-} !Int
  , _ordered   :: Bool
  , _encoder   :: a -> Int64
  }

instance Contravariant Schedule where
  contramap f (Schedule p s c o e) = Schedule p s c o (e . f)
  {-# INLINE contramap #-}

makeClassy ''Schedule
