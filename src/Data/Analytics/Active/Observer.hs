module Data.Analytics.Active.Observer
  ( Observer(..)
  ) where

import Control.Exception
import Control.Monad.IO.Class
import Data.Analytics.Active.Task
import Data.Functor.Contravariant
import Data.Monoid

data Observer a = Observer
  { (!) :: a -> Task ()
  , err :: SomeException -> Task ()
  , complete :: Task ()
  }

instance Contravariant Observer where
  contramap g (Observer f h c) = Observer (f . g) h c
  {-# INLINE contramap #-}

instance Monoid (Observer a) where
  mempty = Observer (\_ -> return ()) (liftIO . throwIO) (return ())
  {-# INLINE mempty #-}
  p `mappend` q = Observer
    (\a -> do spawn (p ! a); q ! a)
    (\e -> spawn (err p e) >> err q e)
    (spawn (complete p) >> complete q)
  {-# INLINE mappend #-}
