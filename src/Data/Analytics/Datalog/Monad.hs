{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is not considered packaged under the package versioning
-- policy. Any direct dependency upon it is likely to break even
-- between minor versions.
--
--------------------------------------------------------------------
module Data.Analytics.Datalog.Monad
  (
  -- * Datalog
    Datalog
  , DatalogT(..)
  , query
  ) where

import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Data.Analytics.Datalog.Atom
import Data.Analytics.Datalog.Term
import Data.Analytics.Datalog.Query
import Data.Functor.Bind
import Data.Functor.Identity

infixr 0 :-

------------------------------------------------------------------------------
-- Datalog
------------------------------------------------------------------------------

-- | An @operational@ encoding of a 'Datalog' program.
type Datalog = DatalogT Identity

-- | An @operational@ encoding of a 'Datalog' program with extra effects in @m@.
data DatalogT :: (* -> *) -> * -> * where
  (:-)   :: Atom a b -> Query a -> DatalogT m ()
  Fresh  :: DatalogT m Int
  Query  :: Query a -> DatalogT m [a]
  Bind   :: DatalogT m a -> (a -> DatalogT m b) -> DatalogT m b
  Return :: a -> DatalogT m a
  Lift   :: m a -> DatalogT m a

instance Functor (DatalogT m) where
  fmap f m = Bind m (Return . f)
  {-# INLINE fmap #-}

instance Apply (DatalogT m) where
  mf <.> ma = Bind mf $ \f -> fmap f ma
  {-# INLINE (<.>) #-}

instance Applicative (DatalogT m) where
  pure = Return
  {-# INLINE pure #-}

  mf <*> ma = Bind mf $ \f -> fmap f ma
  {-# INLINE (<*>) #-}

instance Bind (DatalogT m) where
  (>>-) = Bind
  {-# INLINE (>>-) #-}

instance Monad m => Monad (DatalogT m) where
  return = Return
  {-# INLINE return #-}

  (>>=) = Bind
  {-# INLINE (>>=) #-}

  fail = Lift . fail
  {-# INLINE fail #-}

instance MonadIO m => MonadIO (DatalogT m) where
  liftIO = Lift . liftIO
  {-# INLINE liftIO #-}

instance MonadState s m => MonadState s (DatalogT m) where
  get = lift get
  {-# INLINE get #-}

  put = lift . put
  {-# INLINE put #-}

  state = lift . state
  {-# INLINE state #-}

instance MonadReader e m => MonadReader e (DatalogT m) where
  reader = lift . reader
  {-# INLINE reader #-}

  ask = lift ask
  {-# INLINE ask #-}

  local f (Bind m k) = Bind (local f m) (local f . k)
  local f (Lift m)   = Lift (local f m)
  local _ m = m
  {-# INLINE local #-}

instance MonadTrans DatalogT where
  lift = Lift
  {-# INLINE lift #-}

instance (Term a, Entity a ~ a, u ~ ()) => TermOf (DatalogT m u) a

-- | Perform a 'Query'.
query :: Query a -> DatalogT m [a]
query = Query
{-# INLINE query #-}
