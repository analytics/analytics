{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------
-- |
-- Module    :  Data.Analytics.Internal.Datalog
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
-- The definition for 'Datalog' is forced into @ExistentialQuantification@
-- syntax because of haddock bug #43. <http://trac.haskell.org/haddock/ticket/43>
--
-- Otherwise it would be written as:
--
-- @
-- data 'Datalog' :: (* -> *) -> * -> * where
--   'Fact'   :: ('Typeable1' t, 'Match' t) => (forall a. t a) -> 'Datalog' m ()
--   (':-')   :: 'Ord' a => 'Relation' a -> 'Query' a t -> 'Datalog' m ()
--   'Query'  :: 'Ord' a => 'Query' a t -> 'Datalog' m [t]
--   'Data.Analytics.Internal.Datalog.Bind'   :: 'Datalog' m a -> (a -> 'Datalog' m b) -> 'Datalog' m b
--   'Return' :: a -> 'Datalog' m a
--   'Lift'   :: m a -> 'Datalog' m a
-- @
--------------------------------------------------------------------
module Data.Analytics.Internal.Datalog
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
import Data.Analytics.Match
import Data.Analytics.Query
import Data.Analytics.Relation
import Data.Functor.Bind
import Data.Functor.Identity
import Data.Typeable
import Data.Void

infixr 0 :-

------------------------------------------------------------------------------
-- Datalog
------------------------------------------------------------------------------

-- | An @operational@ encoding of a 'Datalog' program.
type Datalog = DatalogT Identity

-- | An @operational@ encoding of a 'Datalog' program with extra effects in @m@.
data DatalogT m r
  = forall t. (Typeable1 t, Match t, r ~ ()) => Fact (forall x. t x) -- ^ Add a 'Fact' to the EDB
  | forall t a. (Ord a, r ~ ())  => Relation a :- Query a t          -- ^ Add an inference rule to the IDB
  | forall t a. (Ord a, r ~ [t]) => Query (Query a t)                -- ^ Perform a 'query'
  | forall a. Bind (DatalogT m a) (a -> DatalogT m r)
  | Return r
  | Lift (m r)

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

instance (Typeable1 t, Match t, u ~ ()) => Rel t Void (DatalogT m u) where
  rel tv = Fact (vacuous tv)
  {-# INLINE rel #-}

-- | Perform a 'Query'.
query :: Ord a => Query a t -> DatalogT m [t]
query = Query
{-# INLINE query #-}
