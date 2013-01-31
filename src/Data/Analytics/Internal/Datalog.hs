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
--   (':-')   :: 'Ord' a => 'Relation' a -> Query a t -> 'Datalog' m ()
--   'Query'  :: 'Ord' a => 'Query' a t -> 'Datalog' m [t]
--   'Bind'   :: 'Datalog' m a -> (a -> 'Datalog' m b) -> 'Datalog' m b
--   'Return' :: a -> 'Datalog' m a
--   'Lift'   :: m a -> 'Datalog' m a
-- @
--------------------------------------------------------------------
module Data.Analytics.Internal.Datalog
  (
  -- * Datalog
    Datalog(..)
  , query
  ) where

import Data.Analytics.Match
import Data.Analytics.Query
import Data.Analytics.Relation
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.State.Class
import Control.Monad.Reader.Class
import Data.Functor.Bind
import Data.Typeable
import Data.Void

infixr 2 :-

------------------------------------------------------------------------------
-- Datalog
------------------------------------------------------------------------------

-- | An @operational@ encoding of a 'Datalog' program.
data Datalog m r
  = forall t. (Typeable1 t, Match t, r ~ ()) => Fact (forall x. t x) -- ^ Add a fact to the EDB
  | forall t a. (Ord a, r ~ ())  => Relation a :- Query a t          -- ^ Add an inference rule to the IDB
  | forall t a. (Ord a, r ~ [t]) => Query (Query a t)                -- ^ Perform a query
  | forall a. Bind (Datalog m a) (a -> Datalog m r)
  | Return r
  | Lift (m r)

instance Functor (Datalog m) where
  fmap f m = Bind m (Return . f)
  {-# INLINE fmap #-}

instance Apply (Datalog m) where
  mf <.> ma = Bind mf $ \f -> fmap f ma
  {-# INLINE (<.>) #-}

instance Applicative (Datalog m) where
  pure = Return
  {-# INLINE pure #-}

  mf <*> ma = Bind mf $ \f -> fmap f ma
  {-# INLINE (<*>) #-}

instance Bind (Datalog m) where
  (>>-) = Bind
  {-# INLINE (>>-) #-}

instance Monad m => Monad (Datalog m) where
  return = Return
  {-# INLINE return #-}

  (>>=) = Bind
  {-# INLINE (>>=) #-}

  fail = Lift . fail
  {-# INLINE fail #-}

instance MonadIO m => MonadIO (Datalog m) where
  liftIO = Lift . liftIO
  {-# INLINE liftIO #-}

instance MonadState s m => MonadState s (Datalog m) where
  get = lift get
  {-# INLINE get #-}

  put = lift . put
  {-# INLINE put #-}

  state = lift . state
  {-# INLINE state #-}

instance MonadReader e m => MonadReader e (Datalog m) where
  reader = lift . reader
  {-# INLINE reader #-}

  ask = lift ask
  {-# INLINE ask #-}

  local f (Bind m k) = Bind (local f m) (local f . k)
  local f (Lift m)   = Lift (local f m)
  local _ m = m
  {-# INLINE local #-}

instance MonadTrans Datalog where
  lift = Lift
  {-# INLINE lift #-}

instance (Typeable1 t, Match t, u ~ ()) => Rel t Void (Datalog m u) where
  rel tv = Fact (vacuous tv)
  {-# INLINE rel #-}

-- | Perform a 'Query'.
query :: Ord a => Query a t -> Datalog m [t]
query = Query
{-# INLINE query #-}
