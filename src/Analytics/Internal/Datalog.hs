{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
--------------------------------------------------------------------
-- |
-- Module    :  Analytics.Internal.Datalog
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module is not considered packaged under the package versioning
-- policy. Any direct dependency upon it is likely to break even
-- between minor versions.
--------------------------------------------------------------------
module Analytics.Internal.Datalog
  (
  -- * Datalog
    Datalog(..)
  , query
  ) where

import Analytics.Match
import Analytics.Query
import Analytics.Relation
import Control.Applicative
import Control.Monad.Trans.Class
import Data.Functor.Bind
import Data.Typeable
import Data.Void

infixr 1 :-

------------------------------------------------------------------------------
-- Datalog
------------------------------------------------------------------------------

-- | An @operational@ encoding of a 'Datalog' program.
data Datalog :: (* -> *) -> * -> * where
  Fact   :: (Typeable1 t, Match t) => (forall a. t a) -> Datalog m ()
  (:-)   :: Ord a => Relation a -> Query a t -> Datalog m ()
  Query  :: Ord a => Query a t -> Datalog m [t]
  Bind   :: Datalog m a -> (a -> Datalog m b) -> Datalog m b
  Return :: a -> Datalog m a
  Lift   :: m a -> Datalog m a

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

instance MonadTrans Datalog where
  lift = Lift
  {-# INLINE lift #-}

instance (Typeable1 t, Match t, u ~ ()) => Rel t Void (Datalog m u) where
  rel tv = Fact (vacuous tv)
  {-# INLINE rel #-}

query :: Ord a => Query a t -> Datalog m [t]
query = Query
{-# INLINE query #-}
