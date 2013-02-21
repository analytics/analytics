{-# LANGUAGE Rank2Types, MultiParamTypeClasses, TypeFamilies, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Datalog.Table
  ( T0, T1, T2, T3, T4
  , t0, t1, t2, t3, t4
  -- * Implementation Details
  , Atom
  , Atomic(..)
  -- * Predicative Tables
  , Table0(..), Table1(..), Table2(..), Table3(..), Table4(..)
  , Tabled(..)
  , MonadTable(..)
  ) where

import Control.Applicative
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import Data.Analytics.Datalog.Atom
import Data.Analytics.Datalog.Atomic
import Data.Analytics.Datalog.Monad
import Data.Analytics.Datalog.Row
import Data.Analytics.Datalog.Term

type T0 o m         = forall r.          Atomic r m o
                      => r
type T1 o x m       = forall r a.       (Atomic r m o, TermOf r a, Entity a ~ x)
                      => a -> r
type T2 o x y m     = forall r a b.     (Atomic r m o, TermOf r a, Entity a ~ x, TermOf r b, Entity b ~ y)
                      => a -> b -> r
type T3 o x y z m   = forall r a b c.   (Atomic r m o, TermOf r a, Entity a ~ x, TermOf r b, Entity b ~ y, TermOf r c, Entity c ~ z)
                      => a -> b -> c -> r
type T4 o w x y z m = forall r a b c d. (Atomic r m o, TermOf r a, Entity a ~ w, TermOf r b, Entity b ~ x, TermOf r c, Entity c ~ y, TermOf r d, Entity d ~ z)
                      => a -> b -> c -> d -> r

t0 :: Int -> (m -> o) -> T0 o m
t0 t o = atom t $ pure o

t1 :: Int -> (x -> m -> o) -> T1 o x m
t1 t f a = atom t $ f <$> arg a

t2 :: Int -> (x -> y -> m -> o) -> T2 o x y m
t2 t f a b = atom t $ f <$> arg a <*> arg b

t3 :: Int -> (x -> y -> z -> m -> o) -> T3 o x y z m
t3 t f a b c = atom t $ f <$> arg a <*> arg b <*> arg c

t4 :: Int -> (w -> x -> y -> z -> m -> o) -> T4 o w x y z m
t4 t f a b c d = atom t $ f <$> arg a <*> arg b <*> arg c <*> arg d

------------------------------------------------------------------------------
-- Useful for returning t0..t4 in a 'Monad' without @ImpredicativeTypes@.
------------------------------------------------------------------------------

data Table0 o m = T0 (T0 o m)
data Table1 o x m = T1 (T1 o x m)
data Table2 o x y m = T2 (T2 o x y m)
data Table3 o x y z m = T3 (T3 o x y z m)
data Table4 o w x y z m = T4 (T4 o w x y z m)

------------------------------------------------------------------------------
-- Tabled
------------------------------------------------------------------------------

class Tabled k r | r -> k where
  table :: MonadTable m => k -> m r

instance Tabled (n -> k) (Table0 k n) where
  table k = freshTable >>= \t -> return $ T0 (t0 t k)

instance Tabled (x -> n -> k) (Table1 k x n) where
  table k = freshTable >>= \t -> return $ T1 (t1 t k)

instance Tabled (x -> y -> n -> k) (Table2 k x y n) where
  table k = freshTable >>= \t -> return $ T2 (t2 t k)

instance Tabled (x -> y -> z -> n -> k) (Table3 k x y z n) where
  table k = freshTable >>= \t -> return $ T3 (t3 t k)

instance Tabled (w -> x -> y -> z -> n -> k) (Table4 k w x y z n) where
  table k = freshTable >>= \t -> return $ T4 (t4 t k)

------------------------------------------------------------------------------
-- MonadTable
------------------------------------------------------------------------------

class Monad m => MonadTable m where
  freshTable :: m Int -- Build a table with a given arity

instance MonadTable m => MonadTable (Lazy.StateT s m) where
  freshTable = lift freshTable
  {-# INLINE freshTable #-}

instance MonadTable m => MonadTable (Strict.StateT s m) where
  freshTable = lift freshTable
  {-# INLINE freshTable #-}

instance MonadTable m => MonadTable (DatalogT m) where
  freshTable = lift freshTable
  {-# INLINE freshTable #-}
