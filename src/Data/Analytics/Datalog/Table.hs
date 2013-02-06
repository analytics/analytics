{-# LANGUAGE Rank2Types, MultiParamTypeClasses, TypeFamilies, FunctionalDependencies, FlexibleInstances, UndecidableInstances #-}
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
import Data.Analytics.Datalog.Term

type T0 t o         = forall r.          Atomic r t o
                    => r
type T1 t o x       = forall r a.       (Atomic r t o, TermOf r a, Entity a ~ x)
                    => a -> r
type T2 t o x y     = forall r a b.     (Atomic r t o, TermOf r a, Entity a ~ x, TermOf r b, Entity b ~ y)
                    => a -> b -> r
type T3 t o x y z   = forall r a b c.   (Atomic r t o, TermOf r a, Entity a ~ x, TermOf r b, Entity b ~ y, TermOf r c, Entity c ~ z)
                    => a -> b -> c -> r
type T4 t o w x y z = forall r a b c d. (Atomic r t o, TermOf r a, Entity a ~ w, TermOf r b, Entity b ~ x, TermOf r c, Entity c ~ y, TermOf r d, Entity d ~ z)
                    => a -> b -> c -> d -> r

t0 :: t -> o -> T0 t o
t0 t o = atom t $ pure o

t1 :: t -> (x -> o) -> T1 t o x
t1 t f a = atom t $ f <$> arg a

t2 :: t -> (x -> y -> o) -> T2 t o x y
t2 t f a b = atom t $ f <$> arg a <*> arg b

t3 :: t -> (x -> y -> z -> o) -> T3 t o x y z
t3 t f a b c = atom t $ f <$> arg a <*> arg b <*> arg c

t4 :: t -> (w -> x -> y -> z -> o) -> T4 t o w x y z
t4 t f a b c d = atom t $ f <$> arg a <*> arg b <*> arg c <*> arg d

------------------------------------------------------------------------------
-- Useful for returning t0..t4 in a 'Monad' without @ImpredicativeTypes@.
------------------------------------------------------------------------------

data Table0 t o = T0 (T0 t o)
data Table1 t o x = T1 (T1 t o x)
data Table2 t o x y = T2 (T2 t o x y)
data Table3 t o x y z = T3 (T3 t o x y z)
data Table4 t o w x y z = T4 (T4 t o w x y z)

------------------------------------------------------------------------------
-- Tabled
------------------------------------------------------------------------------

class Tabled t k r | r -> t k where
  table :: MonadTable t m => k -> m r

instance Tabled t k (Table0 t k) where
  table k = freshTable 0 >>= \t -> return $ T0 (t0 t k)

instance Tabled t (x -> k) (Table1 t k x) where
  table k = freshTable 1 >>= \t -> return $ T1 (t1 t k)

instance Tabled t (x -> y -> k) (Table2 t k x y) where
  table k = freshTable 2 >>= \t -> return $ T2 (t2 t k)

instance Tabled t (x -> y -> z -> k) (Table3 t k x y z) where
  table k = freshTable 3 >>= \t -> return $ T3 (t3 t k)

instance Tabled t (w -> x -> y -> z -> k) (Table4 t k w x y z) where
  table k = freshTable 4 >>= \t -> return $ T4 (t4 t k)

------------------------------------------------------------------------------
-- MonadTable
------------------------------------------------------------------------------

class Monad m => MonadTable t m | m -> t where
  freshTable :: Int -> m t -- Build a table with a given arity

instance MonadTable t m => MonadTable t (Lazy.StateT s m) where
  freshTable = lift . freshTable
  {-# INLINE freshTable #-}

instance MonadTable t m => MonadTable t (Strict.StateT s m) where
  freshTable = lift . freshTable
  {-# INLINE freshTable #-}

instance MonadTable t m => MonadTable t (DatalogT t m) where
  freshTable = lift . freshTable
  {-# INLINE freshTable #-}
