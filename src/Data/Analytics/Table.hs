{-# LANGUAGE Rank2Types, MultiParamTypeClasses, TypeFamilies #-}
module Data.Analytics.Table
  ( T0, T1, T2, T3, T4
  , t0, t1, t2, t3, t4
  ) where

import Control.Applicative
import Data.Analytics.Internal.Atom
import Data.Analytics.Internal.Atomic
import Data.Analytics.Internal.Term

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

t0 :: t o -> o -> T0 t o
t0 t o = atom t $ pure o

t1 :: t o -> (x -> o) -> T1 t o x
t1 t f a = atom t $ f <$> arg a

t2 :: t o -> (x -> y -> o) -> T2 t o x y
t2 t f a b = atom t $ f <$> arg a <*> arg b

t3 :: t o -> (x -> y -> z -> o) -> T3 t o x y z
t3 t f a b c = atom t $ f <$> arg a <*> arg b <*> arg c

t4 :: t o -> (w -> x -> y -> z -> o) -> T4 t o w x y z
t4 t f a b c d = atom t $ f <$> arg a <*> arg b <*> arg c <*> arg d
