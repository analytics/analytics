{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE PolyKinds #-}
module Problem where

import Control.Applicative
import Control.Lens
import Data.Foldable
import Data.Functor.Identity
import Data.Monoid
import Prelude.Extras

------------------------------------------------------------------------------
-- Munging
------------------------------------------------------------------------------

infixl 1 >>-
infixr 1 -<<
infixr 0 :->

type a :-> b  = forall x. a x -> b x
type K m a b = forall x. a x -> m (b x)

class Fun f where
  hmap :: (a :-> b) -> f a :-> f b
  default hmap :: Munge f => (a :-> b) -> f a :-> f b
  hmap f m = runIdentity (munge (Identity . unit . f) m)

class Fun f => Munge f where
  unit  :: a :-> f a
  munge :: Applicative m => K m a (f b) -> K m (f a) (f b)

(>>-) :: Munge f => f a x -> (a :-> f b) -> f b x
m >>- f = runIdentity (munge (Identity . f) m)
{-# INLINE (>>-) #-}

(-<<) :: Munge f => (a :-> f b) -> f a :-> f b
f -<< m = m >>- f
{-# INLINE (-<<) #-}

------------------------------------------------------------------------------
-- Variable
------------------------------------------------------------------------------

class Variable t where
  var :: Prism' (t f a) (f a)

------------------------------------------------------------------------------
-- Prop
------------------------------------------------------------------------------

data Prop

deriving instance Show Prop

------------------------------------------------------------------------------
-- Bound
------------------------------------------------------------------------------

newtype Bound a = Bound { runBound :: Int }
  deriving (Eq,Ord,Show,Read)

instance Show1 Bound
instance Eq1 Bound
instance Ord1 Bound
instance Read1 Bound

instance Functor Bound where
  fmap _ = Bound . runBound

instance Foldable Bound where
  foldMap _ _ = mempty

instance Traversable Bound where
  traverse _ = pure . Bound . runBound

_Bound :: Iso (Bound a) (Bound b) Int Int
_Bound = iso runBound Bound

class HasBindings t where
  bindings :: Lens' t Int

------------------------------------------------------------------------------
-- Clause
------------------------------------------------------------------------------

infix 0 :-
data Clause f = (:-) { _clauseHead :: f Prop, _clauseBody :: [f Prop] }

instance Show1 f => Show (Clause f) where
  showsPrec d (h :- b) = showParen (d > 0) $
     showsPrec1 d h . showString " :- " . showList1 b

-- instance Fun Clause where
--   hmap f (x :- xs) = f x :- map f xs

makeLenses ''Clause

class HasClause h h' f f' | h -> f, h' -> f', h f' -> h', h' f -> h where
  clause :: Lens h h' (Clause f) (Clause f')

instance HasClause (Clause f) (Clause f') f f' where
  clause = id

------------------------------------------------------------------------------
-- Schema
------------------------------------------------------------------------------

data Schema t = Schema
  { _schemaBindings :: {-# UNPACK #-} !Int
  , _schemaClause :: Clause (t Bound)
  }

instance Show1 (t Bound) => Show (Schema t) where
  showsPrec d (Schema n hb) = showParen (d > 10) $
    showString "Schema " . showsPrec 11 n . showChar ' ' . showsPrec 11 hb

makeLenses ''Schema

instance HasBindings (Schema t) where
  bindings = schemaBindings

instance HasClause (Schema t) (Schema t') (t Bound) (t' Bound) where
  clause = schemaClause

------------------------------------------------------------------------------
-- EDB
------------------------------------------------------------------------------

newtype EDB t = EDB { runEDB :: forall (f :: * -> *). [t f Prop] } -- an EDB has no variables

instance Show1 (t Bound) => Show (EDB t) where
  showsPrec d (EDB xs) = showParen (d > 10) $
    showString "EDB " . showList1 (xs :: [t Bound Prop])

class HasEDB h h' t t' | h -> t, h' -> t', h t' -> h', h' t -> h where
  edb :: Lens h h' (EDB t) (EDB t')

instance HasEDB (EDB t) (EDB t') t t' where
  edb = id

------------------------------------------------------------------------------
-- IDB
------------------------------------------------------------------------------

newtype IDB t = IDB { runIDB :: [[Schema t]] }

deriving instance Show1 (t Bound) => Show (IDB t)

class HasIDB h h' t t' | h -> t, h' -> t', h t' -> h', h' t -> h where
  idb :: Lens h h' (IDB t) (IDB t')

instance HasIDB (IDB t) (IDB t') t t' where
  idb = id

_IDB :: Iso (IDB s) (IDB t) [[Schema s]] [[Schema t]]
_IDB = iso runIDB IDB

------------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------------

data Query t = Query
  { _queryBindings :: {-# UNPACK #-} !Int
  , _queryBody     :: [t Bound Prop]
  }

instance Show1 (t Bound) => Show (Query t) where
  showsPrec d (Query n xs) = showParen (d > 10) $
     showString "Query " . showsPrec 11 n . showChar ' ' . showList1 xs

makeLenses ''Query

class HasQuery h h' t t' | h -> t, h' -> t', h t' -> h', h' t -> h where
  query :: Lens h h' (Query t) (Query t')

instance HasQuery (Query t) (Query t') t t' where
  query = id

------------------------------------------------------------------------------
-- Problem
------------------------------------------------------------------------------

data Problem t = Problem
  { _problemEDB :: EDB t
  , _problemIDB :: IDB t
  , _problemQuery :: Query t
  }

deriving instance Show1 (t Bound) => Show (Problem t)

makeLenses ''Problem

instance HasQuery (Problem t) (Problem t) t t where
  query = problemQuery

instance HasEDB (Problem t) (Problem t) t t where
  edb = problemEDB

instance HasIDB (Problem t) (Problem t) t t where
  idb = problemIDB

------------------------------------------------------------------------------
-- Testing
------------------------------------------------------------------------------

data Node

deriving instance Show Node

-- a test dialect
data Foo :: (* -> *) -> * -> * where
  Var  :: f a -> Foo f a
  A    :: Foo f Node
  B    :: Foo f Node
  C    :: Foo f Node
  TC   :: Foo f Node -> Foo f Node -> Foo f Prop
  Edge :: Foo f Node -> Foo f Node -> Foo f Prop

instance Fun Foo
instance Munge Foo where
  unit = Var
  munge f (Var x)    = f x
  munge _ A          = pure A
  munge _ B          = pure B
  munge _ C          = pure C
  munge f (TC a b)   = TC   <$> munge f a <*> munge f b
  munge f (Edge a b) = Edge <$> munge f a <*> munge f b

instance Variable Foo where
  var = prism Var $ \t -> case t of
    Var fa -> Right fa
    _      -> Left t

instance f ~ Bound => Show1 (Foo f)

-- instance (Show1 f, Show a) => Show (Foo f a) where
instance (f ~ Bound, Show a) => Show (Foo f a) where -- force decent defaulting for REPL use
  showsPrec d (Var x) = showParen (d > 10) $
    showString "Var " . showsPrec1 11 x
  showsPrec _ A = showChar 'A'
  showsPrec _ B = showChar 'B'
  showsPrec _ C = showChar 'C'
  showsPrec d (TC x y) = showParen (d > 10) $
    showString "TC " . showsPrec 11 x . showChar ' ' . showsPrec 11 y
  showsPrec d (Edge x y) = showParen (d > 10) $
    showString "Edge " . showsPrec 11 x . showChar ' ' . showsPrec 11 y
