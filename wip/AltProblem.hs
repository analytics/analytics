{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
-- {-# LANGUAGE PolyKinds #-}
module Problem where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Array
import Data.Data
import Data.Foldable
import Data.Graph hiding (Node)
import Data.Ix
import Data.Tree hiding (Node)
import Data.Functor.Identity
import GHC.Generics
import Data.Map as Map
import Data.Monoid
import Data.String
import Data.Traversable
import Prelude.Extras

------------------------------------------------------------------------------
-- Variable
------------------------------------------------------------------------------

class Variable t where
  var :: Prism (t a) (t b) a b

------------------------------------------------------------------------------
-- Prop
------------------------------------------------------------------------------

data Prop

deriving instance Show Prop

------------------------------------------------------------------------------
-- Bound
------------------------------------------------------------------------------

class HasBindings t where
  bindings :: Lens' t Int

------------------------------------------------------------------------------
-- Clause
------------------------------------------------------------------------------

infix 1 :-
data Clause a = a :- [a]
  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable)

class HasClause h h' f f' | h -> f, h' -> f', h f' -> h', h' f -> h where
  clause :: Lens h h' (Clause f) (Clause f')

instance HasClause (Clause f) (Clause f') f f' where
  clause = id

------------------------------------------------------------------------------
-- Schema
------------------------------------------------------------------------------

data Schema t = Schema
  { _schemaBindings :: {-# UNPACK #-} !Int
  , _schemaClause   :: Clause (t Int)
  }

instance Show (t Int) => Show (Schema t) where
  showsPrec d (Schema n hb) = showParen (d > 10) $
    showString "Schema " . showsPrec 11 n . showChar ' ' . showsPrec 11 hb

makeLenses ''Schema

instance HasBindings (Schema t) where
  bindings = schemaBindings

instance HasClause (Schema t) (Schema t') (t Int) (t' Int) where
  clause = schemaClause

(|-) :: (Traversable f, Ord a) => f a -> [f a] -> Schema f
h |- b = Schema (snd mnl) hb where
 (mnl, hb) = mapAccumLOf (traverse.traverse) go (Map.empty, 0) (h :- b)
 go mn@(m, n) k = case m^.at k of
   Just c  -> (mn, c)
   Nothing -> let n' = n + 1 in n' `seq` ((m & at k ?~ n, n'), n)


------------------------------------------------------------------------------
-- EDB
------------------------------------------------------------------------------

newtype EDB t = EDB { runEDB :: forall a. [t a] } -- an EDB has no variables

instance Show (t Int) => Show (EDB t) where
  showsPrec d (EDB xs) = showParen (d > 10) $
    showString "EDB " . showList (xs :: [t Int])

class HasEDB h h' t t' | h -> t, h' -> t', h t' -> h', h' t -> h where
  edb :: Lens h h' (EDB t) (EDB t')

instance HasEDB (EDB t) (EDB t') t t' where
  edb = id

------------------------------------------------------------------------------
-- IDB
------------------------------------------------------------------------------

newtype IDB t = IDB { runIDB :: [[Schema t]] }

deriving instance Show (t Int) => Show (IDB t)

class HasIDB h h' t t' | h -> t, h' -> t', h t' -> h', h' t -> h where
  idb :: Lens h h' (IDB t) (IDB t')

instance HasIDB (IDB t) (IDB t') t t' where
  idb = id

_IDB :: Iso (IDB s) (IDB t) [[Schema s]] [[Schema t]]
_IDB = iso runIDB IDB

-- | The wrong way to calculate strongly connected components between predicates
comps :: forall t. Data (t Int) => [Schema t] -> Forest Vertex -- IDB t
comps es = case dataTypeRep (dataTypeOf (undefined :: t Int)) of
  AlgRep cs | n <- length cs
            , arr <- accumArray (flip (:)) [] (0,n-1) $ es >>= \(Schema _ (h :- bs)) -> do
                let hi = constrIndex (toConstr h) - 1
                b <- bs
                return (hi, constrIndex (toConstr b) - 1)
            -> scc arr
  _ -> error "expected algebraic data type"

------------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------------

data Query t = Query
  { _queryBindings :: {-# UNPACK #-} !Int
  , _queryBody     :: [t Int]
  }

instance Show (t Int) => Show (Query t) where
  showsPrec d (Query n xs) = showParen (d > 10) $
     showString "Query " . showsPrec 11 n . showChar ' ' . showList xs

makeLenses ''Query

class HasQuery h h' t t' | h -> t, h' -> t', h t' -> h', h' t -> h where
  query :: Lens h h' (Query t) (Query t')

instance HasQuery (Query t) (Query t') t t' where
  query = id

que :: (Traversable f, Ord a) => [f a] -> Query f
que b = Query (snd mnl) b' where
 (mnl, b') = mapAccumLOf (traverse.traverse) go (Map.empty, 0) b
 go mn@(m, n) k = case m^.at k of
   Just c  -> (mn, c)
   Nothing -> let n' = n + 1 in n' `seq` ((m & at k ?~ n, n'), n)

------------------------------------------------------------------------------
-- Problem
------------------------------------------------------------------------------

data Problem t = Problem
  { _problemEDB :: EDB t
  , _problemIDB :: IDB t
  , _problemQuery :: Query t
  }

deriving instance Show (t Int) => Show (Problem t)

makeLenses ''Problem

instance HasQuery (Problem t) (Problem t) t t where
  query = problemQuery

instance HasEDB (Problem t) (Problem t) t t where
  edb = problemEDB

instance HasIDB (Problem t) (Problem t) t t where
  idb = problemIDB

problem :: (Traversable t, Ord x) => (forall a. [t a]) -> [[Schema t]] -> [t x] -> Problem t
problem e i q = Problem (EDB e) (IDB i) (que q)

------------------------------------------------------------------------------
-- Testing
------------------------------------------------------------------------------

data Node a = Node a | A | B | C
  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable,Data,Typeable,Generic)

instance a ~ String => IsString (Node a) where
  fromString = Node

makePrisms ''Node

instance Variable Node where
  var = _Node

-- a test dialect
data Test a
  = TC   (Node a) (Node a)
  | Edge (Node a) (Node a)
  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable,Data,Typeable,Generic)

toy :: Problem Test
toy = problem
  [Edge A B, Edge B C, Edge B A]
  [[ TC "x" "y" |- [Edge "x" "y"]
   , TC "x" "z" |- [TC "x" "y", Edge "y" "z"]
  ]]
  [ Edge A "x" ]
