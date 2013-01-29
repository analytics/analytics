{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}
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
module Problem where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Array
import Data.Data
import Data.Foldable as Foldable
import Data.Graph hiding (Node)
import Data.Ix
import Data.Functor.Identity
import Data.Map as Map
import Data.Monoid
import Data.Sequence as Seq
import Data.Set as Set
import Data.Set.Lens as Set
import Data.String
import Data.Traversable
import Data.Tree hiding (Node)
import Prelude.Extras

------------------------------------------------------------------------------
-- Variable
------------------------------------------------------------------------------

class Variable t where
  _Var :: Prism (t a) (t b) a b

------------------------------------------------------------------------------
-- Bindings
------------------------------------------------------------------------------

class HasBindings t where
  bindings :: Lens' t Int

------------------------------------------------------------------------------
-- Legal
------------------------------------------------------------------------------

class Legal t where
  legal :: t -> Bool

instance Legal t => Legal [t] where
  legal = Prelude.all legal

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

instance (Foldable t, Ord a) => Legal (Clause (t a)) where
  legal (h :- b) = setOf folded h `isSubsetOf` setOf (folded.folded) b

------------------------------------------------------------------------------
-- Rule
------------------------------------------------------------------------------

data Rule t = Rule
  { _ruleBindings :: {-# UNPACK #-} !Int
  , _ruleClause   :: Clause (t Int)
  }

instance Show (t Int) => Show (Rule t) where
  showsPrec d (Rule n hb) = showParen (d > 10) $
    showString "Rule " . showsPrec 11 n . showChar ' ' . showsPrec 11 hb

makeLenses ''Rule

instance HasBindings (Rule t) where
  bindings = ruleBindings

instance HasClause (Rule t) (Rule t') (t Int) (t' Int) where
  clause = ruleClause

instance Foldable t => Legal (Rule t) where
  legal (Rule n c) = legal c && Set.size (setOf (folded.folded) c) == n

infix 1 .-
(.-) :: (Traversable f, Ord a) => f a -> [f a] -> Rule f
h .- b = Rule (snd mnl) hb where
 (mnl, hb) = mapAccumLOf (traverse.traverse) go (Map.empty, 0) (h :- b)
 go mn@(m, n) k = case m^.at k of
   Just c  -> (mn, c)
   Nothing -> let n' = n + 1 in n' `seq` ((m & at k ?~ n, n'), n)

------------------------------------------------------------------------------
-- EDB, the Extensional database
------------------------------------------------------------------------------

-- | Facts, stored as rule heads with no bound variables.
newtype EDB t = EDB { runEDB :: forall a. [t a] } -- an EDB has no variables

instance Show (t Int) => Show (EDB t) where
  showsPrec d (EDB xs) = showParen (d > 10) $
    showString "EDB " . showList (xs :: [t Int])

class HasEDB h h' t t' | h -> t, h' -> t', h t' -> h', h' t -> h where
  edb :: Lens h h' (EDB t) (EDB t')

instance HasEDB (EDB t) (EDB t') t t' where
  edb = id

instance Legal (EDB t) where
  legal _ = True

------------------------------------------------------------------------------
-- IDB, the Intensional database
------------------------------------------------------------------------------

-- | Inference rules, broken into SCCs
newtype IDB t = IDB { runIDB :: [[Rule t]] }

deriving instance Show (t Int) => Show (IDB t)

class HasIDB h h' t t' | h -> t, h' -> t', h t' -> h', h' t -> h where
  idb :: Lens h h' (IDB t) (IDB t')

instance HasIDB (IDB t) (IDB t') t t' where
  idb = id

_IDB :: Iso (IDB s) (IDB t) [[Rule s]] [[Rule t]]
_IDB = iso runIDB IDB

instance Foldable t => Legal (IDB t) where
  legal (IDB xxs) = legal xxs

rules :: forall t. Data (t Int) => [Rule t] -> IDB t
rules es = IDB $ case dataTypeRep (dataTypeOf (undefined :: t Int)) of
  AlgRep cs | n <- Prelude.length cs
            , arr <- accumArray (flip (:)) [] (0,n+m-1) $ Prelude.zip [n..] es >>= \(r,Rule _ (h :- bs)) -> do
               (constrIndex (toConstr h) - 1,r) : Prelude.map (\b -> (r, (constrIndex (toConstr b) - 1))) bs
            -> Prelude.filter (not . Prelude.null) $ do
               t <- scc arr -- for each tree in the forest
               return $ do
                 r <- Foldable.toList t
                 guard (r >= n)
                 return $ Seq.index ess (r - n)
  _ -> error "expected algebraic data type"
  where m = Prelude.length es
        ess = Seq.fromList es

------------------------------------------------------------------------------
-- Query
------------------------------------------------------------------------------

-- | A rule body with bound variables.
data Query t = Query
  { _queryBindings :: {-# UNPACK #-} !Int
  , _queryBody     :: [t Int]
  }

instance Show (t Int) => Show (Query t) where
  showsPrec d (Query n xs) = showParen (d > 10) $
     showString "Query " . showsPrec 11 n . showChar ' ' . showList xs

instance Foldable t => Legal (Query t) where
  legal (Query n xxs) = Set.size (setOf (folded.folded) xxs) == n

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

instance Foldable t => Legal (Problem t) where
  legal (Problem e i q) = legal e && legal i && legal q

problem :: (Data (t Int), Traversable t, Ord x) => (forall a. [t a]) -> [Rule t] -> [t x] -> Problem t
problem e i q = Problem (EDB e) (rules i) (que q)

------------------------------------------------------------------------------
-- Testing
------------------------------------------------------------------------------

data Node a = Node a | A | B | C | D | E | F | G
  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable,Data,Typeable)

instance a ~ String => IsString (Node a) where
  fromString = Node

makePrisms ''Node

instance Variable Node where
  _Var = _Node

data Test a
  = TC   (Node a) (Node a)
  | Edge (Node a) (Node a)
  deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable,Data,Typeable)

toy :: Problem Test
toy = problem
  [Edge A B, Edge B C, Edge B A, Edge C D, Edge D E, Edge E F]
  [ TC "x" "y" .- [Edge "x" "y"]
  , TC "x" "z" .- [TC "x" "y", Edge "y" "z"]
  ]
  [ TC A "x" ]
