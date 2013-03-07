{-# LANGUAGE TypeFamilies, TemplateHaskell, DeriveDataTypeable, FlexibleContexts, GADTs, Rank2Types, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Examples.Closure where

import Control.Applicative
import Control.Monad.Logic
import Data.Analytics.Datalog
import Data.Typeable

data Node = A | B | C deriving (Eq,Ord,Show,Typeable)
instance Term Node

data NV = X | Y | Z deriving (Eq,Ord,Show,Typeable)
instance Term NV where type Entity NV = Node; term = var

data Edge = Edge Node Node () deriving (Eq,Ord,Show,Typeable)

edge, tc :: T2 Edge Node Node ()
edge = t2 (Table 0 const) Edge
tc   = t2 (Table 1 const) Edge

endpoint :: T1 Node Node ()
endpoint = t1 (Table 3 const) (\x () -> x)

test :: Monad m => DatalogT m (Logic Edge)
test = do
  T1 cyclic <- table (\x () -> x :: Node) const
  T1 acyclic <- table (\x () -> x :: Node) const
  edge A B
  edge B C
  edge B A
  tc X Y :- edge X Y
  tc X Z :- tc X Y <* edge Y Z
  cyclic X :- tc X X
  acyclic X :- (edge X Y <|> edge Y X) <* no (cyclic X)
  endpoint X :- acyclic X
  query $ row (tc A X) <* no (edge X C)

test' :: Datalog (Logic Edge)
test' = test
