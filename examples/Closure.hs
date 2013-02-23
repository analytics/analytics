{-# LANGUAGE TypeFamilies, TemplateHaskell, DeriveDataTypeable, FlexibleContexts, GADTs, Rank2Types, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Examples.Closure where

import Control.Applicative
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

test :: Monad m => DatalogT m [Edge]
test = do
  edge A B
  edge B C
  edge B A
  tc X Y :- edge X Y
  tc X Z :- tc X Y <* edge Y Z
  query $ row (tc A X) <* no (edge X C)

test' :: Datalog [Edge]
test' = test
