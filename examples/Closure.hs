{-# LANGUAGE TypeFamilies, TemplateHaskell, DeriveDataTypeable, FlexibleContexts, GADTs, Rank2Types, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
module Examples.Closure where

import Control.Applicative
import Data.Analytics.Datalog
import Data.Typeable

data Node = A | B | C deriving (Eq,Ord,Show,Typeable)
instance Term Node

data NV = X | Y | Z deriving (Eq,Ord,Show,Typeable)
instance Term NV where type Entity NV = Node; term = var

data Edge = Edge Node Node () deriving (Eq,Ord)

edge, tc :: T2 Edge Node Node ()
edge = t2 0 Edge -- const
tc   = t2 1 Edge -- const

test :: Datalog [Edge]
test = do
  edge A B
  edge B C
  edge B A
  tc X Y :- edge X Y
  tc X Z :- tc X Y <* edge Y Z
  query $ tc A X <* no (edge X C)
