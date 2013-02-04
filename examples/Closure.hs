{-# LANGUAGE TypeFamilies, TemplateHaskell, DeriveDataTypeable, FlexibleContexts, GADTs, Rank2Types #-}
module Examples.Closure where

import Control.Applicative
import Control.Lens
import Data.Analytics
import Data.Foldable
import Data.String
import Data.Typeable
import Data.Void
import Generics.Deriving
import Prelude.Extras

data Node = A | B | C deriving (Eq,Ord,Show,Typeable)
instance Term Node

data NV = X | Y | Z deriving (Eq,Ord,Show,Typeable)
instance Term NV where type Entity NV = Node; term = var

data Edge = Edge Node Node deriving (Eq,Ord)

data Schema = EDGE | TC deriving (Eq,Ord,Show)

edge, tc :: T2 Schema Edge Node Node
edge = t2 EDGE Edge
tc   = t2 TC   Edge

test :: Datalog Schema [Edge]
test = do
  edge A B
  edge B C
  edge B A
  tc X Y :- edge X Y
  tc X Z :- tc X Y <* edge Y Z
  query $ tc A X <* no (edge X C)
