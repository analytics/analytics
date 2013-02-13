{-# LANGUAGE TypeFamilies, TemplateHaskell, DeriveDataTypeable, FlexibleContexts, GADTs, Rank2Types #-}
module Examples.Closure where

import Control.Applicative
import Data.Analytics.Datalog
import Data.Typeable

data Node = A | B | C deriving (Eq,Ord,Show,Typeable)
instance Term Node

data NV = X | Y | Z deriving (Eq,Ord,Show,Typeable)
instance Term NV where type Entity NV = Node; term = var

data Edge = Edge Node Node deriving (Eq,Ord)

test :: MonadTable t m => DatalogT t m [Edge]
test = do
  T2 edge <- table Edge
  T2 tc   <- table Edge
  edge A B
  edge B C
  edge B A
  tc X Y :- edge X Y
  tc X Z :- tc X Y <* edge Y Z
  query $ tc A X <* no (edge X C)
