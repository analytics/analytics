{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable, DeriveGeneric, FlexibleContexts #-}
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

data Node a = Node a | A | B | C deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable,Typeable,Generic)
makePrisms ''Node
instance a ~ String => IsString (Node a) where fromString = Node
instance Eq1 Node
instance Variable Node where var = _Node
instance Match Node where match = matchVar

data Edge a = Edge (Node a) (Node a) deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable,Typeable,Generic)
instance Match Edge where match f (Edge a b) (Edge c d) = Edge <$> match f a c <*> match f b d
edge :: Rel Edge a r => Node a -> Node a -> r
edge x y = rel (Edge x y)

data TC a = TC (Node a) (Node a) deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable,Typeable,Generic)
instance Match TC where match f (TC a b) (TC c d) = TC <$> match f a c <*> match f b d
tc :: Rel TC a r => Node a -> Node a -> r
tc x y = rel (TC x y)

test :: Monad m => Datalog m [TC Void]
test = do
  edge A B
  edge B C
  edge B A
  tc x y :- edge x y
  tc x z :- tc x y <* edge y z
  query $ tc A x <* no (edge x C)
  where x = "x"; y = "y"; z = "z"
