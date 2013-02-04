{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable, DeriveGeneric, FlexibleContexts #-}
module Examples.Open where

import Control.Applicative
import Control.Lens
import Data.Analytics
import Data.Char
import Data.Foldable
import Data.String
import Data.Typeable
import Data.Void
import Generics.Deriving
import Prelude.Extras

data Atom a = Var a | Atom String deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable,Typeable,Generic)
makePrisms ''Atom
instance Variable Atom where var = _Var
instance Eq1 Atom
instance Match Atom where match = matchVar
instance IsString (Atom a) where
  fromString = Atom

-- | This provides a big open relation type, which can hold arbitrary atoms
data Struct a = Struct String [Atom a] deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable,Typeable,Generic)
instance Match Struct where
  match f (Struct n xs) (Struct m ys)
    | n /= m = Nothing
    | otherwise = Struct n <$> go xs ys where
    go (a:as) (b:bs) = (:) <$> match f a b <*> go as bs
    go [] [] = Just []
    go _ _   = Nothing

struct :: Rel Struct a r => String -> [Atom a] -> r
struct n xs = rel (Struct n xs)

edge, tc :: Rel Struct a r => Atom a -> Atom a -> r
edge x y = struct "edge" [x,y]
tc x y   = struct "tc" [x,y]

test :: Datalog [Struct Void]
test = do
  edge "A" "B"
  edge "B" "C"
  edge "B" "A"
  struct "waffles" []
  tc x y :- edge x y
  tc x z :- tc x y & edge y z
  query $ tc "A" x & no (edge x "C")
  where x = Var "x"; y = Var "y"; z = Var "z"
