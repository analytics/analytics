{-# LANGUAGE OverloadedStrings, TypeFamilies, TemplateHaskell, DeriveFunctor, DeriveFoldable, DeriveTraversable, DeriveDataTypeable, DeriveGeneric, FlexibleContexts #-}
module Examples.Open where

import Analytics.Datalog
import Analytics.Match
import Analytics.Variable
import Control.Applicative
import Control.Lens
import Data.Char
import Data.Foldable
import Data.String
import Data.Typeable
import Data.Void
import Generics.Deriving

data Atom a = Var a | Atom String deriving (Eq,Ord,Show,Read,Functor,Foldable,Traversable,Typeable,Generic)
makePrisms ''Atom
instance Variable Atom where var = _Var
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

struct :: Relative Struct a r => String -> [Atom a] -> r
struct n xs = rel (Struct n xs)

edge, tc :: Relative Struct a r => Atom a -> Atom a -> r
edge x y = struct "edge" [x,y]
tc x y   = struct "tc" [x,y]

test :: Datalog m [Struct Void]
test = do
  edge "A" "B"
  edge "B" "C"
  edge "B" "A"
  struct "waffles" []
  tc x y :- edge x y
  tc x z :- tc x y :& edge y z
  es <- query $ tc "A" x :& no (edge x "C")
  return $ fst <$> es -- ignore the no
  where x = Var "x"; y = Var "y"; z = Var "z"
