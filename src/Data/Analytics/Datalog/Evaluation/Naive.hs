{-# LANGUAGE GADTs, DeriveDataTypeable #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Data.Analytics.Datalog.Evaluation.Naive
  ( Relation(..)
  , rows
  , insert
  ) where

import Control.Applicative
import Control.Lens
import Control.Monad
import Data.Analytics.Datalog.Atom
import Data.Analytics.Datalog.Row
import Data.Maybe
import Data.Map as Map hiding (insert)
import Data.Semigroup
import Data.Typeable

data Relation where
  Relation :: (Typeable a, Typeable b) => Map (Row (a -> b)) a -> Relation
  deriving Typeable

rows :: (At r, Typeable a, Typeable b, IxValue r ~ Relation) => Atom (Index r) a b -> r -> [b]
rows (Atom t r) m = case m^.at t of
  Nothing -> []
  Just (Relation rl) -> fromMaybe (error "wibble") $ cast $ do
     (r', a) <- Map.toList rl
     guard $ matches r r'
     f <- maybeToList (runRow r')
     return (f a)

insert :: (At r, Typeable a, Typeable b, Semigroup a, IxValue r ~ Relation) => Atom (Index r) a b -> a -> r -> r
insert (Atom t r) a m = m & at t %~ \ys -> case ys of
  Nothing            -> Just $! Relation (Map.singleton r a)
  Just (Relation rm) -> cast rm <&> \rn -> Relation $ rn & at r %~ \xs -> fmap (<> a) xs <|> Just a
