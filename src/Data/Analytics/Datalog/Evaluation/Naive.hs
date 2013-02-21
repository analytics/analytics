{-# LANGUAGE GADTs, DeriveDataTypeable #-}
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
import Data.IntMap as IntMap hiding (insert)
import Data.Map as Map hiding (insert)
import Data.Semigroup
import Data.Typeable

data Relation where
  Relation :: (Typeable a, Typeable b) => Map (Row (a -> b)) a -> Relation
  deriving Typeable

rows :: (Typeable a, Typeable b) => Atom Int a b -> IntMap Relation -> [b]
rows (Atom t r) m = case m^.at t of
  Nothing -> []
  Just (Relation rl) -> fromMaybe (error "wibble") $ cast $ do
     (r', a) <- Map.toList rl
     guard $ matches r r'
     f <- maybeToList (runRow r')
     return (f a)

insert :: (Typeable a, Typeable b, Semigroup a) => Atom Int a b -> a -> IntMap Relation -> IntMap Relation
insert (Atom t r) a m = m & at t %~ \ys -> case ys of
  Nothing            -> Just $! Relation (Map.singleton r a)
  Just (Relation rm) -> cast rm <&> \rn -> Relation $ rn & at r %~ \xs -> fmap (<> a) xs <|> Just a
