{-# LANGUAGE GADTs, DeriveDataTypeable #-}
module Data.Analytics.Datalog.Evaluation.Naive
  ( Relation(..)
  , rows
  , insert
  ) where

import Control.Lens
import Data.Analytics.Datalog.Atom
import Data.Analytics.Datalog.Row
import Data.Maybe
import Data.IntMap
import Data.Semigroup
import Data.Typeable

data Relation where
  Relation :: (Typeable a, Typeable b) => [(Row (a -> b), a -> b, a)] -> Relation
  deriving Typeable

rows :: (Typeable a, Typeable b) => Atom Int a b -> IntMap Relation -> [b]
rows (Atom t r) m = case m^.at t of
  Nothing -> []
  Just (Relation rl) -> fromMaybe (error "wibble") $ cast [ f a | (r',f,a) <- rl, matches r r' ]

insert :: (Typeable a, Typeable b, Semigroup a) => Atom Int a b -> a -> IntMap Relation -> IntMap Relation
insert (Atom t r) a m = case runRow r of
  Nothing -> error "wobble"
  Just f -> m & at t %~ relate f
 where
    relate f Nothing   = Just $ Relation [(r,f,a)]
    relate f (Just (Relation rl)) = Just $ Relation $ go f $ fromMaybe (error "twubble") $ cast rl
    go f [] = [(r,f,a)]
    go f (rgb@(r',g,b):xs)
      | matches r r' = (r',g,b <> a):xs
      | otherwise    = rgb:go f xs
