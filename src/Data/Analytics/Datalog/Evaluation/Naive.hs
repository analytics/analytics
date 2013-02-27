{-# LANGUAGE CPP #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExistentialQuantification #-}
module Data.Analytics.Datalog.Evaluation.Naive
  ( Relation(..)
  , rows, bodyRows
  , insert
  , eval
  , Env(..)
  , HasEnv(..)
  , Rule(..)
  ) where

import Control.Applicative as Ap
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Data.Analytics.Datalog.Atom
import Data.Analytics.Datalog.Monad hiding (Fresh, (:-), Query)
import Data.Analytics.Datalog.Prompt
import Data.Analytics.Datalog.Query
import Data.Analytics.Datalog.Row
import Data.Analytics.Datalog.Subst
import Data.Maybe
import Data.IntMap as IntMap hiding (insert)
import Data.Map as Map hiding (insert)
import Data.Typeable

#ifndef HLINT
data Relation where
  Relation :: (Typeable a, Show a, Typeable b) => !(Map (Row (a -> b)) a) -> Relation
  deriving Typeable
#endif

instance Show Relation where
  showsPrec d (Relation m) = showParen (d > 10) $
    showString "Relation " . showsPrec 11 m

rows :: HasSubst s => Atom a b -> IntMap Relation -> StateT s [] (a, b)
rows (Atom i r) m = case m^.at (i^.tableId) of
  Nothing -> mzero
  Just (Relation rl) -> do
     (r', a) <- lift $ Map.toList rl
     r'' <- match r' r
     f <- lift $ maybeToList $ runRow r'' >>= cast
     a' <- lift $ maybeToList $ cast a
     return (a', f a)

insert' :: Atom a b -> a -> IntMap Relation -> Maybe (IntMap Relation)
insert' (Atom i r) a m = at (i^.tableId) ?? m $ \ys -> case ys of
  Nothing            -> Just $! Just $! Relation $ Map.singleton r a
  Just (Relation rm) -> do
    rn <- cast rm
    fmap (Just . Relation) $ at r ?? rn $ \xs -> case xs of
      Nothing -> Just $! Just $! a
      Just _  -> Nothing -- we should update using an omega-continuous semiring.

insert :: Atom a b -> a -> IntMap Relation -> (Any, IntMap Relation)
insert a r m = case insert' a r m of
  Just n  -> (Any True, n)
  Nothing -> (Any False, m)

data Rule where
  Rule :: Atom a b -> Query a -> Rule

data Env = Env
  { _envFresh :: {-# UNPACK #-} !Int
  , _edb      :: IntMap Relation
  , _idb      :: [Rule]
  } deriving Typeable

makeClassy ''Env

defacto :: Query a -> [a]
defacto (Ap l r)  = defacto l <*> defacto r
defacto (Map f x) = f <$> defacto x
defacto (Pure a)  = [a]
defacto (Alt l r) = defacto l ++ defacto r
defacto _         = []

-- eval :: (Monad m, HasEnv s) => DatalogT m a -> StateT s m a
eval :: HasEnv s => DatalogT IO a -> StateT s IO a
eval m = lift (promptT m) >>= \ s -> case s of
  Done a -> return a
  Fresh f :>>= k -> do
    i <- envFresh <+= 1
    eval $ k $ Table i f
  (h :- b) :>>= k -> case defacto b of
    [a] -> do
      _ <- edb %%= insert h a
      eval $ k ()
    _ -> do
      idb %= (Rule h b:)
      eval $ k ()
  Query q :>>= k -> do
    saturate
    use edb >>= lift . print
    xs <- uses edb $ \db -> evalStateT (bodyRows db q) mempty
    eval $ k xs

bodyRows :: IntMap Relation -> Query a -> StateT Subst [] a
bodyRows db (Ap l r)        = bodyRows db l <*> bodyRows db r
bodyRows db (Map f x)       = f <$> bodyRows db x
bodyRows _  (Pure a)        = pure a
bodyRows db (Alt l r)       = bodyRows db l <|> bodyRows db r
bodyRows _  Empty           = Ap.empty
bodyRows db (Row x)         = snd <$> rows x db
bodyRows db (Value x)       = fst <$> rows x db
bodyRows _  No{}            = Ap.empty -- we can't stratify in this mode
bodyRows _  Key{}           = Ap.empty -- we can't read keys without shuffling the query around.
bodyRows _  (Filtering _ _) = Ap.empty

saturate :: (MonadState s m, HasEnv s) => m ()
saturate = do
  rules <- use idb
  let go :: Rule -> (Any, IntMap Relation) -> (Any, IntMap Relation)
      go (Rule h b) (n,db) = case Prelude.foldr (goto h) (n,db) $ runStateT ?? mempty $ bodyRows db b of
        (m, db') -> (n <> m, db')
      goto :: Atom a b -> (a, Subst) -> (Any, IntMap Relation) -> (Any, IntMap Relation)
      goto h@Atom{} (a, e) (n, db) = case insert (apply e h) a db of
        (m, db') -> (n <> m, db')

  Any interesting <- edb %%= \db -> Prelude.foldr go (mempty,db) rules
  when interesting saturate
