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
  , naive, stratified
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
import Data.Analytics.Datalog.Term
import Data.Array
import Data.Foldable as Foldable
import Data.Graph
import Data.IntMap as IntMap hiding (insert)
import qualified Data.IntSet as IntSet
import Data.Map as Map hiding (insert)
import Data.Maybe
import Data.Sequence as Seq
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

instance Show Rule where
  showsPrec d (Rule a b) = showParen (d > 0) $
    shows a . showString " :- " . showsPrec 1 b

data Env = Env
  { _envFresh :: {-# UNPACK #-} !Int
  , _edb      :: IntMap Relation
  , _idb      :: [Rule]
  } deriving (Show, Typeable)

makeClassy ''Env

defacto :: Query a -> [a]
defacto (Ap l r)  = defacto l <*> defacto r
defacto (Map f x) = f <$> defacto x
defacto (Pure a)  = [a]
defacto (Alt l r) = defacto l ++ defacto r
defacto _         = []

naive :: (Monad m, HasEnv s) => DatalogT m a -> StateT s m a
naive m = lift (promptT m) >>= \ s -> case s of
  Done a -> return a
  Fresh f :>>= k -> do
    i <- envFresh <+= 1
    naive $ k $ Table i f
  (h :- b) :>>= k -> case defacto b of
    [a] -> do
      _ <- edb %%= insert h a
      naive $ k ()
    _ -> do
      idb %= (Rule h b:)
      naive $ k ()
  Query q :>>= k -> do
    saturate
    xs <- uses edb $ \db -> evalStateT (bodyRows db q) mempty
    naive $ k xs

stratified :: (Monad m, HasEnv s) => DatalogT m a -> StateT s m a
stratified m = lift (promptT m) >>= \ s -> case s of
  Done a -> return a
  Fresh f :>>= k -> do
    i <- envFresh <+= 1
    stratified $ k $ Table i f
  (h :- b) :>>= k -> case defacto b of
    [a] -> do
      _ <- edb %%= insert h a
      stratified $ k ()
    _ -> do
      idb %= (Rule h b:)
      stratified $ k ()
  Query q :>>= k -> do
    rules <- use idb
    Foldable.forM_ (stratifying rules) $ \stratum -> do
       idb .= stratum
       saturate
    idb .= rules
    xs <- uses edb $ \db -> evalStateT (bodyRows db q) mempty
    stratified $ k xs

stratifying :: [Rule] -> [[Rule]]
stratifying rules = Prelude.filter (not . Prelude.null) $ do
   t <- scc arr
   return $ do
     r <- Foldable.toList t
     guard (r >= nts)
     return $ Seq.index rs (r - nts)
  where
    rs  = Seq.fromList rules
    ts  = Foldable.fold [ IntSet.insert (i^.tableId) (tables b) | Rule (Atom i _) b <- rules ]
    nts = IntSet.size ts
    tm  = IntMap.fromList $ Prelude.zip (IntSet.toList ts) [0..]
    nrs = Prelude.length rules
    arr = accumArray (flip (:)) [] (0,nts+nrs-1) $ do
      (r, Rule (Atom i _) b) <- Prelude.zip [nts..] rules
      (tm IntMap.! (i^.tableId), r) : [ (r, tm IntMap.! j) | j <- IntSet.toList (tables b)]

bodyRows :: IntMap Relation -> Query a -> StateT Subst [] a
bodyRows db q = do
  q' <- prepare db q
  u <- use subst
  finish db u q'

prepare :: IntMap Relation -> Query a -> StateT Subst [] (Query a)
prepare db (Ap l r)        = Ap <$> prepare db l <*> prepare db r
prepare db (Map f x)       = Map f <$> prepare db x
prepare _  (Pure a)        = pure (Pure a)
prepare db (Alt l r)       = prepare db l <|> prepare db r
prepare _  Empty           = Ap.empty
prepare db (Row x)         = pure . snd <$> rows x db
prepare db (Value x)       = pure . fst <$> rows x db
prepare _  q               = return q

finish :: Alternative m => IntMap Relation -> Subst -> Query a -> m a
finish db u (Ap l r)  = finish db u l <*> finish db u r
finish db u (Map f x) = f <$> finish db u x
finish _  _ (Pure a)  = pure a
finish db u (No x)
  | Prelude.null $ runStateT (rows x db) u = pure ()
  | otherwise = Ap.empty
finish _  u (Key t) = case term `withArgType` t of
  IsEntity -> pure t
  IsVar -> case u^.mgu.at (Var t) of
    Just (AVar _) -> error "Variable 'key': You probably want to move it to the right of the query!"
    Just (AnEntity t') -> case cast t' of
      Just t'' -> pure t''
      Nothing -> error "finish: Mismatched type"
    Nothing -> error "Variable 'key': You probably want to move it to the right of the query!"
finish _  u (Filtering t p) = case term `withArgType` t of
  IsEntity
    | p t -> pure t
    | otherwise -> Ap.empty
  IsVar -> case u^.mgu.at (Var t) of
    Just (AVar _) -> error "Variable 'key': You probably want to move it to the right of the query!"
    Just (AnEntity t') -> case cast t' of
      Just t''
        | p t'' -> pure t''
        | otherwise -> Ap.empty
      Nothing -> error "finish: Mismatched type"
    Nothing -> error "Variable 'key': You probably want to move it to the right of the query!"
finish _ _ _ = error "wibble"

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

withArgType :: t a -> a -> t a
withArgType = const
