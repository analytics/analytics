{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
--------------------------------------------------------------------
-- |
-- Module    :  Analytics.Unification
-- Copyright :  (c) Edward Kmett 2011-2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Analytics.Unification
  ( MetaT, TermM
  , unify
  , unifyVar
  , occurs
  , generalize
  ) where

import Bound
import Control.Applicative
import Control.Arrow hiding ((<+>))
import Control.Lens
import Control.Monad.ST
import Control.Monad.ST.Class
import Control.Monad.State.Strict
import Control.Monad.Writer.Strict
import Data.IntMap as IntMap
import Data.IntSet as IntSet
import Data.Set as Set
import Data.STRef
import Data.Traversable as Traversable
import Analytics.Diagnostic
import Analytics.Pretty
import Analytics.Term
import Analytics.Meta

-- $setup
-- >>> :set -XFlexibleContexts -XConstraintKinds -XTypeFamilies -XRankNTypes -XOverloadedStrings
-- >>> import Analytics.Syntax
-- >>> import Text.Trifecta.Rendering
-- >>> let test :: (forall m s. (MonadWriter Any m, MonadMeta s m) => m (TermM s)) -> Scope Int Term b; test mk = head $ snd $ fst $ runM_ emptyRendering (runWriterT (mk >>= generalize . return))

-- | A term meta-variable
type MetaT s = Meta s Term

-- | A term filled with meta-variables
type TermM s = Term (MetaT s)

data Occ = Occ { _occVar :: IntMap String, _occFresh :: [String] }
makeLenses ''Occ

-- | Die with an error message due to a cycle between the specified terms.
--
-- >>> test $ do k <- Var <$> newMeta; unify mempty k (Struct "Foo" [k])
-- Scope *** Exception: (interactive):1:1: error: infinite term detected
-- cyclic term: a = Foo a
occurs :: MonadMeta s m => Set (MetaT s) -> m a
occurs zs = evalStateT ?? Occ mempty names $ do
  ns  <- for (Set.toList zs) $ \z -> occVar.at (z^.metaId).non "" <<~ occFresh %%= (head &&& tail)
  rhs <- for (Set.toList zs) $ \z -> lift (readMeta z) >>= \mk -> case mk of
    Nothing -> fail "the impossible happened: unbound cyclic term-meta-variable"
    Just k  -> prettyTerm k walk
  let docs = zipWith ?? ns ?? rhs $ \n r -> text "cyclic term:" <+> hang 4 (group (pretty n </> char '=' </> r))
  r <- view rendering
  let msg | length ns == 1 = "infinite term detected"
          | otherwise      = "infinite terms detected"
  throwM $ die r msg & footnotes .~ docs
  where
    -- walk :: MetaT s -> StateT Occ (M s) TermDoc
    walk v | zs^.contains v = unbound v
    walk v = readMeta v >>= \mk -> case mk of
      Nothing -> unbound v
      Just k -> prettyTerm k walk

    -- unbound :: MetaT s -> StateT Occ (M s) TermDoc
    unbound v | i <- v^.metaId = use (occVar.at i) >>= \ mn -> case mn of
      Just n -> return $ pretty n
      Nothing -> do
        n <- occVar.at i.non "" <<~ occFresh %%= (head &&& tail)
        return $ pretty n

-- | Returns the a unified form if different from the left argument.
--
-- The writer is used to track if any interesting edits have been made
unify :: (MonadMeta s m, MonadWriter Any m) => IntSet -> TermM s -> TermM s -> m (TermM s)
unify is k1 k2 = do
  k1' <- semiprune k1
  k2' <- semiprune k2
  go k1' k2'
  where
    go k@(Var kv1) (Var kv2) | kv1 == kv2 = return k -- boring
    go a@(Var (Meta i r u)) b@(Var (Meta j s v)) = do
      -- union-by-rank
      m <- liftST $ readSTRef u
      n <- liftST $ readSTRef v
      case compare m n of
        LT -> unifyTV is True i r b $ return ()
        EQ -> unifyTV is True i r b $ writeSTRef v $! n + 1
        GT -> unifyTV is False j s a $ return ()
    go (Var (Meta i r _)) k    = unifyTV is True i r k $ return ()
    go k (Var (Meta i r _))    = unifyTV is False i r k $ return ()
    go (Struct x xs) (Struct y ys) | x == y = Struct x <$> Traversable.sequence (zipWith (unify is) xs ys)
    go _ _ = fail "term mismatch"
{-# INLINE unify #-}

-- | We don't need to update depths in the term variables. We only create
-- meta variables with non-rankInf rank for annotations, and annotations do
-- not, at least at this time, bind terms.
unifyTV :: (MonadMeta s m, MonadWriter Any m) => IntSet -> Bool -> Int -> STRef s (Maybe (TermM s)) -> TermM s -> ST s () -> m (TermM s)
unifyTV is interesting i r k bump = liftST (readSTRef r) >>= \mb -> case mb of
  Just j | is^.contains i  -> cycles is j >>= occurs
         | otherwise       -> do
    (k', Any m) <- listen $ unify (IntSet.insert i is) j k
    if m then liftST $ k' <$ writeSTRef r (Just k') -- write back interesting changes
         else j <$ tell (Any True)                  -- short-circuit
  Nothing -> do
    tell (Any interesting)
    liftST $ do
      bump
      k <$ writeSTRef r (Just k)

-- | Unify a known 'Meta' variable with a term that isn't a 'Var'.
unifyVar :: (MonadMeta s m, MonadWriter Any m) => IntSet -> MetaT s -> TermM s -> m (TermM s)
unifyVar is (Meta i r _) kv = unifyTV is True i r kv $ return ()
unifyVar _  (Skolem _) _  = fail "unifyVar: Skolem"
{-# INLINE unifyVar #-}

-- | Generalize terms, checking for escaped Skolems.
generalize :: MonadMeta s m => [TermM s] -> m (Int, [Scope Int Term a])
generalize ks0 = do
  ks <- traverse (zonk mempty ?? occurs) ks0
  (rs,(_,n)) <- runStateT (traverse (traverse go) ks) (IntMap.empty, 0)
  return (n , Scope <$> rs)
  where
   go (Skolem _)   = StateT $ \ _ -> fail "escaped skolem"
   go (Meta i _ _) = StateT $ \imn@(im, n) -> case im^.at i of
     Just b  -> return (B b, imn)
     Nothing -> let n' = n + 1 in n' `seq` return (B n, (im & at i ?~ n, n'))
