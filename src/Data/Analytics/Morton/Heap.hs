{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Morton.Heap
  ( Heap(..)
  , insertMin
  , extractMin
  ) where

import Control.Applicative
import Control.Lens
import Data.Analytics.Morton.Node
import Data.Foldable
import Data.Semigroup

------------------------------------------------------------------------------
-- Heap
------------------------------------------------------------------------------

-- | a non-empty min-pairing heap
data Heap a = Heap !(Node a) [Heap a]
  deriving Show

instance HasNode (Heap a) a where
  node f (Heap n cs) = (`Heap` cs) <$> f n
  {-# INLINE node #-}

instance Semigroup (Heap a) where
  x@(Heap m xs) <> y@(Heap n ys)
    | m <= n    = Heap m (y:xs)
    | otherwise = Heap n (x:ys)
  {-# INLINE (<>) #-}

instance Functor Heap where
  fmap f (Heap n ts) = Heap (fmap f n) (fmap f <$> ts)
  {-# INLINEABLE fmap #-}

instance Foldable Heap where
  foldMap f (Heap n ts) = foldMap f n `mappend` foldMap (foldMap f) ts
  {-# INLINEABLE foldMap #-}

instance Traversable Heap where
  traverse f (Heap n ts) = Heap <$> traverse f n <*> traverse (traverse f) ts
  {-# INLINEABLE traverse #-}

instance HasNodes Heap where
  nodes f (Heap n ts) = Heap <$> f n <*> traverse (nodes f) ts
  {-# INLINEABLE nodes #-}

insertMin :: Node a -> Heap a -> Heap a
insertMin n (Heap m xs)
  | n <= m    = Heap n [Heap m xs]
  | otherwise = Heap m (Heap n []:xs)
{-# INLINE insertMin #-}

extractMin :: Heap a -> (Node a, Maybe (Heap a))
extractMin (Heap n cs) = (,) n $! case cs of
  [] -> Nothing
  (t:ts) -> Just (meld t ts) where
    meld u []     = u
    meld u (s:ss) = meld (u <> s) ss
{-# INLINE extractMin #-}
