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
  ( MinHeap(..)
  , insertMin
  , extractMin
  , MaxHeap(..)
  , insertMax
  , extractMax
  ) where

import Control.Applicative
import Control.Lens
import Data.Analytics.Morton.Node
import Data.Foldable
import Data.Semigroup

------------------------------------------------------------------------------
-- MinHeap
------------------------------------------------------------------------------

-- | a non-empty min-pairing heap
data MinHeap a = MinHeap !(Node a) [MinHeap a]
  deriving Show

instance HasNode (MinHeap a) a where
  node f (MinHeap n cs) = (`MinHeap` cs) <$> f n
  {-# INLINE node #-}

instance Semigroup (MinHeap a) where
  x@(MinHeap m xs) <> y@(MinHeap n ys)
    | m <= n    = MinHeap m (y:xs)
    | otherwise = MinHeap n (x:ys)
  {-# INLINE (<>) #-}

instance Functor MinHeap where
  fmap f (MinHeap n ts) = MinHeap (fmap f n) (fmap f <$> ts)
  {-# INLINEABLE fmap #-}

instance Foldable MinHeap where
  foldMap f (MinHeap n ts) = foldMap f n `mappend` foldMap (foldMap f) ts
  {-# INLINEABLE foldMap #-}

instance Traversable MinHeap where
  traverse f (MinHeap n ts) = MinHeap <$> traverse f n <*> traverse (traverse f) ts
  {-# INLINEABLE traverse #-}

instance HasNodes MinHeap where
  nodes f (MinHeap n ts) = MinHeap <$> f n <*> traverse (nodes f) ts
  {-# INLINEABLE nodes #-}

insertMin :: Node a -> MinHeap a -> MinHeap a
insertMin n (MinHeap m xs)
  | n <= m    = MinHeap n [MinHeap m xs]
  | otherwise = MinHeap m (MinHeap n []:xs)
{-# INLINE insertMin #-}

extractMin :: MinHeap a -> (Node a, Maybe (MinHeap a))
extractMin (MinHeap n cs) = (,) n $! case cs of
  [] -> Nothing
  (t:ts) -> Just (meld t ts) where
    meld u []     = u
    meld u (s:ss) = meld (u <> s) ss
{-# INLINE extractMin #-}

------------------------------------------------------------------------------
-- MaxHeap
------------------------------------------------------------------------------

-- | a non-empty max-pairing heap
data MaxHeap a = MaxHeap !(Node a) [MaxHeap a]
  deriving Show

instance HasNode (MaxHeap a) a where
  node f (MaxHeap n cs) = (`MaxHeap` cs) <$> f n
  {-# INLINE node #-}

instance Functor MaxHeap where
  fmap f (MaxHeap n cs) = MaxHeap (fmap f n) (fmap f <$> cs)
  {-# INLINEABLE fmap #-}

instance Foldable MaxHeap where
  foldMap f (MaxHeap n cs) = foldMap f n `mappend` foldMap (foldMap f) cs
  {-# INLINEABLE foldMap #-}

instance Traversable MaxHeap where
  traverse f (MaxHeap n cs) = MaxHeap <$> traverse f n <*> traverse (traverse f) cs
  {-# INLINEABLE traverse #-}

instance HasNodes MaxHeap where
  nodes f (MaxHeap n ts) = MaxHeap <$> f n <*> traverse (nodes f) ts
  {-# INLINEABLE nodes #-}

instance Semigroup (MaxHeap a) where
  x@(MaxHeap m xs) <> y@(MaxHeap n ys)
    | m >  n    = MaxHeap m (y:xs)
    | otherwise = MaxHeap n (x:ys)
  {-# INLINE (<>) #-}

insertMax :: Node a -> MaxHeap a -> MaxHeap a
insertMax n (MaxHeap m xs)
  | n >  m    = MaxHeap n [MaxHeap m xs]
  | otherwise = MaxHeap m (MaxHeap n []:xs)
{-# INLINE insertMax #-}

extractMax :: MaxHeap a -> (Node a, Maybe (MaxHeap a))
extractMax (MaxHeap n cs) = (,) n $! case cs of
  [] -> Nothing
  (t:ts) -> Just (meld t ts) where
    meld u []     = u
    meld u (s:ss) = meld (u <> s) ss
{-# INLINE extractMax #-}
