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
import Data.Semigroup

------------------------------------------------------------------------------
-- Heap
------------------------------------------------------------------------------

-- | a non-empty min-pairing heap
data Heap = Heap !Node [Heap]
  deriving Show

instance HasNode Heap where
  node f (Heap n cs) = (`Heap` cs) <$> f n
  {-# INLINE node #-}

instance Semigroup Heap where
  x@(Heap m xs) <> y@(Heap n ys)
    | m <= n    = Heap m (y:xs)
    | otherwise = Heap n (x:ys)
  {-# INLINE (<>) #-}

instance HasNodes Heap where
  nodes f (Heap n ts) = Heap <$> f n <*> traverse (nodes f) ts
  {-# INLINEABLE nodes #-}

insertMin :: Node -> Heap -> Heap
insertMin n (Heap m xs)
  | n <= m    = Heap n [Heap m xs]
  | otherwise = Heap m (Heap n []:xs)
{-# INLINE insertMin #-}

extractMin :: Heap -> (Node, Maybe Heap)
extractMin (Heap n cs) = (,) n $! case cs of
  [] -> Nothing
  (t:ts) -> Just (meld t ts) where
    meld u []     = u
    meld u (s:ss) = meld (u <> s) ss
{-# INLINE extractMin #-}
