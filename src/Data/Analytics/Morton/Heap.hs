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
module Data.Analytics.Morton.Heap
  ( MinHeap(..), insertMin
  , MaxHeap(..), insertMax
  ) where

import Control.Applicative
import Data.Analytics.Morton.Node
import Control.Lens
import Data.Int
import Data.Semigroup

------------------------------------------------------------------------------
-- MinHeap
------------------------------------------------------------------------------

-- | a non-empty min-pairing heap
data MinHeap f = MinHeap (Node f) [MinHeap f]

deriving instance Show (f Int64) => Show (MinHeap f)

instance HasNode (MinHeap f) f where
  node f (MinHeap n cs) = (`MinHeap` cs) <$> f n

instance Semigroup (MinHeap f) where
  x@(MinHeap m xs) <> y@(MinHeap n ys)
    | m <= n    = MinHeap m (y:xs)
    | otherwise = MinHeap n (x:ys)
  {-# INLINE (<>) #-}

instance HasNodes MinHeap where
  nodes f (MinHeap n ts) = MinHeap <$> f n <*> traverse (nodes f) ts
  {-# INLINEABLE nodes #-}

insertMin :: Node f -> MinHeap f -> MinHeap f
insertMin n (MinHeap m xs)
  | n <= m    = MinHeap n [MinHeap m xs]
  | otherwise = MinHeap m (MinHeap n []:xs)
{-# INLINE insertMin #-}

------------------------------------------------------------------------------
-- MaxHeap
------------------------------------------------------------------------------

-- | a non-empty max-pairing heap
data MaxHeap f = MaxHeap (Node f) [MaxHeap f]

deriving instance Show (f Int64) => Show (MaxHeap f)

instance HasNode (MaxHeap f) f where
  node f (MaxHeap n cs) = (`MaxHeap` cs) <$> f n

instance HasNodes MaxHeap where
  nodes f (MaxHeap n ts) = MaxHeap <$> f n <*> traverse (nodes f) ts
  {-# INLINEABLE nodes #-}

instance Semigroup (MaxHeap f) where
  x@(MaxHeap m xs) <> y@(MaxHeap n ys)
    | m >  n    = MaxHeap m (y:xs)
    | otherwise = MaxHeap n (x:ys)
  {-# INLINE (<>) #-}

insertMax :: Node f -> MaxHeap f -> MaxHeap f
insertMax n (MaxHeap m xs)
  | n >  m    = MaxHeap n [MaxHeap m xs]
  | otherwise = MaxHeap m (MaxHeap n []:xs)
{-# INLINE insertMax #-}
