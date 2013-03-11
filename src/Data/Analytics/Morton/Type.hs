{-# LANGUAGE CPP #-}
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
{-# LANGUAGE DeriveDataTypeable #-}
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
module Data.Analytics.Morton.Type
  ( Morton(..)
  , morton
  , morton64
  ) where

import Control.Applicative
import Control.Lens
import Data.Analytics.Approximate.Type
import Data.Analytics.Morton.Heap
import Data.Analytics.Morton.Node
import Data.Analytics.Morton.Schedule
import Data.Data
import Data.Foldable
import Data.Semigroup

------------------------------------------------------------------------------
-- Morton Ordering
------------------------------------------------------------------------------

-- | This provides a double-ended priority queue with a fixed schedule that
-- can be used to obtain a generalized morton sequence of bits from multiple
-- sources. Use the monoid to fairly interleave breaking ties to the left.
data Morton a
  = Z
  | Morton
    {-# UNPACK #-} !(Approximate Int) -- expected bits remaining
    {-# UNPACK #-} !Int               -- index count
    !(Heap a)                         -- top down
  deriving (Typeable, Data, Functor, Foldable, Traversable)

instance Semigroup (Morton a) where
  (<>) = mappend
  {-# INLINE (<>) #-}

instance Monoid (Morton a) where
  mempty = Z
  {-# INLINE mempty #-}
  mappend Z m = m
  mappend m Z = m
  mappend (Morton n i t) (Morton m j t')
    = Morton (n + m) (i + j) (t <> (t' & nodes.nodeSequence +~ i))
  {-# INLINE mappend #-}

instance (p ~ (->), Applicative f, Gettable f) => Cons p f (Morton a) (Morton b) a b where
  _Cons _ Z = pure Z
  _Cons f (Morton k i (Heap (Node np ns nw nb nbs) ts0)) = coerce $ f $ (,) nb $
    case nbs of
      [] -> case ts0 of
        [] -> Z
        (t:ts) -> Morton (k - 1) i $ meldWithHeap t ts
      (b:bs) -> Morton (k - 1) i $ meldWithNode (Node (np + nw) ns nw b bs) ts0
    where
       meldWithNode n (t2:ts) = insertMin n (meldWithHeap t2 ts)
       meldWithNode n []      = Heap n []
       meldWithHeap t (t2:ts) = t <> meldWithHeap t2 ts
       meldWithHeap t []      = t
  {-# INLINE _Cons #-}

morton64 :: Schedule a -> [b] -> Morton b
morton64 _ [] = mempty
morton64 (Schedule p w c _ _ _) (b:bs) = Morton c 1 (Heap (Node p 0 w b bs) [])
{-# INLINE morton64 #-}

morton :: Schedule a -> a -> Morton Bool
morton s a = morton64 s (view scheduleEncoder s a)
{-# INLINE morton #-}
