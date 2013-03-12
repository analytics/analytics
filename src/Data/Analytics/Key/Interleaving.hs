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
-- This provides a generalized key interleaving system for
-- variable-length keys. For fixed-length keys this looks like the
-- Morton or Z-order.
--
-- TODO: Provide for Grey-coding for (fixed length parts of?) the
-- resulting schedule to get us a variable-length Hilbert ordering.
--------------------------------------------------------------------
module Data.Analytics.Key.Interleaving
  ( Interleaving(..)
  , interleaving
  , interleavingBy
  ) where

import Control.Applicative
import Control.Lens
import Data.Analytics.Approximate.Type
import Data.Analytics.Key.Heap
import Data.Analytics.Key.Node
import Data.Analytics.Key.Schedule
import Data.Data
import Data.Foldable
import Data.Semigroup

------------------------------------------------------------------------------
-- Ordered Interleaving
------------------------------------------------------------------------------

-- | This provides a priority queue with a fixed schedule that
-- can be used to obtain an interleaved sequence of bits from multiple
-- sources. Use the monoid to fairly interleave breaking ties to the left.
data Interleaving a
  = Z
  | Interleaving
    {-# UNPACK #-} !(Approximate Int) -- expected bits remaining
    {-# UNPACK #-} !Int               -- index count
    !(Heap a)                         -- top down
  deriving (Typeable, Data, Functor, Foldable, Traversable)

instance Semigroup (Interleaving a) where
  (<>) = mappend
  {-# INLINE (<>) #-}

instance Monoid (Interleaving a) where
  mempty = Z
  {-# INLINE mempty #-}
  mappend Z m = m
  mappend m Z = m
  mappend (Interleaving n i t) (Interleaving m j t')
    = Interleaving (n + m) (i + j) (t <> (t' & nodes.nodeSequence +~ i))
  {-# INLINE mappend #-}

instance (p ~ (->), Applicative f, Gettable f) => Cons p f (Interleaving a) (Interleaving b) a b where
  _Cons _ Z = pure Z
  _Cons f (Interleaving k i (Heap (Node np ns nw nb nbs) ts0)) = coerce $ f $ (,) nb $
    case nbs of
      [] -> case ts0 of
        [] -> Z
        (t:ts) -> Interleaving (k - 1) i $ meldWithHeap t ts
      (b:bs) -> Interleaving (k - 1) i $ meldWithNode (Node (np + nw) ns nw b bs) ts0
    where
       meldWithNode n (t2:ts) = insertMin n (meldWithHeap t2 ts)
       meldWithNode n []      = Heap n []
       meldWithHeap t (t2:ts) = t <> meldWithHeap t2 ts
       meldWithHeap t []      = t
  {-# INLINE _Cons #-}

interleavingBy :: Schedule a -> [b] -> Interleaving b
interleavingBy _ [] = mempty
interleavingBy (Schedule p w c _ _ _) (b:bs) = Interleaving c 1 (Heap (Node p 0 w b bs) [])
{-# INLINE interleavingBy #-}

interleaving :: Schedule a -> a -> Interleaving Bool
interleaving s a = interleavingBy s (view scheduleEncoder s a)
{-# INLINE interleaving #-}
