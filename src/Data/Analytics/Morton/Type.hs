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
import Data.Analytics.Morton.Heap
import Data.Analytics.Morton.Node
import Data.Analytics.Morton.Schedule
import Data.Semigroup

------------------------------------------------------------------------------
-- Morton Ordering
------------------------------------------------------------------------------

-- | This provides a double-ended priority queue with a fixed schedule that
-- can be used to obtain a generalized morton sequence of bits from multiple
-- sources. Use the monoid to fairly interleave breaking ties to the left.
data Morton f
  = Z
  | Morton
    {-# UNPACK #-} !Int -- bits remaining
    {-# UNPACK #-} !Int -- index count
    !(Heap (f (Int -> Bool))) -- top down

instance Semigroup (Morton f) where
  (<>) = mappend
  {-# INLINE (<>) #-}

instance Monoid (Morton f) where
  mempty = Z
  {-# INLINE mempty #-}
  mappend Z m = m
  mappend m Z = m
  mappend (Morton n i t) (Morton m j t')
    = Morton (n + m) (i + j) (t <> (t' & nodes.nodeSequence +~ i))
  {-# INLINE mappend #-}

instance (p ~ (->), Applicative f, Gettable f, Functor g, Functor h) => Cons p f (Morton g) (Morton h) (g Bool) (h Bool) where
  _Cons _ Z = pure Z
  _Cons f (Morton k i (Heap (Node np ns nw nh nl nd) ts0)) = coerce $ f
     ( fmap ($ nhm1) nd
     , if k == 1 then Z else Morton (k - 1) i t'
     ) where
       !nhm1 = nh - 1
       t' | nhm1 == nl = case ts0 of
            []     -> error "_Cons: the impossible happened"
            (t:ts) -> meldWithHeap t ts
          | otherwise = meldWithNode ts0
       meldWithNode (t2:ts) = insertMin (Node (np + nw) ns nw nhm1 nl nd) (meldWithHeap t2 ts)
       meldWithNode []      = Heap (Node (np + nw) ns nw nhm1 nl nd) []
       meldWithHeap t (t2:ts) = t <> meldWithHeap t2 ts
       meldWithHeap t []      = t
  {-# INLINE _Cons #-}

morton64 :: Functor f => Schedule a -> f (Int -> Bool) -> Morton f
morton64 (Schedule p w c _ _ _) fi
  = Morton c 1 (Heap (Node p             0 w c 0 fi) [])
{-# INLINE morton64 #-}

morton :: Functor f => Schedule a -> f a -> Morton f
morton (Schedule p w c _ _ f) fa
  = Morton c 1 (Heap (Node p             0 w c 0 fi) [])
  where fi = fmap f fa
{-# INLINE morton #-}
