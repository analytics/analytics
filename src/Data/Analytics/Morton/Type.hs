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
#ifndef MIN_VERSION_lens
#define MIN_VERSION_lens(x,y,z) 1
#endif

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
#if !(MIN_VERSION_lens(3,9,0))
import Data.Functor.Contravariant
#endif
import Data.Semigroup
import Data.Void

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
    !(MinHeap (f (Int -> Bool))) -- top down
    !(MaxHeap (f (Int -> Bool))) -- bottom up

instance Semigroup (Morton f) where
  (<>) = mappend
  {-# INLINE (<>) #-}

instance Monoid (Morton f) where
  mempty = Z
  {-# INLINE mempty #-}
  mappend Z m = m
  mappend m Z = m
  mappend (Morton n i t b) (Morton m j t' b')
    = Morton (n + m) (i + j) (t <> (t' & nodes.nodeSequence +~ i))
                             (b <> (b' & nodes.nodeSequence +~ i))
  {-# INLINE mappend #-}

instance (p ~ (->), Applicative f, Contravariant f, Functor g, Functor h) => Cons p f (Morton g) (Morton h) (g Bool) (h Bool) where
  _Cons _ Z = pure Z
  _Cons f (Morton k i (MinHeap (Node np ns nw nh nl nd) ts0) b) = coerce' $ f
     ( fmap ($ nhm1) nd
     , if k == 1 then Z else Morton (k - 1) i t' b
     ) where
       !nhm1 = nh - 1
       t' | nhm1 == nl = case ts0 of
            []     -> error "_Cons: the impossible happened"
            (t:ts) -> meldWithHeap t ts
          | otherwise = meldWithNode ts0
       meldWithNode (t2:ts) = insertMin (Node (np + nw) ns nw nhm1 nl nd) (meldWithHeap t2 ts)
       meldWithNode []      = MinHeap (Node (np + nw) ns nw nhm1 nl nd) []
       meldWithHeap t (t2:ts) = t <> meldWithHeap t2 ts
       meldWithHeap t []      = t
  {-# INLINE _Cons #-}

instance (p ~ (->), Applicative f, Contravariant f, Functor g, Functor h) => Snoc p f (Morton g) (Morton h) (g Bool) (h Bool) where
  _Snoc _ Z = pure Z
  _Snoc f (Morton k i b (MaxHeap (Node np ns nw nh nl nd) ts0)) = coerce' $ f
     ( if k == 1 then Z else Morton (k - 1) i b t'
     , fmap ($ nl) nd
     ) where
       !nlp1 = nl + 1
       t' | nlp1 == nh = case ts0 of
            []     -> error "_Snoc: the impossible happened"
            (t:ts) -> meldWithHeap t ts
          | otherwise = meldWithNode ts0
       meldWithNode (t2:ts) = insertMax (Node (np - nw) ns nw nh nlp1 nd) (meldWithHeap t2 ts)
       meldWithNode []      = MaxHeap (Node (np - nw) ns nw nh nlp1 nd) []
       meldWithHeap t (t2:ts) = t <> meldWithHeap t2 ts
       meldWithHeap t []      = t
  {-# INLINE _Snoc #-}

morton64 :: Functor f => Schedule a -> f (Int -> Bool) -> Morton f
morton64 (Schedule p w c _ _ _) fi
  = Morton c 1 (MinHeap (Node p             0 w c 0 fi) [])
               (MaxHeap (Node (p + w*(c-1)) 0 w c 0 fi) [])
{-# INLINE morton64 #-}

morton :: Functor f => Schedule a -> f a -> Morton f
morton (Schedule p w c _ _ f) fa
  = Morton c 1 (MinHeap (Node p             0 w c 0 fi) [])
               (MaxHeap (Node (p + w*(c-1)) 0 w c 0 fi) [])
  where fi = fmap f fa
{-# INLINE morton #-}

coerce' :: (Contravariant f, Functor f) => f a -> f b
coerce' a = absurd <$> contramap absurd a
{-# INLINE coerce' #-}
