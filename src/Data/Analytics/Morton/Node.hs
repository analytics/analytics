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
module Data.Analytics.Morton.Node
  ( Node(Node)
  , HasNode(..)
  , HasNodes(..)
  ) where

import Control.Lens
import Data.Foldable
import Data.Semigroup

------------------------------------------------------------------------------
-- Nodes
------------------------------------------------------------------------------

data Node a = Node
  { _nodePriority :: {-# UNPACK #-} !Int -- ^ @lo + remaining * stride = hi@. This changes roles if we're going top down or bottom up
  , _nodeSequence :: {-# UNPACK #-} !Int -- ^ used to break ties deterministically
  , _nodeStride   :: {-# UNPACK #-} !Int
  , _nodeHi       :: {-# UNPACK #-} !Int
  , _nodeLo       :: {-# UNPACK #-} !Int -- ^ Data occupies @[nodeLo, nodeHi)@
  , _nodeBits     :: a                   -- ^ the bits. By storing it in @f@, we can reuse this machinery for simultaneously calculating \"don't care\" vectors, etc.
  } deriving (Show,Functor,Foldable,Traversable)

makeClassy ''Node

instance Eq (Node a) where
  Node p s _ _ _ _ == Node q t _ _ _ _ = p == q && s == t
  {-# INLINE (==) #-}

instance Ord (Node a) where
  Node p s _ _ _ _ `compare` Node q t _ _ _ _ = compare p q <> compare s t
  {-# INLINE compare #-}

class HasNodes t where
  nodes :: Traversal (t a) (t b) (Node a) (Node b)
