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
module Data.Analytics.Storage.Morton.Node
  ( Node(..)
  , HasNode(..)
  , HasNodes(..)
  ) where

import Control.Lens
import Data.Int
import Data.Semigroup

------------------------------------------------------------------------------
-- Nodes
------------------------------------------------------------------------------

data Node f = Node
  { _nodePriority :: {-# UNPACK #-} !Int -- ^ @lo + remaining * stride = hi@. This changes roles if we're going top down or bottom up
  , _nodeSequence :: {-# UNPACK #-} !Int -- ^ used to break ties deterministically
  , _nodeStride   :: {-# UNPACK #-} !Int
  , _nodeHi       :: {-# UNPACK #-} !Int
  , _nodeLo       :: {-# UNPACK #-} !Int -- ^ Data occupies @[nodeLo, nodeHi)@
  , _nodeBits     :: f Int64             -- ^ the bits. By storing it in @f@, we can reuse this machinery for simultaneously calculating \"don't care\" vectors, etc.
  }

deriving instance Show (f Int64) => Show (Node f)

makeClassy ''Node

instance Eq (Node f) where
  Node p s _ _ _ _ == Node q t _ _ _ _ = p == q && s == t
  {-# INLINE (==) #-}

instance Ord (Node f) where
  Node p s _ _ _ _ `compare` Node q t _ _ _ _ = compare p q <> compare s t
  {-# INLINE compare #-}

class HasNodes t where
  nodes :: Traversal (t f) (t g) (Node f) (Node g)
