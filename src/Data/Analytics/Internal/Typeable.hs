--------------------------------------------------------------------
-- |
-- Module    :  Data.Analytics.Internal.Typeable
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: GHC Typeable
--
--------------------------------------------------------------------
module Data.Analytics.Internal.Typeable
  ( cast1
  ) where

import Data.Functor.Identity
import Data.Typeable

cast1 :: (Typeable1 f, Typeable1 g) => f a -> Maybe (g a)
cast1 = fmap runIdentity . gcast1 . Identity
{-# INLINE cast1 #-}
