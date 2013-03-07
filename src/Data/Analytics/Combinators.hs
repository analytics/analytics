--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Combinators
  ( andThen
  , orElse
  ) where

andThen :: Monad m => m Bool -> m Bool -> m Bool
andThen m n = m >>= \a -> if a then n else return a
{-# INLINE andThen #-}

orElse :: Monad m => m Bool -> m Bool -> m Bool
orElse m n = m >>= \a -> if a then return a else n
{-# INLINE orElse #-}
