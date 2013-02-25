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
