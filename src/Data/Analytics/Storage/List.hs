{-# LANGUAGE DataKinds, PolyKinds, GADTs, TypeOperators, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, KindSignatures #-}
module Data.Analytics.Storage.List
  ( List(..)
  ) where

import Control.Lens

-- | An HList.
data List f (as :: [*]) where
  Cons :: f a -> List f as -> List f (a ': as)
  Nil  :: List f '[]

-- This instance requires lens HEAD/3.9
instance (Profunctor p, Functor f) => Cons p f (List g (a ': as)) (List g as) (List h (b ': bs)) (List h bs) (g a) (h b) where
  _Cons = iso (\(Cons x xs) -> (x,xs)) (\(x,xs) -> Cons x xs)
  {-# INLINE _Cons #-}
