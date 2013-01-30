{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DefaultSignatures #-}
module Analytics.Match
  ( Match(..)
  ) where

import Control.Applicative
import Data.Traversable as T
import Generics.Deriving

class Traversable t => Match t where
  match :: (a -> b -> c) -> t a -> t b -> Maybe (t c)
  default match :: (Generic1 t, GMatch (Rep1 t)) => (a -> b -> c) -> t a -> t b -> Maybe (t c)
  match abc ta tb = to1 <$> gmatch abc (from1 ta) (from1 tb)

class GMatch t where
  gmatch :: (a -> b -> c) -> t a -> t b -> Maybe (t c)

instance GMatch U1 where
  gmatch _ U1 U1 = Just U1

instance GMatch Par1 where
  gmatch f (Par1 a) (Par1 b) = Just $ Par1 (f a b)

instance Eq c => GMatch (K1 i c) where
  gmatch _ (K1 a) (K1 b)
    | a == b    = Just (K1 a)
    | otherwise = Nothing

instance GMatch f => GMatch (Rec1 f) where
  gmatch f (Rec1 a) (Rec1 b) = Rec1 <$> gmatch f a b

instance (GMatch f, GMatch g) => GMatch (f :+: g) where
  gmatch f (L1 a) (L1 b) = L1 <$> gmatch f a b
  gmatch f (R1 a) (R1 b) = R1 <$> gmatch f a b
  gmatch _ _      _      = Nothing

instance (GMatch f, GMatch g) => GMatch (f :*: g) where
  gmatch f (a :*: b) (c :*: d) = (:*:) <$> gmatch f a c <*> gmatch f b d

instance (Match f, GMatch g) => GMatch (f :.: g) where
  gmatch f (Comp1 x) (Comp1 y) = do
    ms <- match (gmatch f) x y
    Comp1 <$> T.sequence ms

