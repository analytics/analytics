{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------
-- |
-- Module    :  Analytics.Match
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Analytics.Match
  ( Match(..)
  ) where

import Control.Applicative
import Control.Lens
import Data.Traversable as T
import Generics.Deriving

------------------------------------------------------------------------------
-- Matching
------------------------------------------------------------------------------

-- | Simple flat unification
class Traversable t => Match t where
  -- | Use 'Generic1' by default
  --
  -- @default 'match' :: ('Generic1' t, 'GMatch' ('Rep1' t)) => (a -> b -> c) -> t a -> t b -> 'Maybe' (t c)@
  match :: (a -> b -> c) -> t a -> t b -> Maybe (t c)
  default match :: (Generic1 t, GMatch (Rep1 t)) => (a -> b -> c) -> t a -> t b -> Maybe (t c)
  match abc ta tb = to1 <$> gmatch abc (from1 ta) (from1 tb)
  {-# INLINE match #-}

------------------------------------------------------------------------------
-- Generic Matching
------------------------------------------------------------------------------

-- | Enable matching via 'Generic' programming.
class GMatch t where
  gmatch :: (a -> b -> c) -> t a -> t b -> Maybe (t c)

instance GMatch U1 where
  gmatch _ U1 U1 = Just U1
  {-# INLINE gmatch #-}

instance GMatch Par1 where
  gmatch f (Par1 a) (Par1 b) = Just $ Par1 (f a b)
  {-# INLINE gmatch #-}

instance Eq c => GMatch (K1 i c) where
  gmatch _ (K1 a) (K1 b)
    | a == b    = Just (K1 a)
    | otherwise = Nothing
  {-# INLINE gmatch #-}

instance GMatch f => GMatch (Rec1 f) where
  gmatch f (Rec1 a) (Rec1 b) = Rec1 <$> gmatch f a b
  {-# INLINE gmatch #-}

instance (GMatch f, GMatch g) => GMatch (f :+: g) where
  gmatch f (L1 a) (L1 b) = L1 <$> gmatch f a b
  gmatch f (R1 a) (R1 b) = R1 <$> gmatch f a b
  gmatch _ _      _      = Nothing
  {-# INLINE gmatch #-}

instance (GMatch f, GMatch g) => GMatch (f :*: g) where
  gmatch f (a :*: b) (c :*: d) = (:*:) <$> gmatch f a c <*> gmatch f b d
  {-# INLINE gmatch #-}

instance (Match f, GMatch g) => GMatch (f :.: g) where
  gmatch f (Comp1 x) (Comp1 y) = do
    ms <- match (gmatch f) x y
    Comp1 <$> T.sequence ms
  {-# INLINE gmatch #-}

