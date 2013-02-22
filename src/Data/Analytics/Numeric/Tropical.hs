{-# LANGUAGE ForeignFunctionInterface, DeriveDataTypeable, DeriveGeneric #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Numeric.Tropical
  ( Tropical(..)
  ) where

import Control.Applicative
import Control.Comonad
import Data.Foldable
import Data.Functor.Apply
import Data.Traversable
import Data.Data
import Generics.Deriving

-- | The Tropical semiring.
--
-- <http://ncatlab.org/nlab/show/tropical+semiring>
newtype Tropical a = Tropical a deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

runTropical :: Tropical a -> a
runTropical (Tropical a) = a
{-# INLINE runTropical #-}

instance Functor Tropical where
  fmap f (Tropical a) = Tropical (f a)
  {-# INLINE fmap #-}

instance Foldable Tropical where
  foldMap f (Tropical a) = f a
  {-# INLINE foldMap #-}

instance Traversable Tropical where
  traverse f (Tropical a) = Tropical <$> f a
  {-# INLINE traverse #-}

instance Comonad Tropical where
  extract (Tropical a) = a
  {-# INLINE extract #-}
  extend f w@(Tropical _) = Tropical (f w)
  {-# INLINE extend #-}

instance Applicative Tropical where
  pure = Tropical
  {-# INLINE pure #-}
  Tropical f <*> Tropical a = Tropical (f a)
  {-# INLINE (<*>) #-}

instance Apply Tropical where
  Tropical f <.> Tropical a = Tropical (f a)
  {-# INLINE (<.>) #-}

instance Monad Tropical where
  return = Tropical
  {-# INLINE return #-}
  Tropical a >>= f = f a
  {-# INLINE (>>=) #-}

instance (Fractional a, Ord a) => Num (Tropical a) where
  Tropical a * Tropical b = Tropical (a + b)
  {-# INLINE (*) #-}
  Tropical a + Tropical b = Tropical (max a b)
  {-# INLINE (+) #-}
  a - _ = a
  {-# INLINE (-) #-}
  signum (Tropical a) = Tropical (signum a)
  {-# INLINE signum #-}
  negate _ = error "Tropical.negate"
  -- negate _ = minBound
  {-# INLINE negate #-}
  abs (Tropical a) = Tropical (abs a)
  {-# INLINE abs #-}
  fromInteger = Tropical . fromInteger
  {-# INLINE fromInteger #-}

instance (Fractional a, Ord a) => Fractional (Tropical a) where
  -- n/0 == infinity is handled seamlessly for us. We must catch 0/0 and infinity/infinity NaNs, and handle 0/infinity.
  Tropical a / Tropical b = Tropical (a - b)
  {-# INLINE (/) #-}
  fromRational = Tropical . fromRational -- not what you'd expect from ('/'), but what you'd expect from 'fromInteger'
  {-# INLINE fromRational #-}

instance (Fractional a, Real a) => Real (Tropical a) where
  toRational (Tropical a) = toRational a
  {-# INLINE toRational #-}

{-# RULES
"realToFrac" realToFrac = Tropical . realToFrac . runTropical :: Tropical Double -> Tropical Float
"realToFrac" realToFrac = Tropical . realToFrac . runTropical :: Tropical Float -> Tropical Double
"realToFrac" realToFrac = runTropical :: Tropical Double -> Double
"realToFrac" realToFrac = runTropical :: Tropical Float -> Float
"realToFrac" realToFrac = Tropical    :: Double -> Tropical Double
"realToFrac" realToFrac = Tropical    :: Float -> Tropical Float #-}
