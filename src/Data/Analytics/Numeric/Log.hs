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
module Data.Analytics.Numeric.Log
  ( Log(..)
  , Precise(..)
  ) where

import Control.Applicative
import Control.Comonad
import Data.Complex
import Data.Foldable
import Data.Functor.Apply
import Data.Traversable
import Data.Data
import Generics.Deriving

-- | @data Log@ has no connection to 'Data.Analytics.Datalog'.
--
-- Errors should be resolved by taking the answer to negative infinity where possible
-- as this is typically used as the lower bound on a confidence level.
newtype Log a = Log a deriving (Eq,Ord,Show,Read,Data,Typeable,Generic)

runLog :: Log a -> a
runLog (Log a) = a
{-# INLINE runLog #-}

instance Functor Log where
  fmap f (Log a) = Log (f a)
  {-# INLINE fmap #-}

instance Foldable Log where
  foldMap f (Log a) = f a
  {-# INLINE foldMap #-}

instance Traversable Log where
  traverse f (Log a) = Log <$> f a
  {-# INLINE traverse #-}

instance Comonad Log where
  extract (Log a) = a
  {-# INLINE extract #-}
  extend f w@(Log _) = Log (f w)
  {-# INLINE extend #-}

instance Applicative Log where
  pure = Log
  {-# INLINE pure #-}
  Log f <*> Log a = Log (f a)
  {-# INLINE (<*>) #-}

instance Apply Log where
  Log f <.> Log a = Log (f a)
  {-# INLINE (<.>) #-}

instance Monad Log where
  return = Log
  {-# INLINE return #-}
  Log a >>= f = f a
  {-# INLINE (>>=) #-}

negInf :: Fractional a => a
negInf = -(1/0)
{-# INLINE negInf #-}

instance (Precise a, RealFloat a) => Num (Log a) where
  Log a * Log b
    | isInfinite a && isInfinite b && a == -b = Log negInf
    | otherwise = Log (a + b)
  {-# INLINE (*) #-}
  Log a + Log b
    | a == b && isInfinite a && isInfinite b = Log a
    | a >= b    = Log (a + log1p (exp (b - a)))
    | otherwise = Log (b + log1p (exp (a - b)))
  {-# INLINE (+) #-}
  Log a - Log b
    | a == negInf && b == negInf = Log negInf
    | otherwise = Log (a + log1p (negate (exp (b - a))))
  {-# INLINE (-) #-}
  signum (Log a)
    | a == negInf = 0
    | a > negInf  = 1
    | otherwise   = negInf
  {-# INLINE signum #-}
  negate _ = negInf
  {-# INLINE negate #-}
  abs = id
  {-# INLINE abs #-}
  fromInteger = Log . log . fromInteger
  {-# INLINE fromInteger #-}

instance (Precise a, RealFloat a, Eq a) => Fractional (Log a) where
  -- n/0 == infinity is handled seamlessly for us. We must catch 0/0 and infinity/infinity NaNs, and handle 0/infinity.
  Log a / Log b
    | a == b && isInfinite a && isInfinite b = Log negInf
    | a == negInf                            = Log negInf
    | otherwise                              = Log (a-b)
  {-# INLINE (/) #-}
  fromRational = Log . log . fromRational
  {-# INLINE fromRational #-}

instance (Precise a, RealFloat a, Ord a) => Real (Log a) where
  toRational (Log a) = toRational (exp a)
  {-# INLINE toRational #-}

{-# RULES
"realToFrac" realToFrac = Log . realToFrac . runLog :: Log Double -> Log Float
"realToFrac" realToFrac = Log . realToFrac . runLog :: Log Float -> Log Double
"realToFrac" realToFrac = exp . runLog :: Log Double -> Double
"realToFrac" realToFrac = exp . runLog :: Log Float -> Float
"realToFrac" realToFrac = Log . log :: Double -> Log Double
"realToFrac" realToFrac = Log . log :: Float -> Log Float #-}

class Floating a => Precise a where
  log1p :: a -> a
  expm1 :: a -> a

instance Precise Double where
  log1p = c_log1p
  {-# INLINE log1p #-}
  expm1 = c_expm1
  {-# INLINE expm1 #-}

instance Precise Float where
  log1p = c_log1pf
  {-# INLINE log1p #-}
  expm1 = c_expm1f
  {-# INLINE expm1 #-}

instance (RealFloat a, Precise a) => Precise (Complex a) where
  expm1 x@(a :+ b)
    | a*a + b*b < 1, u <- expm1 a, v <- sin (b/2), w <- -2*v*v = (u*w+u+w) :+ (u+1)*sin b
    | otherwise = exp x - 1
  {-# INLINE expm1 #-}
  log1p x@(a :+ b)
    | abs a < 0.5 && abs b < 0.5, u <- 2*a+a*a+b*b = log1p (u/(1+sqrt (u+1))) :+ atan2 (1 + a) b
    | otherwise = log (1 + x)
  {-# INLINE log1p #-}

foreign import ccall unsafe "math.h log1p" c_log1p :: Double -> Double
foreign import ccall unsafe "math.h expm1" c_expm1 :: Double -> Double
foreign import ccall unsafe "math.h expm1f" c_expm1f :: Float -> Float
foreign import ccall unsafe "math.h log1pf" c_log1pf :: Float -> Float
