--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- These functions provide wildly inaccurate but very fast
-- approximations to common transcendental functions.
--
-- The algorithms here are based on Martin Ankerl's optimized 'pow',
-- <http://martin.ankerl.com/2007/10/04/optimized-pow-approximation-for-java-and-c-c/>
-- which is in turn based on
-- <http://nic.schraudolph.org/pubs/Schraudolph99.pdf>
--------------------------------------------------------------------
module Data.Analytics.Numeric.Fast
  ( Fast(..)
  , blog
  , pow_fast
  , pow_fast_precise
  , log_fast
  ) where

class Floating a => Fast a where
  -- | Calculate an approximate log.
  flog    :: a -> a
  flog_lb :: a -> a
  flog_ub :: a -> a
  -- | Calculate an approximate exp.
  fexp    :: a -> a
  fexp_lb :: a -> a
  fexp_ub :: a -> a
  -- | Calculate an approximate pow.
  fpow    :: a -> a -> a
  fpow_lb :: a -> a -> a
  fpow_ub :: a -> a -> a

instance Fast Float where
  flog     = logf_fast
  flog_lb  = logf_fast_lb
  flog_ub  = logf_fast_ub
  fexp     = expf_fast
  fexp_lb  = expf_fast_lb
  fexp_ub  = expf_fast_ub
  fpow     = powf_fast_precise

instance Fast Double where
  flog     = log_fast
  flog_lb  = log_fast_lb
  flog_ub  = log_fast_ub
  fexp     = exp_fast
  fexp_lb  = exp_fast_lb
  fexp_ub  = exp_fast_ub
  fpow     = pow_fast_precise
  fpow_lb  = pow_fast_lb
  fpow_ub  = pow_fast_ub

-- | Borchardt’s Algorithm from “Dead Reckoning: Calculating without instruments”.
--
-- This is a remarkably bad approximate logarithm.
--
-- 'flog' should outperform it! It is provided merely for comparison.
blog :: Floating a => a -> a
blog x = 6 * (x - 1) / (x + 1 + 4 * sqrt(x));

foreign import ccall unsafe pow_fast  :: Double -> Double -> Double
foreign import ccall unsafe pow_fast_ankerl  :: Double -> Double -> Double
foreign import ccall unsafe pow_fast_lb  :: Double -> Double -> Double
foreign import ccall unsafe pow_fast_ub  :: Double -> Double -> Double
foreign import ccall unsafe powf_fast :: Float -> Float -> Float
foreign import ccall unsafe pow_fast_precise_ankerl  :: Double -> Double -> Double
foreign import ccall unsafe pow_fast_precise  :: Double -> Double -> Double
foreign import ccall unsafe powf_fast_precise :: Float -> Float -> Float
foreign import ccall unsafe exp_fast  :: Double -> Double
foreign import ccall unsafe exp_fast_schraudolph :: Double -> Double
foreign import ccall unsafe exp_fast_lb :: Double -> Double
foreign import ccall unsafe exp_fast_ub :: Double -> Double
foreign import ccall unsafe expf_fast :: Float -> Float
foreign import ccall unsafe expf_fast_lb :: Float -> Float
foreign import ccall unsafe expf_fast_ub :: Float -> Float
foreign import ccall unsafe log_fast  :: Double -> Double
foreign import ccall unsafe log_fast_ankerl :: Double -> Double
foreign import ccall unsafe log_fast_lb :: Double -> Double
foreign import ccall unsafe log_fast_ub :: Double -> Double
foreign import ccall unsafe logf_fast :: Float -> Float
foreign import ccall unsafe logf_fast_lb :: Float -> Float
foreign import ccall unsafe logf_fast_ub :: Float -> Float
