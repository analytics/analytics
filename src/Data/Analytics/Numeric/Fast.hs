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
  , pow_fast_smooth
  , pow_fast_precise
  , exp_fast
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
  flog = logf_fast
  fexp = expf_fast
  fpow = powf_fast_precise

instance Fast Double where
  flog     = log_fast_smooth
  flog_lb  = log_fast_smooth_lower_bound
  flog_ub  = log_fast_smooth_upper_bound
  fexp     = exp_fast_smooth
  fexp_lb  = exp_fast_smooth_lower_bound
  fexp_ub  = exp_fast_smooth_upper_bound
  fpow     = pow_fast_precise_smooth
  fpow_lb  = pow_fast_smooth_lower_bound
  fpow_ub  = pow_fast_smooth_upper_bound

-- | Borchardt’s Algorithm from “Dead Reckoning: Calculating without instruments”.
--
-- This is a remarkably bad approximate logarithm.
--
-- 'flog' should outperform it! It is provided merely for comparison.
blog :: Floating a => a -> a
blog x = 6 * (x - 1) / (x + 1 + 4 * sqrt(x));

foreign import ccall unsafe pow_fast  :: Double -> Double -> Double
foreign import ccall unsafe pow_fast_smooth  :: Double -> Double -> Double
foreign import ccall unsafe pow_fast_smooth_lower_bound  :: Double -> Double -> Double
foreign import ccall unsafe pow_fast_smooth_upper_bound  :: Double -> Double -> Double
foreign import ccall unsafe powf_fast :: Float -> Float -> Float
foreign import ccall unsafe pow_fast_precise  :: Double -> Double -> Double
foreign import ccall unsafe pow_fast_precise_smooth  :: Double -> Double -> Double
foreign import ccall unsafe powf_fast_precise :: Float -> Float -> Float
foreign import ccall unsafe exp_fast  :: Double -> Double
foreign import ccall unsafe exp_fast_smooth :: Double -> Double
foreign import ccall unsafe exp_fast_smooth_lower_bound :: Double -> Double
foreign import ccall unsafe exp_fast_smooth_upper_bound :: Double -> Double
foreign import ccall unsafe expf_fast :: Float -> Float
foreign import ccall unsafe log_fast  :: Double -> Double
foreign import ccall unsafe log_fast_smooth :: Double -> Double
foreign import ccall unsafe log_fast_smooth_lower_bound :: Double -> Double
foreign import ccall unsafe log_fast_smooth_upper_bound :: Double -> Double
foreign import ccall unsafe logf_fast :: Float -> Float
