{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
#if defined(__GLASGOW_HASKELL__) && __GLASGOW_HASKELL__ >= 706
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeOperators #-}
#define USE_TYPE_LITS 1
#endif
{-# OPTIONS_GHC -fno-warn-orphans #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Reflection
  ( int, nat, Z, D, SD, PD
  ) where

import Control.Lens
import Control.Monad
import Data.Proxy
import Data.Reflection
import Language.Haskell.TH
#ifdef USE_TYPE_LITS
import GHC.TypeLits
#endif

data Z -- 0
data D  (n :: *) -- 2n
data SD (n :: *) -- 2n+1
data PD (n :: *) -- 2n-1

instance Reifies Z Int where
  reflect _ = 0
  {-# INLINE reflect #-}

retagD :: (Proxy n -> a) -> proxy (D n) -> a
retagD f _ = f Proxy
{-# INLINE retagD #-}

retagSD :: (Proxy n -> a) -> proxy (SD n) -> a
retagSD f _ = f Proxy
{-# INLINE retagSD #-}

retagPD :: (Proxy n -> a) -> proxy (PD n) -> a
retagPD f _ = f Proxy
{-# INLINE retagPD #-}

instance Reifies n Int => Reifies (D n) Int where
  reflect = retagD reflect <&> \n -> n+n
  {-# INLINE reflect #-}

instance Reifies n Int => Reifies (SD n) Int where
  reflect = retagSD reflect <&> \n -> n+n+1
  {-# INLINE reflect #-}

instance Reifies n Int => Reifies (PD n) Int where
  reflect = retagPD reflect <&> \n -> n+n-1
  {-# INLINE reflect #-}

-- | This can be used to generate a template haskell splice for a type level version of a given 'int'.
--
-- This does not use GHC TypeLits, instead it generates a numeric type by hand similar to the ones used
-- in the \"Functional Pearl: Implicit Configurations\" paper by Oleg Kiselyov and Chung-Chieh Shan.
int :: Int -> TypeQ
int n = case quotRem n 2 of
  (0, 0) -> conT ''Z
  (q,-1) -> conT ''PD `appT` int q
  (q, 0) -> conT ''D  `appT` int q
  (q, 1) -> conT ''SD `appT` int q
  _     -> error "ghc is bad at math"

-- | This is a restricted version of 'int' that can only generate natural numbers. Attempting to generate
-- a negative number results in a compile time error. Also the resulting sequence will consist entirely of
-- Z, D, and SD constructors representing the number in zeroless binary.
nat :: Int -> TypeQ
nat n
  | n >= 0 = int n
  | otherwise = error "nat: negative"

instance Num a => Num (Q a) where
  (+) = liftM2 (+)
  (*) = liftM2 (*)
  (-) = liftM2 (-)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = return . fromInteger

instance Fractional a => Fractional (Q a) where
  (/) = liftM2 (/)
  recip = fmap recip
  fromRational = return . fromRational

-- | This permits the use of $(5) as a type splice.
instance Num Type where
#ifdef USE_TYPE_LITS
  a + b = AppT (AppT (VarT ''(+)) a) b
  a * b = AppT (AppT (VarT ''(*)) a) b
#if MIN_VERSION_base(4,8,0)
  a - b = AppT (AppT (VarT ''(-)) a) b
#else
  (-) = error "Type.(-): undefined"
#endif
  fromInteger = LitT . NumTyLit
#else
  (+) = error "Type.(+): undefined"
  (*) = error "Type.(*): undefined"
  (-) = error "Type.(-): undefined"
  fromInteger n = case quotRem n 2 of
      (0, 0) -> ConT ''Z
      (q,-1) -> ConT ''PD `AppT` fromInteger q
      (q, 0) -> ConT ''D  `AppT` fromInteger q
      (q, 1) -> ConT ''SD `AppT` fromInteger q
      _ -> error "ghc is bad at math"
#endif
  abs = error "Type.abs"
  signum = error "Type.signum"

plus, times, minus :: Num a => a -> a -> a
plus = (+)
times = (*)
minus = (-)
fract :: Fractional a => a -> a -> a
fract = (/)

-- | This permits the use of $(5) as an expression splice.
instance Num Exp where
  a + b = AppE (AppE (VarE 'plus) a) b
  a * b = AppE (AppE (VarE 'times) a) b
  a - b = AppE (AppE (VarE 'minus) a) b
  negate = AppE (VarE 'negate)
  signum = AppE (VarE 'signum)
  abs = AppE (VarE 'abs)
  fromInteger = LitE . IntegerL

instance Fractional Exp where
  a / b = AppE (AppE (VarE 'fract) a) b
  recip = AppE (VarE 'recip)
  fromRational = LitE . RationalL
