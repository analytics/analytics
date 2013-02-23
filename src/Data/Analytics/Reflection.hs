{-# LANGUAGE KindSignatures, TemplateHaskell, MultiParamTypeClasses, FlexibleContexts #-}
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
import Data.Proxy
import Data.Reflection
import Language.Haskell.TH

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
