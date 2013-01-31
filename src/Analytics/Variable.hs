{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
--------------------------------------------------------------------
-- |
-- Module    :  Analytics.Variable
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Analytics.Variable
  ( Variable(..), matchVar
  ) where

import Analytics.Match
import Control.Applicative
import Control.Lens
import Data.Void
import Prelude.Extras

------------------------------------------------------------------------------
-- Variable
------------------------------------------------------------------------------

-- | A 'Variable' type has a distinguishable 'var' construction.
class (Match v, Eq1 v) => Variable v where
  var :: Prism (v a) (v b) a b

-- | Default definition of 'match' for a 'Var'
matchVar :: Variable t => (a -> b -> c) -> t a -> t b -> Maybe (t c)
matchVar f va vb = case var Left va of
  Left a -> Just (f a <$> vb)
  Right a' -> case var Left vb of
    Left b -> Just ((`f` b) <$> va)
    Right b' | a' ==# b' -> Just (vacuous a')
             | otherwise -> Nothing
