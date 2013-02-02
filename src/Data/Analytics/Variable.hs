{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
--------------------------------------------------------------------
-- |
-- Module    :  Data.Analytics.Variable
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Variable
  ( Variable(..), matchVar
  , HasVars(..)
  ) where

import Data.Analytics.Match
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

------------------------------------------------------------------------------
-- Variable
------------------------------------------------------------------------------

class HasVars s t a b | s -> a, t -> b, s b -> t, t a -> s where
  vars :: Traversal s t a b

instance HasVars s t a b => HasVars (Map x s) (Map x t) a b where
  vars = traverse.vars
  {-# INLINE vars #-}

instance HasVars s t a b => HasVars (IntMap s) (IntMap t) a b where
  vars = traverse.vars
  {-# INLINE vars #-}

instance HasVars s t a b => HasVars [s] [t] a b where
  vars = traverse.vars
  {-# INLINE vars #-}

