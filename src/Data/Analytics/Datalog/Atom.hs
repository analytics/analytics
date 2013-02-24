{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Datalog.Atom
  ( Atomic(..)
  , Atom(..)
  , Table(..)
  , tableId
  , tableRollup
  ) where

import Control.Applicative
import Control.Monad
import Control.Lens
import Data.Analytics.Datalog.Row
import Data.Analytics.Datalog.Subst
import Data.Analytics.Datalog.Term
import Data.Typeable

data Table a = Table
  { _tableId :: {-# UNPACK #-} !Int
  , _tableRollup :: a -> a -> a
  } deriving Typeable

instance Show (Table a) where
  showsPrec d (Table i _) = showParen (d > 10) $
    showString "Table " . showsPrec 11 i . showString " .."

makeLenses ''Table

data Atom :: * -> * -> * where
  Atom :: (Typeable a, Show a, Typeable b) => !(Table a) -> !(Row (a -> b)) -> Atom a b
  deriving Typeable

instance Show (Atom a b) where
  showsPrec d (Atom (Table i _) r) = showParen (d > 10) $
    showsPrec 10 i . showChar ' ' . showsPrec 10 r

instance HasVars (Atom a b) where
  applyM s (Atom t r) = Atom t `liftM` applyM s r
  {-# INLINEABLE applyM #-}

  vars f (Atom l r) = Atom l <$> vars f r
  {-# INLINEABLE vars #-}

instance Term x => TermOf (Atom a b) x

class Atomic r a b where
  atom :: Atom a b -> r

instance (Typeable c, Show c, Typeable d, a ~ c, b ~ d) => Atomic (Atom a b) c d where
  atom = id

