{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Datalog.Term
  ( Term(..)
  , Handler(..)
  , var
  , entity
  , TermOf
  , compareTerm
  , eqTerm
  ) where

import Data.Typeable
import Unsafe.Coerce

--------------------------------------------------------------------
-- Term
--------------------------------------------------------------------

data Handler a where
  IsVar :: Handler a
  IsEntity   :: Entity a ~ a => Handler a

var :: Handler a
var = IsVar

entity :: Entity a ~ a => Handler a
entity = IsEntity

#ifndef HLINT
class (Typeable (Entity a), Ord (Entity a), Show (Entity a), Typeable a, Ord a, Show a) => Term a where
  type Entity a :: *
  type Entity a = a

  term :: Handler a
  default term :: Entity a ~ a => Handler a
  term = entity
#endif

compareTerm :: (Term a, Term b) => a -> b -> Ordering
compareTerm x y = case typeOf x `compare` typeOf y of
  LT -> LT
  EQ -> unsafeCoerce x `compare` y
  GT -> GT
{-# INLINE compareTerm #-}

eqTerm :: (Term a, Term b) => a -> b -> Bool
eqTerm a b = cast a == Just b

class Term a => TermOf r a
