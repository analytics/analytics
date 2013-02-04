{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
--------------------------------------------------------------------
-- |
-- Module    :  Data.Analytics.Internal.Term
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Internal.Term
  ( Term(..)
  , Handler(..)
  , var
  , entity
  , TermOf
  ) where

import Data.Typeable

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

class (Typeable (Entity a), Ord (Entity a), Typeable a, Ord a) => Term a where
  type Entity a :: *
  type Entity a = a

  term :: Handler a
  default term :: Entity a ~ a => Handler a
  term = entity

class Term a => TermOf r a