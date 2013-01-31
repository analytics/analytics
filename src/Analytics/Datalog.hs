{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
--------------------------------------------------------------------
-- |
-- Module    :  Analytics.Datalog
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Analytics.Datalog
  (
  -- * Unification
    Match(..)
  , Variable(..)
  , matchVar
  -- * Datalog
  , Datalog((:-))
  , query
  -- * Body
  , Body, no
  -- * Implementation Details
  , Rel(rel)
  ) where

import Analytics.Datalog.Internal

{-# ANN module "HLint: ignore Use import/export shortcut" #-}
