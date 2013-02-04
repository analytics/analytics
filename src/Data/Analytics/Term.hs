{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------
-- |
-- Module    :  Data.Analytics.Term
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
--------------------------------------------------------------------
module Data.Analytics.Term
  ( Term(..)
  , var
  ) where

import Data.Analytics.Internal.Term
