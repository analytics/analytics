{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
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
module Data.Analytics.Instances where

import Control.Lens
import Control.Monad.Logic

instance (Profunctor p, Bifunctor p, Functor f, Monad m, m ~ n, a ~ b) => Cons p f (LogicT m a) (LogicT n b) a b where
  _Cons = unto (reflect . Just)

