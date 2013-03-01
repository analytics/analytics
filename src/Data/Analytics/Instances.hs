{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Analytics.Instances where

import Control.Lens
import Control.Monad.Logic

instance (Profunctor p, Bifunctor p, Functor f, Monad m, m ~ n, a ~ b) => Cons p f (LogicT m a) (LogicT n b) a b where
  _Cons = unto (reflect . Just)
