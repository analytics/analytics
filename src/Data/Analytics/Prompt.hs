{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
--------------------------------------------------------------------
-- |
-- Module    :  Data.Analytics.Prompt
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module lets you step through a 'Datalog' program one 'Fact',
-- rule or 'Query' at a time.
--
-- /Note:/ This module conflicts with @Data.Analytics.Datalog@ and
-- @Data.Analytics.Internal.Datalog@ and the API is mostly of interest
-- to query engine providers, so it is not re-exported through
-- @Data.Analytics@.
--------------------------------------------------------------------
module Data.Analytics.Prompt
  (
  -- * Each Step
    Step, StepT(..)
  -- * Prompting for Steps
  , Prompt, PromptT(..)
  , prompt, promptT
  , unprompt, unpromptT
  -- * Prompt isomorphisms
  , _Prompt, _PromptT
  ) where

import Control.Lens
import Control.Monad
import Data.Analytics.Internal.Atom
import qualified Data.Analytics.Internal.Datalog as Datalog
import Data.Analytics.Internal.Datalog hiding (Fact, Query, (:-))
import Data.Analytics.Internal.Query
import Data.Functor.Identity

infixr 0 :-
infixl 1 :>>=

type Step t = StepT t Identity

-- | A single 'Datalog' 'Fact', rule or 'Query'.
data StepT :: * -> (* -> *) -> * -> * where
  Fact  :: Atom t a -> StepT t m ()
  (:-)  :: Atom t a -> Query t b -> StepT t m ()
  Query :: Query t a -> StepT t m [a]

type Prompt t = PromptT t Identity

-- | The result of prompting, after monadic effects.
data PromptT :: * -> (* -> *) -> * -> * where
  Done :: a -> PromptT t m a
  (:>>=) :: StepT t m a -> (a -> DatalogT t m b) -> PromptT t m b

prompt :: Datalog t a -> Prompt t a
prompt = runIdentity . promptT

unprompt :: PromptT t m a -> DatalogT t m a
unprompt (Fact xs :>>= k)  = Bind (Datalog.Fact xs) k
unprompt (Query q :>>= k)  = Bind (Datalog.Query q) k
unprompt ((h :- b) :>>= k) = Bind (h Datalog.:- b) k
unprompt (Done a)          = Return a

-- | Quotient out operational details of the Datalog program and get to the
-- next 'Step'.
promptT :: Monad m => DatalogT t m a -> m (PromptT t m a)
promptT (Datalog.Fact xs)          = return $ Fact xs :>>= return
promptT (Datalog.Query q)          = return $ Query q :>>= return
promptT (h Datalog.:- b)           = return $ (h :- b) :>>= return
promptT (Return a)                 = return $ Done a
promptT (Lift m)                   = liftM Done m
promptT (Bind (Datalog.Fact xs) g) = return $ Fact xs :>>= g
promptT (Bind (Datalog.Query q) g) = return $ Query q :>>= g
promptT (Bind (h Datalog.:- b) g)  = return $ (h :- b) :>>= g
promptT (Bind (Lift m) g)          = m >>= promptT . g
promptT (Bind (Return a) g)        = promptT (g a)
promptT (Bind m g `Bind` h)        = promptT $ m `Bind` \x -> g x `Bind` h

unpromptT :: m (PromptT t m a) -> DatalogT t m a
unpromptT mp = Bind (Lift mp) unprompt

_PromptT :: Monad m => Iso (DatalogT t m a) (DatalogT t n b) (m (PromptT t m a)) (n (PromptT t n b))
_PromptT = iso promptT unpromptT

_Prompt :: Iso (Datalog t a) (Datalog t b) (Prompt t a) (Prompt t b)
_Prompt = iso prompt unprompt
