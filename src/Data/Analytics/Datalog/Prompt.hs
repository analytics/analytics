{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE GADTs #-}
--------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2013
-- License   :  BSD3
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
-- This module lets you step through a 'Datalog' program one
-- rule or 'Query' at a time.
--
-- /Note:/ This module conflicts with @Data.Analytics.Datalog.Monad@ and
-- the API is mostly of interest
-- to query engine providers, so it is not re-exported through
-- @Data.Analytics.Datalog@.
--------------------------------------------------------------------
module Data.Analytics.Datalog.Prompt
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
import Data.Analytics.Datalog.Atom
import qualified Data.Analytics.Datalog.Monad as Datalog
import Data.Analytics.Datalog.Monad hiding (Query, (:-))
import Data.Analytics.Datalog.Query
import Data.Functor.Identity

infixr 0 :-
infixl 1 :>>=

type Step = StepT Identity

-- | A single 'Datalog' rule or 'Query'.
data StepT :: (* -> *) -> * -> * where
  (:-)  :: Atom a b -> Query Body a -> StepT m ()
  Query :: Query Request a -> StepT m [a]

type Prompt = PromptT Identity

-- | The result of prompting, after monadic effects.
data PromptT :: (* -> *) -> * -> * where
  Done :: a -> PromptT m a
  (:>>=) :: StepT m a -> (a -> DatalogT m b) -> PromptT m b

prompt :: Datalog a -> Prompt a
prompt = runIdentity . promptT

unprompt :: PromptT m a -> DatalogT m a
unprompt (Query q :>>= k)  = Bind (Datalog.Query q) k
unprompt ((h :- b) :>>= k) = Bind (h Datalog.:- b) k
unprompt (Done a)          = Return a

-- | Quotient out operational details of the Datalog program and get to the
-- next 'Step'.
promptT :: Monad m => DatalogT m a -> m (PromptT m a)
promptT (Datalog.Query q)          = return $ Query q :>>= return
promptT (h Datalog.:- b)           = return $ (h :- b) :>>= return
promptT (Return a)                 = return $ Done a
promptT (Lift m)                   = liftM Done m
promptT (Bind (Datalog.Query q) g) = return $ Query q :>>= g
promptT (Bind (h Datalog.:- b) g)  = return $ (h :- b) :>>= g
promptT (Bind (Lift m) g)          = m >>= promptT . g
promptT (Bind (Return a) g)        = promptT (g a)
promptT (Bind m g `Bind` h)        = promptT $ m `Bind` \x -> g x `Bind` h

unpromptT :: m (PromptT m a) -> DatalogT m a
unpromptT mp = Bind (Lift mp) unprompt

_PromptT :: Monad m => Iso (DatalogT m a) (DatalogT n b) (m (PromptT m a)) (n (PromptT n b))
_PromptT = iso promptT unpromptT

_Prompt :: Iso (Datalog a) (Datalog b) (Prompt a) (Prompt b)
_Prompt = iso prompt unprompt
