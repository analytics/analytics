{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}
module Data.Analytics.Task.Monad
  ( Task(..), (|>>)
  , Env(..)
  , dynamicWind
  , done
  , mask
  , MonadTask(..)
  -- * Stacks
  , Stack(Empty)
  , switch, rewind, unwind
  ) where

import Control.Applicative
import Control.Exception as Exception hiding (mask)
import Control.Exception.Lens as Lens
import Control.Lens
import Control.Monad
import Control.Monad.CatchIO as CatchIO
import Control.Monad.Cont.Class
import Control.Monad.Error.Class
import Control.Monad.IO.Class
import Control.Monad.Cont
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import Control.Monad.Writer.Lazy as Lazy
import Control.Monad.Writer.Strict as Strict
import Control.Monad.Reader as Reader
import Data.Analytics.Task.STM

import Data.Typeable
import GHC.Prim

data EmptyTask = EmptyTask deriving (Show,Typeable)
instance Exception EmptyTask

type Depth = Int

data Stack
  = Frame
    { _frameDepth :: {-# UNPACK #-} !Depth
    , _frameRewind :: IO ()
    , _frameUnwind :: IO ()
    , _frameNext :: Stack }
  | Empty

depth :: Stack -> Int
depth (Frame i _ _ _) = i
depth Empty = 0

data Env = Env
  { envHandler    :: SomeException -> IO ()
  , envBackground :: Stack -> IO ()
  , envSpawn      :: Task () -> IO ()
  , envStack      :: Stack
  }

newtype Task a = Task { runTask :: (a -> IO ()) -> Env -> IO () }

instance Functor Task where
  fmap f (Task m) = Task $ \k -> m (k . f)
  {-# INLINE fmap #-}

instance Applicative Task where
  pure a = Task $ \kp _ -> kp a
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad Task where
  return a = Task $ \kp _ -> kp a
  {-# INLINE return #-}
  m >>= f = Task $ \kp h -> runTask m (\a -> runTask (f a) kp h) h
  {-# INLINE (>>=) #-}
  fail s = Task $ \_ h -> envHandler h (_ErrorCall # s)
  {-# INLINE fail #-}

instance MonadIO Task where
  liftIO m = Task $ \kp e -> Exception.try m >>= either (envHandler e) kp
  {-# INLINE liftIO #-}

instance MonadError SomeException Task where
  throwError e = Task $ \_ env -> envHandler env e
  {-# INLINE throwError #-}
  Task m `catchError` f = Task $ \kp e -> m kp e { envHandler = \ se -> runTask (f se) kp e }
  {-# INLINE catchError #-}

instance MonadCatchIO Task where
  catch (Task m) f = Task $ \kp e ->
    m kp e {
      envHandler = \ se -> case cast se of
        Just x  -> runTask (f x) kp e
        Nothing -> envHandler e se
    }
  {-# INLINE catch #-}

  -- TODO: figure out a better story for these.

  block (Task m) = Task $ \kp e -> Exception.block (m kp e)
  -- block = dynamicWind Exception.block Exception.unblock
  {-# INLINE block #-}

  unblock (Task m) = Task $ \kp e -> Exception.unblock (m kp e)
  -- unblock = dynamicWind Exception.unblock Exception.block
  {-# INLINE unblock #-}

instance Alternative Task where
  empty = Task $ \_ e -> envHandler e (toException EmptyTask)
  {-# INLINE empty #-}
  Task m <|> Task n = Task $ \kp e ->
    m kp e { envHandler = \_ -> n kp e }

instance MonadPlus Task where
  mzero = empty
  {-# INLINE mzero #-}
  mplus (Task m) (Task n) = Task $ \kp e ->
    m kp e { envHandler = \_ -> n kp e }
  {-# INLINE mplus #-}

done :: Task a
done = Task $ \_ e -> envBackground e (envStack e)
{-# INLINE done #-}

dynamicWind :: IO () -> IO () -> Task a -> Task a
dynamicWind before after (Task m) = Task $ \kp e@Env { envStack = s } -> do
  before
  m kp $! e { envStack = Frame (depth s) before after $! s }
{-# INLINE dynamicWind #-}

-- switch contexts
switch :: Stack -> Stack -> IO ()
switch l@(Frame i _ a l') r@(Frame j b _ r') = case compare i j of
  LT -> switch l r' >> b
  EQ -> case reallyUnsafePtrEquality# l r of
    1# -> return ()
    _  -> a >> switch l' r' >> b
  GT -> a >> switch l' r
switch l Empty = unwind l
switch Empty r = rewind r
{-# INLINEABLE switch #-}

unwind :: Stack -> IO ()
unwind (Frame _ _ a as) = a >> unwind as
unwind Empty = return ()
{-# INLINEABLE unwind #-}

rewind :: Stack -> IO ()
rewind (Frame _ b _ bs) = rewind bs >> b
rewind Empty = return ()
{-# INLINEABLE rewind #-}

-- | The continuation is assumed to be single shot!
instance MonadCont Task where
  callCC h = Task $ \kp e@Env { envStack = s } ->
    let go x = Task $ \ _ Env { envStack = s' } -> switch s' s >> kp x
    in runTask (h go) kp e
  {-# INLINE callCC #-}

-- | A generalized 'mask' for 'MonadCatchIO'
mask :: MonadCatchIO m => ((forall a. m a -> m a) -> m b) -> m b
mask f = liftIO getMaskingState >>= \b -> case b of
  Unmasked -> CatchIO.block $ f CatchIO.unblock
  _        -> f id
{-# INLINE mask #-}

class Monad m => MonadTask m where
  spawn :: Task () -> m ()
  default spawn :: (MonadTrans t, MonadTask n, m ~ t n) => Task () -> m ()
  spawn = lift . spawn
  {-# INLINE spawn #-}

instance MonadTask Task where
  spawn t = Task $ \kp e -> do
    envSpawn e t
    kp ()
  {-# INLINE spawn #-}

(|>>) :: MonadTask m => Task () -> m b -> m b
m |>> r = spawn m >> r

instance MonadTask m => MonadTask (Strict.StateT s m)
instance MonadTask m => MonadTask (Lazy.StateT s m)
instance MonadTask m => MonadTask (ReaderT e m)
instance (MonadTask m, Monoid w) => MonadTask (Strict.WriterT w m)
instance (MonadTask m, Monoid w) => MonadTask (Lazy.WriterT w m)
instance MonadTask m => MonadTask (ContT r m)

instance MonadSTM Task where
  stm = liftIO . stm
  {-# INLINE stm #-}
  newTV = liftIO . newTV
  {-# INLINE newTV #-}
  readTV = liftIO . readTV
  {-# INLINE readTV #-}
  newTMV = liftIO . newTMV
  {-# INLINE newTMV #-}
  newEmptyTMV = liftIO newEmptyTMV
  {-# INLINE newEmptyTMV #-}
  newTC = liftIO newTC
  {-# INLINE newTC #-}
