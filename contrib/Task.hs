{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Data.Analytics.Task.Monad
  ( Task(..)
  , Env(..)
  , dynamicWind
  , done
  , mask
  -- * Stacks
  , Stack(Empty)
  , switch
  ) where

import Control.Applicative
import Control.Exception as Exception hiding (mask)
import Control.Exception.Lens as Lens
import Control.Lens
import Control.Monad
import Control.Monad.CatchIO as CatchIO
import Control.Monad.Cont.Class
import Control.Monad.IO.Class
import Data.Typeable
import GHC.Prim

data EmptyTask = EmptyTask deriving (Show,Typeable)
instance Exception EmptyTask

type Depth = Int

data Stack
  = Frame
    { frameDepth :: {-# UNPACK #-} !Depth
    , frameRewind :: IO ()
    , frameUnwind :: IO ()
    , frameNext :: Stack }
  | Empty

depth :: Stack -> Int
depth (Frame i _ _ _) = i
depth Empty = 0

data Env r = Env
  { envHandler    :: SomeException -> IO r
  , envBackground :: Stack -> IO r
  , envSpawn      :: Task r () -> IO r
  , envStack      :: Stack
  }

spawn :: Task r () -> Task r ()
spawn t = Task $ \kp e -> do
  envSpawn e t
  kp ()
{-# INLINE spawn #-}

newtype Task r a = Task { runTask :: (a -> IO r) -> Env r -> IO r }

instance Functor (Task r) where
  fmap f (Task m) = Task $ \k -> m (k . f)
  {-# INLINE fmap #-}

instance Applicative (Task r) where
  pure a = Task $ \kp _ -> kp a
  {-# INLINE pure #-}
  (<*>) = ap
  {-# INLINE (<*>) #-}

instance Monad (Task r) where
  return a = Task $ \kp _ -> kp a
  {-# INLINE return #-}
  m >>= f = Task $ \kp h -> runTask m (\a -> runTask (f a) kp h) h
  {-# INLINE (>>=) #-}
  fail s = Task $ \_ h -> envHandler h (_ErrorCall # s)
  {-# INLINE fail #-}

instance MonadIO (Task r) where
  liftIO m = Task $ \kp e -> Exception.try m >>= either (envHandler e) kp
  {-# INLINE liftIO #-}

instance MonadCatchIO (Task r) where
  catch (Task m) f = Task $ \kp e ->
    m kp e {
      envHandler = \ se -> case cast se of
        Just x  -> runTask (f x) kp e
        Nothing -> envHandler e se
    }
  {-# INLINE catch #-}
  -- block = dynamicWind Exception.block Exception.unblock
  -- {-# INLINE block #-}
  -- unblock = dynamicWind Exception.unblock Exception.block
  -- {-# INLINE unblock #-}

instance Alternative (Task r) where
  empty = Task $ \_ e -> envHandler e (toException EmptyTask)
  {-# INLINE empty #-}
  Task m <|> Task n = Task $ \kp e ->
    m kp e { envHandler = \_ -> n kp e }

instance MonadPlus (Task r) where
  mzero = empty
  {-# INLINE mzero #-}
  mplus (Task m) (Task n) = Task $ \kp e ->
    m kp e { envHandler = \_ -> n kp e }
  {-# INLINE mplus #-}

done :: Task r a
done = Task $ \_ e -> envBackground e (envStack e)
{-# INLINE done #-}

dynamicWind :: IO () -> IO () -> Task r a -> Task r a
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
instance MonadCont (Task r) where
  callCC h = Task $ \kp e@Env { envStack = s } ->
    let go x = Task $ \kp' Env { envStack = s' } -> switch s' s >> kp x
    in runTask (h go) kp e
  {-# INLINE callCC #-}

-- | A generalized 'mask' for 'MonadCatchIO'
mask :: MonadCatchIO m => ((forall a. m a -> m a) -> m b) -> m b
mask f = liftIO getMaskingState >>= \b -> case b of
  Unmasked -> CatchIO.block $ f CatchIO.unblock
  _        -> f id
{-# INLINE mask #-}

