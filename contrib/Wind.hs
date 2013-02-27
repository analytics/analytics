{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}

module Wind where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Data.IORef

data Frame r m a b where
  FFrame :: (a ->   b) -> Frame r m a b
  MFrame :: (a -> ContT r m b) -> Frame r m a b
  BFrame :: m () -> m () -> Frame r m a a

data Stack m a b where
  Empty   :: Stack m a a
  Push    :: Frame b m a z -> Stack m z b -> Stack m a b

newtype ContT r m a = ContT { unContT :: Stack m a r -> m r }

($$) :: (Monad m) => Stack m a b -> a -> m b
Empty               $$ x = return x
Push (FFrame   f) s $$ x = s $$ f x
Push (MFrame   f) s $$ x = f x `unContT` s
Push (BFrame _ a) s $$ x = a >> s $$ x

unwind :: (Monad m) => Stack m a b -> m ()
unwind Empty = return ()
unwind (Push (BFrame _ a) s) = a >> unwind s
unwind (Push _            s) = unwind s

rewind :: (Monad m) => Stack m a b -> m ()
rewind = go (return ())
 where
 go :: Monad m => m () -> Stack m a b -> m ()
 go act Empty = act
 go act (Push (BFrame b _) s) = go (b >> act) s
 go act (Push _            s) = go act s

pushFrame :: Frame r m a b -> ContT r m a -> ContT r m b
pushFrame f (ContT e) = ContT $ \k -> e (Push f k)

dynamicWind :: Monad m => m () -> m () -> ContT r m a -> ContT r m a
dynamicWind before after (ContT e) =
  ContT $ \k -> before >> e (Push (BFrame before after) k)

callCC :: Monad m => ((a -> ContT r m b) -> ContT r m a) -> ContT r m a
callCC h = ContT $ \k ->
  let jump x = ContT $ \k' -> unwind k' >> rewind k >> k $$ x
   in h jump `unContT` k

instance Functor (ContT r m) where
  fmap = pushFrame . FFrame

instance Monad m => Monad (ContT r m) where
  return x = ContT ($$ x)
  (ContT e) >>= f = ContT $ e . Push (MFrame f)

instance MonadTrans (ContT r) where
  lift m = ContT $ \k -> m >>= (k $$)

setup = do r <- lift $ newIORef 0
           x <- lift $ readIORef r
           c <- callCC $ \escape -> do
             dynamicWind (writeIORef r 1) (writeIORef r x) $ do
               lift $ readIORef r >>= print
               callCC $ escape . Left
               lift $ readIORef r >>= print
               lift $ Right <$> readIORef r
           return (r, c)


test :: ContT () IO ()
test = do (r, c) <- setup
          lift $ readIORef r >>= print
          case c of
            Left k  -> lift (putStrLn "Left 1") >> k ()
            Right i -> lift $ putStrLn "Right 1" >> print i
          lift $ readIORef r >>= print
          case c of
            Left k  -> lift (putStrLn "Left 2") >> k ()
            Right i -> lift $ putStrLn "Right 2" >> print i

