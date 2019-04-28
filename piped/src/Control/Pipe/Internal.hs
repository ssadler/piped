{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Pipe.Internal
  ( Pipe(..)
  , Await(..)
  , Yield(..)
  , Await'
  , Yield'
  , runPipe
  , (.|)
  , await
  , yield
  , runAwait
  , runYield
  , termLeft
  , sinkVoid
  , injectLeftover
  , Void
  ) where


import Data.Void

import Control.Monad
import Control.Monad.State
import Control.Monad.Trans

import Debug.Trace

-- | Await type; holds a callback that waits for a value
newtype Await i m a = Await { unAwait :: Await' i m a }

-- | Yield type; holds a callback that yields a value
data Yield i m a = Yield
  { terminate :: m a
  , unYield   :: Yield' i m a
  }

type Await' i m a = Yield i m a -> m a

type Yield' i m a = i -> Await i m a -> m a

runAwait :: Await i m a -> m a -> Yield' i m a -> m a
runAwait (Await awt) a yld = awt $ Yield a yld
{-# INLINE runAwait #-}

runYield :: Yield i m a -> i -> Await' i m a -> m a
runYield (Yield _ a) i = a i . Await
{-# INLINE runYield #-}

termLeft :: Await i m a
termLeft = Await terminate
{-# INLINE termLeft #-}

sinkVoid :: Yield Void m a
sinkVoid = Yield (error "Void") (\i _ -> absurd i)

injectLeftover :: i -> Await i m a -> Await i m a
injectLeftover i await = Await $ \y -> unYield y i await

-- | Left and right continuations
type Rest i o r m a = (Await i m r -> Yield o m r -> a -> m r)

-- | Pipe datatype. 
--
newtype Pipe i o m a =
  Pipe { unPipe :: forall r. Rest i o r m a -> Await i m r -> Yield o m r -> m r }

instance Monad m => Functor (Pipe i o m) where
  fmap f (Pipe p) = Pipe $
    \rest l r -> p (\l r -> rest l r . f) l r

instance Monad m => Applicative (Pipe i o m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (Pipe i o m) where
  return x = Pipe $ \f l r -> f l r x
  Pipe f >>= g = Pipe $
    \rest -> f (\l r a -> unPipe (g a) rest l r)

instance MonadTrans (Pipe i o) where
  lift mf = Pipe $ \f l r -> mf >>= f l r

instance MonadIO m => MonadIO (Pipe i o m) where
  liftIO = lift . liftIO

instance Monad m => Semigroup (Pipe i o m a) where
  (<>) = (>>)

instance Monad m => Monoid (Pipe i o m ()) where
  mempty = pure ()

instance MonadState s m => MonadState s (Pipe i o m) where
  get = lift get
  put = lift . put

-- | Await a value. If there are no more values, Nothing is retuend.
--
await :: Monad m => Pipe i o m (Maybe i)
await = Pipe $
  \rest a y ->
    runAwait a (rest termLeft y Nothing) $ \i a -> rest a y (Just i)
{-# INLINE await #-}


-- | Yield a value to downstream.
--
yield :: o -> Pipe i o m ()
yield i = Pipe $
  \rest a y -> runYield y i $ \y' -> rest a y' ()
{-# INLINE yield #-}


-- | Run pipe to completion.
--
runPipe :: Monad m => Pipe () Void m r -> m r
runPipe pipe = unPipe pipe (\_ _ -> pure) termLeft sinkVoid
{-# INLINE runPipe #-}


-- | Compose a pipe with another pipe; the return type is that of the second pipe.
--
(.|) :: forall i o e m b. Monad m => Pipe i e m () -> Pipe e o m b -> Pipe i o m b
(Pipe f1) .| (Pipe f2) =
  Pipe $ \rest l r ->
    f1 (\_ y () -> terminate y) l $
      Yield
        (           f2 (\_ -> rest termLeft) termLeft                 r)
        (\i left -> f2 (\_ -> rest termLeft) (injectLeftover i left)  r)
