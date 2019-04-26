{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}

module Control.Pipe.Internal
  ( Pipe(..)
  , runPipe
  , await
  , yield
  , termLeft
  , (.|)
  , Await(..)
  , runAwait
  , Yield(..)
  , runYield
  , Await'
  , Yield'
  ) where


import Data.Void

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource

-- | Await type; holds a callback that waits for a value
newtype Await i a = Await { unAwait :: Await' i a }

-- | Yield type; holds a callback that yields a value
data Yield i a = Yield
  { terminate :: a
  , unYield :: !(Yield' i a)
  }

type Await' i a = Yield i a -> a

type Yield' i a = i -> Await i a -> a

runAwait :: Await i a -> a -> Yield' i a -> a
runAwait (Await awt) a yld = awt $ Yield a yld
{-# INLINE runAwait #-}

runYield :: Yield i a -> i -> Await' i a -> a
runYield (Yield _ a) i = a i . Await
{-# INLINE runYield #-}

termLeft :: Await i a
termLeft = Await terminate
{-# INLINE termLeft #-}

-- | Left and right continuations
type L i m r = Await i (m r)
type R o m r = Yield o (m r)

-- | Pipe datatype. 
--
--   i = input
--
--   o = output
--
--   m = base monad
--
--   a = return type
--
newtype Pipe i o m a =
  Pipe { unPipe :: forall r. (L i m r -> R o m r -> a -> m r) -> L i m r -> R o m r -> m r }

instance Monad m => Functor (Pipe i o m) where
  fmap f (Pipe p) = Pipe $ \rest l r -> p (\l r -> rest l r . f) l r

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

instance MonadResource m => MonadResource (Pipe i o m) where
  liftResourceT = lift . liftResourceT

instance MonadThrow m => MonadThrow (Pipe i o m) where
  throwM = lift . throwM

instance Monad m => Semigroup (Pipe i o m ()) where
  (<>) = (>>)

instance Monad m => Monoid (Pipe i o m ()) where
  mempty = pure ()


-- | 
--
await :: Pipe i o m a -> (i -> Pipe i o m a) -> Pipe i o m a
await def act = Pipe $
  \rest a y ->
    let run act awt = unPipe act rest awt y
     in runAwait a (run def a) $ \i a' -> run (act i) a'
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
runPipe pipe = unPipe pipe (\_ _ -> pure) termLeft voidOut
  where
    voidOut = Yield (error "Void") (\i _ -> absurd i)

-- | Compose a pipe with another pipe; the return type is that of the second pipe.
--
(.|) :: forall i o e m b. Pipe i e m () -> Pipe e o m b -> Pipe i o m b
(Pipe f1) .| (Pipe f2) =
  Pipe $ \rest l r -> do
    let goRight left = f2 (\_ -> rest termLeft) left r
        one i left = Await $ \y -> unYield y i left
        yieldRight i left = goRight (one i left)
        termRight = goRight termLeft
    -- The yield right function triggers the right hand pipe
    -- The return method terminates to the right
    f1 (\_ y () -> terminate y) l (Yield termRight yieldRight)
