{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

-- | The internal API and core datatypes of Pipe.
--
module Piped.Internal
  (
  -- ** Pipe
  --
  -- | Piped is implemented using a type of codensity transform, that uses left and right continuations, rather than sum types, to propagate state transitions.
  --
    Pipe(..)
  , await
  , yield
  , runPipe
  , leftover

  -- ** Continuations
  --
  -- | The state of a pipeline is encoded in a recursive data structure (Await / Yield)
  --   which holds continuations that promise a final return value of @r@.
  --
  , Await(..)
  , Yield(..)
  , Await'
  , Yield'
  , runAwait
  , runYield
  , termLeft
  , termRight
  , voidRight
  , addLeftover
  , Void

  -- ** Miscellania
  --
  , fix1
  , fix2
  ) where


import Data.Void

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State


-- | The upstream continuation; holds a callback that provides a value.
--
newtype Await i m a = Await { unAwait :: Await' i m a }


-- | The downstream continuation; holds a callback that accepts a value,
--   plus a default return value in case there is no more input.
--
data Yield i m a = Yield
  { terminate :: m a
  , unYield   :: Yield' i m a
  }

-- | The type of an upstream continuation
--
type Await' i m a = Yield i m a -> m a

-- | The type of a downstream continuation
--
type Yield' i m a = i -> Await i m a -> m a

-- | Run an Await
--
runAwait :: Await i m a -> m a -> Yield' i m a -> m a
runAwait (Await awt) a yld = awt $ Yield a yld
{-# INLINE runAwait #-}

-- | Run a Yield
runYield :: Yield i m a -> i -> Await' i m a -> m a
runYield (Yield _ a) i = a i . Await
{-# INLINE runYield #-}

-- | An Await that terminates when called.
--
termLeft :: Await i m a
termLeft = Await terminate
{-# INLINE termLeft #-}

-- | A Yield that terminates when called.
--
termRight :: Yield i1 m a -> Yield i2 m a
termRight r = Yield (terminate r) (\_ _ -> terminate r)
{-# INLINE termRight #-}

-- | A Void output
--
voidRight :: Yield Void m a
voidRight = Yield (error "Void") (\i _ -> absurd i)
{-# INLINE voidRight #-}

-- | Wrap an Await with a leftover value
--
addLeftover :: i -> Await i m a -> Await i m a
addLeftover i await = Await $ \y -> unYield y i await


-- | The Pipe datatype, that represents a stage in a pipeline with inputs of type @i@ and outputs of type @o@.
--
newtype Pipe i o m a =
  Pipe { unPipe :: forall r. (Await i m r -> Yield o m r -> a -> m r) -> Await i m r -> Yield o m r -> m r }

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

instance MonadReader r m => MonadReader r (Pipe i o m) where
  ask = lift ask
  local f (Pipe p) = Pipe $ \rest l r -> local f $ p rest l r

instance MonadState s m => MonadState s (Pipe i o m) where
  get = lift get
  put = lift . put

instance Monad m => Semigroup (Pipe i o m a) where
  (<>) = (>>)

instance Monad m => Monoid (Pipe i o m ()) where
  mempty = pure ()


-- | Await a value. Nothing indicates that there are no more values.
--
await :: Monad m => Pipe i o m (Maybe i)
await = Pipe $
  \rest l r ->
    let term = rest termLeft r Nothing
     in runAwait l term $ \i l -> rest l r $ Just i
{-# INLINE await #-}


-- | Yield a value to downstream.
--
yield :: o -> Pipe i o m ()
yield i = Pipe $
  \rest a y -> runYield y i $ \y -> rest a y ()
{-# INLINE yield #-}


-- | Run pipe to completion.
--
runPipe :: Monad m => Pipe () Void m r -> m r
runPipe pipe = unPipe pipe (\_ _ -> pure) termLeft voidRight
{-# INLINE runPipe #-}


-- | Push a value back into the incoming pipeline
--
leftover :: i -> Pipe i o m ()
leftover i = Pipe $
  \rest l r -> rest (addLeftover i l) r ()
{-# INLINE leftover #-}


-- `fix` providing one value.
fix1 :: a -> ((a -> b) -> a -> b) -> b
fix1 a f = fix f a
{-# INLINE fix1 #-}


-- `fix` providing two values.
fix2 :: a -> b -> ((a -> b -> c) -> a -> b -> c) -> c
fix2 a b f = fix f a b
{-# INLINE fix2 #-}
