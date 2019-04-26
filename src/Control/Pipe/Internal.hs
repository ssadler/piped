{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Strict #-}

module Control.Pipe.Internal
  ( Pipe(..)
  , runPipe
  , await
  , yield
  , (.|)
  , Await(..)
  , runAwait
  , runAwait'
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
  { unYield :: Yield' i a
  , terminate :: a
  }

type Await' i a = Yield i a -> a

type Yield' i a = i -> Await i a -> a

runAwait :: Await i a -> a -> Yield' i a -> a
runAwait (Await awt) a yld = awt $ Yield yld a
{-# INLINE runAwait #-}

-- runAwait' :: Await i a -> (Await i a -> i -> a) -> a
-- runAwait' (Await a) = a . Yield . flip

runYield :: Yield i a -> i -> Await' i a -> a
runYield (Yield a _) i = a i . Await
{-# INLINE runYield #-}

-- instance Semigroup (Await i a) where
--   a1 <> a2 =
--     Await $ \yld ->
--       let out Nothing _ = unAwait a2 yld
--           out mi      a = unYield yld mi $ a <> a2
--        in runAwait a1 out
-- 
-- instance Monoid (Await i a) where
--   mempty = Await $ \yld -> unYield yld Nothing mempty

--
-- Monad
--

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


-- | Await a value from upstream. This will return Nothing if there is no value,
--   or Just the value otherwise.
--
-- await :: Pipe i o m (Maybe i)
-- await = Pipe $
--   \rest a y -> runAwait a $ \i a' -> rest a' y i
-- {-# INLINE await #-}

await :: Pipe i o m a -> (i -> Pipe i o m a) -> Pipe i o m a
await def act = Pipe $
  \rest a y ->
    let run act awt = unPipe act rest awt y
     in runAwait a (run def a) $ \i a -> undefined -- run (act i) a >>= rest a y


-- | Yield a value to downstream.
--
yield :: o -> Pipe i o m ()
yield i = Pipe $
  \rest a y -> runYield y i $ \y' -> rest a y' ()
{-# INLINE yield #-}


-- | Run pipe to completion.
--
runPipe :: Monad m => Pipe () Void m r -> m r
runPipe pipe = unPipe pipe (\_ _ -> pure) (error "fixme") voidOut
  where

    voidOut = Yield (\i _ -> absurd i) (error "void")

-- | Compose a pipe with another pipe; the return type is that of the second pipe.
--
(.|) :: forall i o e m b. Pipe i e m () -> Pipe e o m b -> Pipe i o m b
(Pipe f1) .| (Pipe f2) =
  Pipe $ \rest l r -> do
    let rest' _ y = rest mempty y
        one i left = Await $ \y -> runYield y i $ unAwait left
        right i left = f2 rest' (one i left) r
        terminate y = runYield y Nothing terminate
    -- The yield right function triggers the right hand pipe
    -- The return method terminates to the right
    f1 (\_ y () -> terminate y) l (Yield right)
