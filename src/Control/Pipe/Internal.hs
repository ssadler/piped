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
  , LR(..)
  , Yield(..)
  , runYield
  , Await'
  , Yield'
  , provide
  ) where


import Data.Void

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Resource

-- | Await type; holds a callback that waits for a value
newtype Await i a = Await { unAwait :: Await' i a }

-- | Yield type; holds a callback that yields a value
newtype Yield i a = Yield { unYield :: Yield' i a }

type Await' i a = Yield i a -> a

type Yield' i a = Maybe i -> Await i a -> a

runAwait :: Await i a -> Yield' i a -> a
runAwait (Await a) = a . Yield
{-# INLINE runAwait #-}

runAwait' (Await a) = a . Yield . flip

runYield :: Yield i a -> Maybe i -> Await' i a -> a
runYield (Yield a) i = a i . Await
{-# INLINE runYield #-}

provide :: [i] -> Await i a
provide [] = mempty
provide (x:xs) = Await $ \yield -> unYield yield (Just x) $ provide xs

instance Semigroup (Await i a) where
  a1 <> a2 =
    Await $ \yield ->
      let out Nothing _ = unAwait a2 yield
          out mi      a = unYield yield mi $ a <> a2
       in runAwait a1 out

instance Monoid (Await i a) where
  mempty = Await $ \yield -> unYield yield Nothing mempty

--
-- Monad
--

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
  Pipe { unPipe :: forall r. LR i o m r -> (LR i o m r -> a -> m r) -> m r }

-- | Left and right continuations
data LR i o m r = LR (Await i (m r)) (Yield o (m r))

instance Monad m => Functor (Pipe i o m) where
  fmap f (Pipe c) = Pipe $ \r rest -> c r (\r -> rest r . f)

instance Monad m => Applicative (Pipe i o m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (Pipe i o m) where
  return x = Pipe $ \r f -> f r x
  Pipe f >>= g = Pipe $
    \r h -> f r $ \r' a -> unPipe (g a) r' h

instance MonadTrans (Pipe i o) where
  lift mf = Pipe $ \r f -> mf >>= f r

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
await :: Pipe i o m (Maybe i)
await = Pipe $
  \(LR a y) f -> runAwait a $ \i a' -> f (LR a' y) i
{-# INLINE await #-}


-- | Yield a value to downstream.
--
yield :: o -> Pipe i o m ()
yield i = Pipe $
  \(LR a y) f -> runYield y (Just i) $ \y' -> f (LR a y') ()
{-# INLINE yield #-}


-- | Run pipe to completion.
--
runPipe :: Monad m => Pipe () Void m r -> m r
runPipe pipe = unPipe pipe (LR mempty voidOut) (\_ -> pure)
  where
    voidOut = Yield $ \(Just i) _ -> absurd i


-- | Compose a pipe with another pipe; the return type is that of the second pipe.
--
(.|) :: Applicative m => Pipe i e m () -> Pipe e o m b -> Pipe i o m b
(Pipe f1) .| (Pipe f2) =
  Pipe $ \(LR await yield) rest -> do
    let rest' (LR _ y) = rest $ LR mempty y
        one await = maybe await (\i -> provide [i] <> await)
        right mi await = f2 (LR (one await mi) yield) rest'
        terminate y = runYield y Nothing terminate
    -- The yield right function triggers the right hand pipe
    -- The return method terminates to the right
    f1 (LR await (Yield right)) (\(LR _ y) () -> terminate y)
