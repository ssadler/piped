
module Piped.Extras where

-- | Convenience methods, plus 'bracketPipe'.
--

import Data.Function

import Piped.Internal


-- | Yield all values from a foldable data structure such as a list.
--
sourceList :: (Monad m, Foldable t) => t o -> Pipe () o m ()
sourceList = foldMap yield
{-# INLINE sourceList #-}

-- | Return all input values as a list.
--
sinkList :: Pipe i Void m [i]
sinkList = Pipe $
  \rest l r ->
    fix2 l id $
      \go l fxs ->
        let term = rest termLeft r $ fxs []
         in runAwait l term $ \i l -> go l $ fxs . (i:)
{-# INLINE sinkList #-}

-- | Consume all values using given function
--
awaitForever :: Monad m => (i -> Pipe i o m a) -> Pipe i o m ()
awaitForever f = Pipe $
  \rest ->
    fix $
      \next await yield -> 
        runAwait await (rest termLeft yield ()) $
          \i await ->
            unPipe (f i) (\l r _ -> next l r) await yield
{-# INLINE awaitForever #-}

-- | Discard all input values
--
sinkNull :: Monad m => Pipe i Void m ()
sinkNull = Pipe $
  \rest l r -> do
    let term = rest termLeft r ()
        f l = runAwait l term $ \_ -> f
     in f l
{-# INLINE sinkNull #-}

-- | Basically `await >>= maybe ...`, but avoids the conversion to a maybe type.
--
awaitMaybe :: Pipe i o m a -> (i -> Pipe i o m a) -> Pipe i o m a
awaitMaybe def act = Pipe $
  \rest l r ->
    runAwait l (unPipe def rest termLeft r)
               (\i l -> unPipe (act i) rest l r)
{-# INLINE awaitMaybe #-}


-- | Act on a value, if there is no value then return ().
--
awaitJust :: Monad m => (i -> Pipe i o m ()) -> Pipe i o m ()
awaitJust act = Pipe $
  \rest l r ->
    runAwait l (rest termLeft r ()) $
      \i l -> unPipe (act i) rest l r
{-# INLINE awaitJust #-}


-- | Yield the result of a monadic action.
--
yieldM :: Monad m => Pipe i o m o -> Pipe i o m ()
yieldM = (>>= yield)
{-# INLINE yieldM #-}


-- | This is the same as [bracketP](http://hackage.haskell.org/package/conduit-1.3.1.1/docs/Data-Conduit.html#v:bracketP) from Conduit, however it's not specialised to MonadResource.
--
bracketPipe :: Monad m => m b -> (b -> m ()) -> (b -> Pipe i o m a) -> Pipe i o m a
bracketPipe alloc free inside = Pipe $
  \rest l r -> do
    b <- alloc
    let rest' l r a = free b >> rest l r a
    unPipe (inside b) rest' l r
