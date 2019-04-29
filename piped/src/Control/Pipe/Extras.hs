{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module Control.Pipe.Extras where

import Control.Monad.Trans.Resource

import Control.Pipe.Internal

import Data.Void
import Data.Function (fix)
import Prelude hiding (foldl, scanl)


yieldM :: Monad m => Pipe i o m o -> Pipe i o m ()
yieldM = (>>= yield)

map :: Monad m => (i -> o) -> Pipe i o m ()
map f = awaitForever $ yield . f

scanl :: forall i o m. (o -> i -> o) -> o -> Pipe i o m ()
scanl f s = Pipe $
  \rest ->
    fix1 s $
      \next !s l r ->
        runYield r s $ \r -> 
          let end = rest l r ()
           in runAwait l end $ \i l -> next (f s i) l r
{-# INLINE scanl #-}

sourceList :: Monad m => [o] -> Pipe () o m ()
sourceList = foldMap yield

-- | Return all input values as a list.
sinkList :: Pipe i Void m [i]
sinkList = Pipe $
  \rest l r ->
    fix2 l id $
      \go l fxs ->
        let term = rest termLeft r (fxs [])
         in runAwait l term $ \i yield -> go yield $ fxs . (i:)
{-# INLINE sinkList #-}

-- | Consume all values using given function
awaitForever :: Monad m => (i -> Pipe i o m a) -> Pipe i o m ()
awaitForever f = Pipe $
  \rest ->
    fix $
      \next await yield -> 
        runAwait await (rest termLeft yield ()) $
          \i await ->
            unPipe (f i) (\l r _ -> next l r) await yield
{-# INLINE awaitForever #-}

-- | Left fold
foldl :: Monad m => (a -> i -> a) -> a -> Pipe i o m a
foldl f start = Pipe $
  \rest ->
    fix1 start $
      \next !s l r ->
        runAwait l (rest termLeft r s) $
          \i await -> next (f s i) await r
{-# INLINE foldl #-}

-- | Discard all input values
sinkNull :: Monad m => Pipe i Void m ()
sinkNull = Pipe $
  \rest l r -> do
    let term = rest termLeft r ()
        f l = runAwait l term $ \_ -> f
     in f l
{-# INLINE sinkNull #-}

-- | Basically `await >>= maybe ...`.
--
awaitMaybe :: Pipe i o m a -> (i -> Pipe i o m a) -> Pipe i o m a
awaitMaybe def act = Pipe $
  \rest a y ->
    let run (Pipe f) awt = f rest awt y
        term = run def termLeft
     in runAwait a term $ run . act
{-# INLINE awaitMaybe #-}

awaitJust :: Monad m => (i -> Pipe i o m ()) -> Pipe i o m ()
awaitJust act = Pipe $
  \rest l r ->
    let run (Pipe p) l = p rest l r
     in runAwait l (rest termLeft r ()) $ run . act
{-# INLINE awaitJust #-}

takeWhile_ :: Monad m => (i -> Bool) -> Pipe i i m ()
takeWhile_ f = awaitForever $ \i -> if f i then yield i else pure ()

dropWhile_ :: Monad m => (i -> Bool) -> Pipe i i m ()
dropWhile_ f =
  awaitForever $ \i -> if f i then pure () else yield i >> identity_



-- takeWhile_ f = Pipe $
--   \rest ->
--     fix $ \next l r ->
--       runAwait l (rest termLeft r ()) $ \i l ->
--         if f i
--            then runYield r i $ next l
--            else rest l r ()
-- 
-- 
-- dropWhile_ :: Monad m => (i -> Bool) -> Pipe i i m ()
-- dropWhile_ f = Pipe $
--   \rest ->
--     fix $ \next l r ->
--       runAwait l (rest termLeft r ()) $ \i l ->
--         if f i
--            then next l r
--            else unPipe identity_ rest l r

identity_ :: Monad m => Pipe i i m ()
identity_ = awaitForever yield

drop_ :: Monad m => Int -> Pipe i i m ()
drop_ 0 = identity_
drop_ n = awaitJust (\i -> yield i >> drop_ (n-1))

take_ :: Monad m => Int -> Pipe i i m ()
take_ 0 = pure ()
take_ n = awaitJust $ \i -> yield i >> take_ (n-1)

mapM_' :: Monad m => (i -> Pipe i o m o) -> Pipe i o m ()
mapM_' f = awaitForever $ \i -> f i >>= yield

filter_ :: Monad m => (i -> Bool) -> Pipe i i m ()
filter_ f = awaitForever $ \i -> if f i then yield i else pure ()

zip_ :: Monad m => Pipe () o m () -> Pipe () o' m () -> Pipe () (o, o') m ()
zip_ = zipWith_ (,)

zipWith_ :: Monad m => (o -> o' -> a) -> Pipe () o m () -> Pipe () o' m () -> Pipe () a m ()
zipWith_ f (Pipe f1) (Pipe f2) = Pipe go where
  go rest _ r = loop (side f1) (side f2) r
    where
      exit y = rest termLeft y ()
      side f = Await $ \y -> f (\_ y () -> terminate y) termLeft y
      loop a1 a2 yield =
        runAwait a1 (exit yield) $ \i1 a1 ->
          runAwait a2 (exit yield) $ \i2 a2 ->
            runYield yield (f i1 i2) $ \yield ->
              loop a1 a2 yield

concat_ :: Monad m => Pipe [i] i m ()
concat_ = awaitForever $ mapM_ yield

bracketP :: MonadResource m
         => IO b
         -> (b -> IO ())
         -> (b -> Pipe i o m a)
         -> Pipe i o m a
bracketP alloc free inside = Pipe $
  \rest l r -> do
    (key, seed) <- allocate alloc free
    let rest' l r a = release key >> rest l r a
    unPipe (inside seed) rest' l r


-- Utility functions

fix1 :: a -> ((a -> b) -> a -> b) -> b
fix1 a f = fix f a
{-# INLINE fix1 #-}
fix2 a b f = fix f a b
{-# INLINE fix2 #-}
