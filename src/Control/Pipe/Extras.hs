{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE Strict #-}

module Control.Pipe.Extras where

import Control.Pipe.Internal

import Data.Void
import Data.Function (fix)
import Prelude hiding (foldl, scanl)


yieldM :: Monad m => Pipe i o m o -> Pipe i o m ()
yieldM = (>>= yield)

map :: Monad m => (i -> o) -> Pipe i o m ()
map f = awaitForever $ yield . f

scanl :: Monad m => (a -> b -> a) -> a -> Pipe b a m ()
scanl f =
  fix $ \next s ->
    let go i = let s' = f s i in yield s >> next s
     in await >>= maybe (yield s) go

-- | Consume all input and return a list
sinkList :: Monad m => Pipe i Void m [i]
sinkList = await >>= maybe (pure []) (\i -> (i:) <$> sinkList)
{-# INLINE sinkList #-}

-- | Consume all values using given function
awaitForever :: Monad m => (i -> Pipe i o m ()) -> Pipe i o m ()
awaitForever f = Pipe $ flip $
  \rest ->
    fix $
      \next (LR await yield) -> 
        runAwait' await $
          \await' ->
            let lr = LR await' yield
                run i = unPipe (f i) lr $ \lr' () -> next lr'
             in maybe (rest lr ()) run
{-# INLINE awaitForever #-}

foldl :: Monad m => (a -> i -> a) -> a -> Pipe i Void m a
foldl f start = Pipe $
  \lr rest ->
    let next (LR l r) s =
          runAwait' l $ \await' ->
            let lr' = LR await' r
             in maybe (rest lr' s) (next lr' . f s)
     in next lr start
{-# INLINE foldl #-}

sinkNull :: Monad m => Pipe i Void m ()
sinkNull = awaitForever $ \_ -> pure ()

awaitJust :: Monad m => (i -> Pipe i o m ()) -> Pipe i o m ()
awaitJust f = await >>= maybe (pure ()) f

dropWhile_ :: Monad m => (i -> Bool) -> Pipe i i m ()
dropWhile_ f =
  awaitJust (\i -> if f i then dropWhile_ f else yield i >> identity_)
{-# INLINE dropWhile_ #-}

takeWhile_ :: Monad m => (i -> Bool) -> Pipe i i m ()
takeWhile_ f = awaitJust (\i -> if f i then yield i else pure ())

identity_ :: Monad m => Pipe i i m ()
identity_ = awaitForever yield

drop_ :: Monad m => Int -> Pipe i i m ()
drop_ 0 = identity_
drop_ n = awaitJust (\i -> yield i >> drop_ (n-1))
{-# INLINE drop_ #-}

take_ :: Monad m => Int -> Pipe i i m ()
take_ 0 = pure ()
take_ n = awaitJust $ \i -> yield i >> take_ (n-1)
{-# INLINE take_ #-}

mapM_' :: Monad m => (i -> Pipe i o m o) -> Pipe i o m ()
mapM_' f = awaitForever $ \i -> f i >>= yield

filter_ :: Monad m => (i -> Bool) -> Pipe i i m ()
filter_ f = awaitForever $ \i -> if f i then yield i else pure ()


zip_ :: Monad m => Pipe () o m () -> Pipe () o m () -> Pipe () (o, o) m ()
zip_ (Pipe f1) (Pipe f2) = Pipe $
  \(LR _ r) rest -> do

    let loop' (Just i1) a1 (Just i2) a2 yield = do
          runYield yield (Just (i1, i2)) $ \yield' -> do
            runAwait a1 $ \i1' a1' -> do
              runAwait a2 $ \i2' a2' -> do
                loop' i1' a1' i2' a2' yield'
        loop' _ _ _ _ yield = rest (LR mempty yield) ()

        y1 = Yield $ \mi1 a1 ->
              let y2 = Yield $ \mi2 a2 ->
                    loop' mi1 a1 mi2 a2 r
               in f2 (LR mempty y2) (\(LR _ r) () -> terminate r)

        terminate y = runYield y Nothing terminate

     in f1 (LR mempty y1) (\(LR _ r) () -> terminate r)


concat_ :: Monad m => Pipe [i] i m ()
concat_ = do
  awaitForever $ mapM_ yield
