{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

-- | Basic functionality, closely mirroring the list / Foldable api from Prelude.
--
-- This module should be imported qualified i.e. `import Piped.Prelude as P`.
-- It also re-exports `Piped`.
--
module Piped.Prelude
  ( module Piped
  , module Piped.Prelude
  ) where

import Control.Monad
import Control.Monad.Trans

import Prelude hiding (foldl, scanl, mapM_, last, take, drop, zipWith)

import Piped
import Piped.Internal


-- | Yield only elements satisfying a predicate.
--
filter :: Monad m => (i -> Bool) -> Pipe i i m ()
filter f = awaitForever $ \i -> if f i then yield i else pure ()
{-# INLINE filter #-}

-- | Yield values while they satisfy a predicate, then return.
--
takeWhile :: Monad m => (i -> Bool) -> Pipe i i m ()
takeWhile f = go where
  go = awaitJust $ \i -> if f i then yield i >> go else leftover i
{-# INLINE takeWhile #-}

-- | Drop values while they satisfy a predicate, then return.
--
--   Note: this does not yield any values, and should be combined with (>>).
--
dropWhile :: Monad m => (i -> Bool) -> Pipe i o m ()
dropWhile f = go where
  go = awaitJust $ \i -> if f i then go else leftover i
{-# INLINE dropWhile #-}

-- | Equivalent to `map id`.
--
identity :: Monad m => Pipe i i m ()
identity = awaitForever yield
{-# INLINE identity #-}

-- | `map` with an accumulator.
--
scanl :: forall i o m. (o -> i -> o) -> o -> Pipe i o m ()
scanl f s = Pipe $
  \rest ->
    fix1 s $
      \next !s l r ->
        runYield r s $ \r -> 
          runAwait l (rest l r ()) $
            \i l -> next (f s i) l r
{-# INLINE scanl #-}

-- | Left fold over input values.
--
foldl :: Monad m => (a -> i -> a) -> a -> Pipe i o m a
foldl f start = Pipe $
  \rest ->
    fix1 start $
      \next !s l r ->
        runAwait l (rest termLeft r s) $
          \i l -> next (f s i) l r
{-# INLINE foldl #-}

-- | Drop n values.
--
--   Note: This will not yield values and should be combined with `>>`
--
drop :: Monad m => Int -> Pipe i o m ()
drop 0 = pure ()
drop n = awaitJust (\_ -> drop $ n-1)
{-# INLINE drop #-}

-- | Take n values.
--
take :: Monad m => Int -> Pipe i i m ()
take 0 = pure ()
take n = awaitJust $ \i -> yield i >> take (n-1)
{-# INLINE take #-}

-- | Map a pure function over input values.
--
map :: Monad m => (i -> o) -> Pipe i o m ()
map f = awaitForever $ yield . f
{-# INLINE map #-}

-- | Map a monadic function over input values.
--
mapM :: Monad m => (i -> m o) -> Pipe i o m ()
mapM f = awaitForever $ \i -> lift (f i) >>= yield
{-# INLINE mapM #-}

-- | Map a monadic function over input values but don't yield anything.
--
mapM_ :: Monad m => (i -> m ()) -> Pipe i () m ()
mapM_ f = awaitForever $ lift . f
{-# INLINE mapM_ #-}

-- | Skip the first value
--
tail :: Monad m => Pipe i i m ()
tail = await >> identity
{-# INLINE tail #-}

-- | Return the last value.
--
last :: Monad m => Pipe i o m (Maybe i)
last = go Nothing where
  go x = awaitMaybe (pure x) (go . Just)
{-# INLINE last #-}

-- | Zip two pipes together.
--
zip :: Monad m => Pipe () o m () -> Pipe () o' m () -> Pipe () (o, o') m ()
zip = zipWith (,)
{-# INLINE zip #-}

-- | Zip two pipes together with a pure function.
--
zipWith :: Monad m => (o -> o' -> a) -> Pipe () o m () -> Pipe () o' m () -> Pipe () a m ()
zipWith f (Pipe f1) (Pipe f2) = Pipe go where
  go rest _ r = loop (side f1) (side f2) r
    where
      exit y = rest termLeft y ()
      side f = Await $ \y -> f (\_ y () -> terminate y) termLeft y
      loop a1 a2 yield =
        runAwait a1 (exit yield) $ \i1 a1 ->
          runAwait a2 (exit yield) $ \i2 a2 ->
            runYield yield (f i1 i2) $ \yield ->
              loop a1 a2 yield

-- | Concatenate foldable inputs to a single stream of outputs.
--
concat :: (Foldable t, Monad m) => Pipe (t i) i m ()
concat = awaitForever $ foldMap yield
{-# INLINE concat #-}
