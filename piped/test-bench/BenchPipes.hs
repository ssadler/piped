{-# LANGUAGE BangPatterns #-}

module BenchPipes where

import Data.Function (fix)

import Control.Monad

import Piped
import Piped.Internal
import Piped.Prelude as P

import Gauge.Main

import Prelude hiding (scanl, foldl, dropWhile, takeWhile)


benchPipes = bgroup "pipes"
  [ bgroup "sinkNull"  [ bench "monadic" $ whnfAppIO (runSink sinkNullM) 1000
                       , bench "yielding" $ whnfAppIO (runSink sinkNull) 1000
                       ]
  , bgroup "sinkList"  [ bench "monadic" $ nfAppIO (runSink sinkListM) 1000
                       , bench "yielding" $ nfAppIO (runSink sinkList) 1000
                       ]

  , bgroup "foldl"     [ bench "monadic" $ nfAppIO bFoldlM 10000
                       , bench "yielding" $ nfAppIO bFoldlY 10000
                       ]

  , bgroup "scanl"     [ bench "monadic" $ nfAppIO bScanlM 10000
                       , bench "yielding" $ nfAppIO bScanlY 10000
                       ]

  , bgroup "dropWhile" [ bench "monadic" $ nfAppIO (bDoWhile dropWhile) 10000
                       , bench "yielding" $ nfAppIO (bDoWhile dropWhileY) 10000
                       ]

  , bgroup "takeWhile" [ bench "monadic" $ nfAppIO (bDoWhile takeWhile) 10000
                       , bench "yielding" $ nfAppIO (bDoWhile takeWhileY) 10000
                       ]
  , bgroup "identity"  [ bench "monadic" $ nfAppIO (bIdentity identity) 1000
                       , bench "yielding" $ nfAppIO (bIdentity identityY) 1000
                       ]
  -- , bgroup "maybes"    [ bench "awaitMaybe" $ nfAppIO (bMaybes $ awaitMaybe mempty) 10000
  --                      , bench "awaitJust" $ nfAppIO (bMaybes $ awaitJust) 10000
  --                      ]
  , bgroup "map"       [ bench "monadic" $ nfAppIO (bSimple $ P.map (+1)) 10000
                       , bench "yielding" $ nfAppIO (bSimple $ pmapMon (+1)) 10000
                       ]
  , bgroup "filter"    [ bench "monadic" $ nfAppIO (bSimple $ P.filter even) 10000
                       , bench "yielding" $ nfAppIO (bSimple $ pFilter even) 10000
                       ]
  ]

source :: Int -> Pipe () Int IO ()
source n = Prelude.mapM_ yield [0..n]

runSink :: Pipe Int Void IO a -> Int -> IO a
runSink sink n =
  runPipe $ source n .| sink

sinkNullM :: Monad m => Pipe i Void m ()
sinkNullM = awaitForever (\_ -> pure ())

sinkListM :: Monad m => Pipe i Void m [i]
sinkListM = let f = awaitMaybe (pure []) (\i -> (i:) <$> f) in f
{-# INLINE sinkListM #-}

bFoldl p n = runPipe $ source n .| (p (+) 0 >> pure ())
bFoldlM n = runPipe $ source n .| (foldl' (+) 0 >> pure ())
bFoldlY n = runPipe $ source n .| (foldl (+) 0 >> pure ())

foldl' :: Monad m => (a -> i -> a) -> a -> Pipe i o m a
foldl' f s =
  let next !s = awaitMaybe (pure s) (next . f s) in next s
{-# INLINE foldl' #-}
-- foldl' :: Monad m => (a -> i -> a) -> a -> Pipe i o m a
-- foldl' f start = Pipe $
--   \rest ->
--     fix1 start $
--       \next !s l r ->
--         runAwait l (rest termLeft r s) $
--           \i l -> next (f s i) l r
-- {-# INLINE foldl' #-}

bScanlM = runSink $ scanlM (+) 0 .| sinkNull
bScanlY = runSink $ scanl (+) 0 .| sinkNull

scanlM :: Monad m => (a -> b -> a) -> a -> Pipe b a m ()
scanlM f =
  fix $ \next !s ->
    yield s >> awaitJust (\i -> next $ f s i)
{-# INLINE scanlM #-}

bDoWhile p n = runPipe $ source n .| p (\_ -> True) .| sinkNull

dropWhileY :: Monad m => (i -> Bool) -> Pipe i i m ()
dropWhileY f = Pipe $
  \rest ->
    fix $ \next l r ->
      runAwait l (rest termLeft r ()) $ \i l ->
        if f i
           then next l r
           else let l' = addLeftover i l
                 in unPipe identity rest l' r
{-# INLINE dropWhileY #-}

takeWhileY :: Monad m => (i -> Bool) -> Pipe i i m ()
takeWhileY f = Pipe $
  \rest ->
    fix $ \next l r ->
      runAwait l (rest termLeft r ()) $ \i l ->
        if f i
           then runYield r i $ next l
           else rest l r ()
{-# INLINE takeWhileY #-}

bIdentity :: Pipe Int Int IO () -> Int -> IO ()
bIdentity p n = runPipe $ source n .| ((iterate (.| p) p) !! 10) .| sinkNull

identityY :: Pipe i i m ()
identityY = Pipe $
  \rest ->
    fix $ \next l r ->
      runAwait l (rest termLeft r ()) $ \i l ->
        runYield r i $ next l
{-# INLINE identityY #-}

bMaybes p n = runPipe $ source n .| go
  where go = p $ (\_ -> go :: Pipe Int Void IO ())

bSimple p n = runPipe $ source n .| p .| sinkNull

pFilter :: (i -> Bool) -> Pipe i i m ()
pFilter f = Pipe $
  \rest -> fix $
    \next l r ->
      runAwait l (rest termLeft r ()) $
        \i l -> if f i
                   then runYield r i $ \r -> next l r
                   else next l r
{-# INLINE pFilter #-}

pmapMon :: (i -> o) -> Pipe i o m ()
pmapMon f = Pipe $
  \rest -> fix $
    \next l r ->
      runAwait l (rest termLeft r ()) $
        \i l -> runYield r (f i) $ \r -> next l r
