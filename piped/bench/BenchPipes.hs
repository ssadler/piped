{-# LANGUAGE BangPatterns #-}

module BenchPipes where

import Data.Function (fix)

import Control.Pipe
import Control.Pipe.Internal
import Control.Pipe.Extras as P

import Gauge.Main

import Prelude hiding (scanl)
import Debug.Trace


benchPipes = bgroup "pipes"
  [ bgroup "sinkNull" [ bench "monadic" $ whnfAppIO (runSink sinkNullM) 1000
                      , bench "yielding" $ whnfAppIO (runSink sinkNull) 1000
                      ]
  , bgroup "sinkList" [ bench "monadic" $ nfAppIO (runSink sinkListM) 1000
                      , bench "yielding" $ nfAppIO (runSink sinkList) 1000
                      ]

  , bgroup "scanl"    [ bench "monadic" $ nfAppIO bScanlM 10000
                      , bench "yielding" $ nfAppIO bScanlY 10000
                      ]
  ]

source n = mapM_ yield [0..n]

runSink :: Pipe Int Void IO a -> Int -> IO a
runSink sink n =
  runPipe $ source n .| sink

sinkNullM :: Monad m => Pipe i Void m ()
sinkNullM = awaitForever (\_ -> pure ())

sinkListM :: Monad m => Pipe i Void m [i]
sinkListM = let f = awaitMaybe (pure []) (\i -> (i:) <$> f) in f
{-# INLINE sinkListM #-}

bScanlM = runSink $ scanlM (+) 0 .| sinkNull
bScanlY = runSink $ scanl (+) 0 .| sinkNull

scanlM :: Monad m => (a -> b -> a) -> a -> Pipe b a m ()
scanlM f =
  fix $ \next !s ->
    yield s >> awaitJust (\i -> next $ f s i)
{-# INLINE scanlM #-}
