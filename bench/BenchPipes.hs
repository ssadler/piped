
module BenchPipes where

import Control.Pipe
import Control.Pipe.Internal
import Control.Pipe.Extras
import Control.Monad.Identity

import Criterion.Main

benchPipes = bgroup "pipes"
  [ bgroup "sinkNull" [ bench "monadic" $ whnfIO $ runSink sinkNullM 1000
                      , bench "yielding" $ whnfIO $ runSink sinkNull 1000
                      ]
  , bgroup "sinkList" [ bench "monadic" $ whnfIO $ runSink sinkList 1000
                      , bench "yielding" $ whnfIO $ runSink sinkList' 1000
                      ]
  ]


runSink sink n =
  runPipe $ replicateM_ n (yield ()) .| sink

sinkNullM :: Monad m => Pipe i Void m ()
sinkNullM =
  let f = awaitForever $ \_ -> f
   in f

sinkList' :: Pipe i Void m [i]
sinkList' = Pipe $
  \rest l r ->
    let go l xs =
          runAwait l (rest termLeft r (reverse xs)) $ \i yield -> go yield (i:xs)
     in go l []
