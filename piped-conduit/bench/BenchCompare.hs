{-# LANGUAGE AllowAmbiguousTypes #-}

module BenchCompare where

import Prelude hiding (foldl, scanl)
import Control.Monad.Trans
import PipeLike
import Gauge.Main
import Debug.Trace
import System.IO.Unsafe

import qualified Control.Pipe as P
import qualified Control.Pipe.Conduit as P
import qualified Data.Conduit as C

import Control.Concurrent (threadDelay)


source n = mapM_ yield [0..n]

iter = 100000


benchCompare = bgroup "compare" $ compare <$> pipes
  where
    compare (name, p, c) = bgroup name $
      [ bench "conduit" $ whnfAppIO (runConduit . c) iter
      , bench "piped"   $ whnfAppIO (runPipe . p) iter
      ]

pipes :: [(String, Integer -> Pipe () Void IO (), Integer -> ConduitT () Void IO ())]
pipes =
    [ ("foldl",     pFoldl,     pFoldl)
    , ("scanl",     pScanl,     pScanl)
    , ("idents",    pIdents,    pIdents)
    ]

-- For some reason, if a decleration has an argument it can be
-- polymorphic without a type signature

pFoldl n = source (n::Integer) .| (foldl (+) 0 >>= lotto)

pScanl n = source n .| scanl (+) 0 .| sinkNull

pIdents n = source n .| compose 10 .| sinkNull
  where
    compose 0 = ident ()
    compose n = ident () .| compose (n-1)

ident () = awaitForever yield

lotto 20000 = liftIO $ print "jackpot!"
lotto _ = pure ()

