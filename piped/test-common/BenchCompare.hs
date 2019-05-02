{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BangPatterns #-}

module BenchCompare where

import Control.Monad
import Data.Function
import Data.Void
import Prelude hiding (map, foldl, takeWhile, dropWhile, scanl, filter)

import Gauge.Main

import qualified Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as CL
import qualified Piped as P
import qualified Piped.Prelude as P


benchCompare = bgroup "compare"
  [ bgroup "n_stages" $
      [ compare (show (n+2), p_stages n, c_stages n) | n <- [0, 2, 4, 6]
      ]
  , bgroup  "mixed_optimised" $
      [ compare ("map",           p_map,           c_map)
      , compare ("foldl",         p_foldl,         c_foldl)
      , compare ("takeall", p_takeall, c_takeall)
      , compare ("dropall", p_dropall, c_dropall)
      , compare ("scanl",   p_scanl,   c_scanl)
      , compare ("filter_all",    p_filter_all,    c_filter_all)
      , compare ("filter_none",   p_filter_none,   c_filter_none)
      , compare ("sinklist",      p_sinklist,      c_sinklist)
      , compare ("sinknull",      p_sinknull,      c_sinknull)
      ]
  , bgroup "mixed_naive" $
      [ compare ("map",           p_n_map,           c_n_map)
      , compare ("foldl",         p_n_foldl,         c_n_foldl)
      , compare ("takeall", p_n_takeall, c_n_takeall)
      , compare ("dropall", p_n_dropall, c_n_dropall)
      , compare ("scanl",   p_n_scanl,   c_n_scanl)
      , compare ("filter_all",    p_n_filter_all,    c_n_filter_all)
      , compare ("filter_none",   p_n_filter_none,   c_n_filter_none)
      , compare ("sinklist",      p_n_sinklist,      c_n_sinklist)
      , compare ("sinknull",      p_n_sinknull,      c_n_sinknull)
      ]
  ]

  where
    iter = 100000
    runC :: (Integer -> C.ConduitT () o IO a) -> Integer -> IO ()
    runC c n = C.runConduit $ (c n >> pure ()) C..| C.sinkNull
    runP :: (Integer -> P.Pipe () o IO a) -> Integer -> IO ()
    runP p n = P.runPipe    $ (p n >> pure ()) P..| P.sinkNull
    compare :: (String, Integer -> P.Pipe () o IO a, Integer -> C.ConduitT () o IO a) -> Benchmark
    compare (name, p, c) = bgroup name $
      [ bench "conduit" $ whnfAppIO (runC c) iter
      , bench "piped"   $ whnfAppIO (runP p) iter
      ]


c_stages s n = source_C n C..| go s C..| C.sinkNull where
  go 0 = ident_C
  go s = ident_C C..| go (s-1)

p_stages s n = source_P n P..| go s P..| P.sinkNull where
  go 0 = ident_P
  go s = ident_P P..| go (s-1)


c_foldl, c_n_foldl :: Integer -> C.ConduitT () () IO Integer
p_foldl, p_n_foldl :: Integer -> P.Pipe () () IO Integer
c_sinklist, c_n_sinklist :: Integer -> C.ConduitT () Void IO [Integer]
p_sinklist, p_n_sinklist :: Integer -> P.Pipe () Void IO [Integer]

-- Optimised

c_map           n = CL.sourceList [0..n] C..| C.map (+1)
c_foldl         n = CL.sourceList [0..n] C..| C.foldl (+) 0
c_takeall       n = CL.sourceList [0..n] C..| C.takeWhile (<n)
c_dropall       n = CL.sourceList [0..n] C..| (C.dropWhile (<n) >> ident_C)
c_scanl         n = CL.sourceList [0..n] C..| C.scanl (+) 0
c_filter_all    n = CL.sourceList [0..n] C..| C.filter (<0)
c_filter_none   n = CL.sourceList [0..n] C..| C.filter (>=0)
c_sinklist      n = CL.sourceList [0..quot n 2] C..| C.sinkList
c_sinknull      n = CL.sourceList [0..n]

p_map           n = P.sourceList [0..n] P..| P.map (+1)
p_foldl         n = P.sourceList [0..n] P..| P.foldl (+) 0
p_takeall       n = P.sourceList [0..n] P..| P.takeWhile (<n)
p_dropall       n = P.sourceList [0..n] P..| (P.dropWhile (<n) >> ident_P)
p_scanl         n = P.sourceList [0..n] P..| P.scanl (+) 0
p_filter_all    n = P.sourceList [0..n] P..| P.filter (<0)
p_filter_none   n = P.sourceList [0..n] P..| P.filter (>=0)
p_sinklist      n = P.sourceList [0..quot n 2] P..| P.sinkList
p_sinknull      n = P.sourceList [0..n]

-- Naive

c_n_map           n = source_C n C..| map_C (+1)
c_n_foldl         n = source_C n C..| foldl_C (+) 0
c_n_takeall       n = source_C n C..| takeWhile_C (<n)
c_n_dropall       n = source_C n C..| (dropWhile_C (<n) >> ident_C)
c_n_scanl         n = source_C n C..| scanl_C (+) 0
c_n_filter_all    n = source_C n C..| filter_C (<0)
c_n_filter_none   n = source_C n C..| filter_C (>=0)
c_n_sinklist      n = source_C (quot n 2) C..| sinkList_C
c_n_sinknull      n = source_C n

p_n_map           n = source_P n P..| map_P (+1)
p_n_foldl         n = source_P n P..| foldl_P (+) 0
p_n_takeall       n = source_P n P..| takeWhile_P (<n)
p_n_dropall       n = source_P n P..| (dropWhile_P (<n) >> ident_P)
p_n_scanl         n = source_P n P..| scanl_P (+) 0
p_n_filter_all    n = source_P n P..| filter_P (<0)
p_n_filter_none   n = source_P n P..| filter_P (>=0)
p_n_sinklist      n = source_P (quot n 2) P..| sinkList_P
p_n_sinknull      n = source_P n



ident_C = C.awaitForever C.yield
source_C n = CL.sourceList [0..n]
map_C f = C.awaitForever $ C.yield . f
awaitMaybe_C d f = C.await >>= maybe d f
foldl_C f = go where
  go (!s) = awaitMaybe_C (pure s) (\i -> go (f s i))
takeWhile_C f = go where
  go = awaitMaybe_C (pure ()) (\i -> if f i then C.yield i >> go else pure ())
dropWhile_C f = go where
  go = awaitMaybe_C (pure ()) (\i -> if f i then go else C.yield i >> ident_C)
scanl_C f = go where
  go (!s) = C.yield s >> awaitMaybe_C (pure ()) (\i -> go $ f s i)
filter_C f = C.awaitForever $ \i -> if f i then C.yield i else pure ()
sinkList_C = awaitMaybe_C (pure []) (\i -> (i:) <$> sinkList_C)


ident_P = P.awaitForever P.yield
source_P n = P.sourceList [0..n]
map_P f = P.awaitForever $ P.yield . f
awaitMaybe_P d f = P.await >>= maybe d f
foldl_P f = go where
  go (!s) = awaitMaybe_P (pure s) (\i -> go (f s i))
takeWhile_P f = go where
  go = awaitMaybe_P (pure ()) (\i -> if f i then P.yield i >> go else pure ())
dropWhile_P f = go where
  go = awaitMaybe_P (pure ()) (\i -> if f i then go else P.yield i >> ident_P)
scanl_P f = go where
  go (!s) = P.yield s >> awaitMaybe_P (pure ()) (\i -> go $ f s i)
filter_P f = P.awaitForever $ \i -> if f i then P.yield i else pure ()
sinkList_P = awaitMaybe_P (pure []) (\i -> (i:) <$> sinkList_P)
