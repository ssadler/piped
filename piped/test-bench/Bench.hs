
module Main where

import BenchShow
import Gauge.Main
import Data.List.Split
import System.Environment (getArgs)

import BenchYields
import BenchPipes
import BenchCompare

import Piped
import Conduit


main = do
  args <- getArgs
  if take 1 args == ["charts"]
     then renderCharts $ args !! 1
     else do
       defaultMain
        [ benchPipes
        , benchYields
        , benchCompare "vs_typical" comparePipes
        , benchCompare "vs_stages" compareIdents
        ]


renderCharts file = do
  -- report file Nothing defaultConfig { presentation = Fields }
  
  graph file "vs_typical" $
    defaultConfig
      { classifyBenchmark = \name ->
          case splitOn "/" name of
            ["vs_typical", b, c]   -> Just (c, b)
            _                      -> Nothing
      }
  
  graph file "vs_stages" $
    defaultConfig
      { classifyBenchmark = \name ->
          case splitOn "/" name of
            ["vs_stages", b, c]   -> Just (c, b)
            _                      -> Nothing
      }
  
  graph file "yielding_vs_monadic" $
    defaultConfig
      { classifyBenchmark = \name ->
          case splitOn "/" name of
            ["pipes", b, c]   -> Just (c, b)
            _                 -> Nothing
      }


benchCompare name pipes = bgroup name $ compare <$> pipes
  where
    iter = 1000000
    compare (name, p, c) = bgroup name $
      [ bench "conduit" $ whnfAppIO (runConduit . c) iter
      , bench "piped"   $ whnfAppIO (runPipe    . p) iter
      ]
