
module Main where

import BenchShow
import Gauge.Main
import Control.Exception
import Data.List (sortOn)
import Data.List.Split
import System.Environment (getArgs)

import BenchYields
import BenchPipes
import BenchCompare


main = do
  args <- getArgs
  if take 1 args == ["charts"]
     then renderCharts $ args !! 1
     else do
       defaultMain
        [ benchPipes
        , benchYields
        , benchCompare
        ]


renderCharts file = do
  stdgraph "mixed_naive" $ drop 1
  stdgraph "n_stages" $ drop 1
  stdgraph "mixed_optimised" $ drop 1
  
  graph file "yielding_vs_monadic" $
    defaultConfig
      { classifyBenchmark = \name ->
          case splitOn "/" name of
            ["pipes", b, c] -> Just (c, b)
            _               -> Nothing
      }

  where
    stdgraph name f =
      handle (\e -> print (e::SomeException)) $
        graph file name $
          defaultConfig
            { classifyBenchmark = \s ->
                case f (splitOn "/" s) of
                  [a, b, c] | a == name -> Just (c, b)
                  _                     -> Nothing
            , title = Just name
            , selectGroups = sortOn $ (/="piped") . fst
            }
