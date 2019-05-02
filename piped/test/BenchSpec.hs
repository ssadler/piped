
module BenchSpec where

import Data.Conduit as C
import Data.Conduit.Combinators as CL
import Piped as P

import Test.Tasty
import Test.Tasty.HUnit

import BenchCompare


test_bench :: TestTree
test_bench = testGroup "Verify that benchmark pipes output same results" $
  [ testGroup "n_stages" $
      [ -- compare (show (n+2), p_stages n, c_stages n) | n <- [0, 2, 4, 6]
      ]
  , testGroup  "mixed_optimised" $
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
  , testGroup "mixed_naive" $
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
    n = 100

    compare :: (Eq o, Show o) => (String, Integer -> Pipe () o IO a, Integer -> ConduitT () o IO a) -> TestTree
    compare (s, p, c) = testCase s $ do
      cr <- runConduit ((c n >> pure ()) C..| CL.sinkList)
      pr <- runPipe    ((p n >> pure ()) P..| P.sinkList)
      pr @?= cr
