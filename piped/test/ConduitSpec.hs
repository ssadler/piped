
module ConduitSpec
  ( module ConduitSpec
  , C.runConduit
  , P.runPipe
  ) where

import qualified Data.Conduit as C
import qualified Piped as P
import Control.Monad.State

import Data.Void
import Test.Tasty
import Test.Tasty.HUnit

import PipeLike
import BenchCompare



type TestPipe p a = p () Void (StateT [Int] IO) a

test_units :: TestTree
test_units = testGroup "Conduit spec" $
  [ t "simple" simple simple
  , t "yield await" yieldAwait yieldAwait
  , t "sinkList" sinkList_ sinkList_
  , t "termOrder1" termOrder1 termOrder1
  , t "termOrder2" termOrder1 termOrder1
  , t "resursive pipe" recursive recursive
  ]
    ++ [t ("compare:" ++ s) c p | (s, p, c) <- comparePipes]

    where
      t s p c = testCompare s (p 2) (c 2)

      testCompare :: (Eq a, Show a)
                  => String -> TestPipe C.ConduitT a -> TestPipe P.Pipe a -> TestTree
      testCompare s c p = testCase s $ do
        cr <- runStateT (C.runConduit c) []
        pr <- runStateT (P.runPipe    p) []
        pr @?= cr



alloc n act = do
  modify $ (++ [n*(-1)])
  act
  modify $ (++ [n])

simple _ = pure 1

yieldAwait _ = alloc 2 (yield 1) .| alloc 1 await

sinkList_ _ = mapM_ yield [True, False] .| sinkList

termOrder1 _ = pure () .| (await >> pure ()) .| (await >> pure True)

termOrder2 _ = alloc 1 await >> alloc 2 await

recursive _ = go 2 where
  go 0 = await >> pure True
  go n = yield () .| (await >> go (n-1))
