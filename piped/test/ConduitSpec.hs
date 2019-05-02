
module ConduitSpec where

import qualified Data.Conduit as C
import qualified Piped as P
import Control.Monad.State

import Test.Tasty.HUnit

import PipeLike as PL


-- | Below tests polymorphic pipeline definitions using Piped and Conduit.
--   The application of () is because for some reason GHC will allow 
--   polymorphic definitions without signatures if an argument is passed,
--   event a useless one.

test_1 = test "demand driven" p p
  where
    p _ = undefined .| (pure ())

test_2 = test "sinkList correct order" p p
  where
    p _ = mapM_ yield [True, False] .| sinkList

test_3 = test "dropWhile doesn't consume" p p
  where
    p _ = sourceList [1..10] .| (PL.dropWhile (<5) >> leftover (-1) >> sinkList)

test_4 = test "takeWhile doesn't drop values" p p
  where
    p _ = sourceList [1..10] .| (PL.takeWhile (<5) >> PL.map (+1)) .| sinkList

test_44 = test "take doesn't drop values" p p
  where
    p _ = sourceList [1..10] .| PL.take 5 .| sinkList

test_45 = test "drop doesn't yield values" p p
  where
    p _ = sourceList [1..10] .| (PL.drop 5 >> PL.map (+1)) .| sinkList

test_5 = test "recursive pipes don't do funny things" p p
  where
    p _ = go 2
    go 0 = await >> pure True
    go n = yield () .| (await >> go (n-1))

test_6 = test "foldl works as expected" p p
  where
    p _ = sourceList [True, False, True] .| PL.foldl (flip (:)) []


test s p c = testCompare s (p ()) (c ())
testCompare s c p = testCase s $ join $ (@?=) <$> P.runPipe p <*> C.runConduit c
