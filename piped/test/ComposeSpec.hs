
module ComposeSpec where

import Piped
import Piped.Compose

import TestUtils


test_4 = testCase "composeSupply init correct order" $ do
  let o = runPipeS [] $ composeSupply (alloc 1 >> yield 1) (alloc 2 >> await)
  o @?= (Just 1, [1, 2])

test_5 = testCase "composeSupplyEither left run first" $ do
  let o = runPipeI $ composeSupplyEither (pure 1) (pure 2)
  o @?= 1

test_55 = testCase "composeSupplyEither right can return" $ do
  let o = runPipeI $ composeSupplyEither (yield () >> pure 1) (pure (2::Int))
  o @?= 2

test_6 = testCase "composeDemand left doesnt run" $
  runPipe $ composeDemand (error "failure") (pure ())

test_8 = testCase "composeDemandEither right run first" $ do
  let o = runPipeI $ composeDemandEither (pure 1) (pure 2)
  o @?= 2

test_88 = testCase "composeDemandEither left can return" $ do
  let o = runPipeI $ composeDemandEither (pure 1) (await >> pure 2)
  o @?= 1
