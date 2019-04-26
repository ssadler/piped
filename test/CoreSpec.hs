
module CoreSpec where

import TestUtils


test_cleanup :: TestTree
test_cleanup = testGroup "pure cleanup"
  [
    testCase "pure cleanup left" $ do
      runTest $ \cleanup ->

        let left = Await $ \yield -> do
              o <- runYield yield (Just ()) $ \_ -> pure LT
              cleanup
              pure o
            right = Yield $ \i await -> pure EQ

         in unAwait left right
  , 
    testCase "pure cleanup right" $ do
      runTest $ \cleanup ->

        let left = Await $ \yield -> do
              runYield yield (Just ()) $ \_ -> pure EQ

            right = Yield $ \(Just ()) await -> do
              o <- runAwait await $ \_ _ -> pure GT
              cleanup
              pure o

         in unAwait left right
  ]
  where
    runTest act = do
      ref <- newIORef False
      r <- act $ writeIORef ref True
      assertEqual "Wrong side returned" EQ r
      readIORef ref >>= assertBool "Cleanup happened"

