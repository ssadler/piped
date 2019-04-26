
module MonadicSpec where

import TestUtils

import Control.Monad
import Control.Monad.Trans
import Debug.Trace


test_connectPipes :: TestTree
test_connectPipes = testCase "connect pipes" $ do
  r <- runPipe $ yield 3 .| await
  r @?= Just 3

test_cleanup :: TestTree
test_cleanup = testGroup "Cleanup"
  [
    runTest 1 "Right hand does run" $
      \alloc -> pure () .| (alloc await >> pure (Just EQ))
  ,
    runTest 1 "Right hand returns and performs cleanup" $
      \alloc -> (yield EQ >> pure ()) .| alloc await
  -- ,
  --   runTest 1 "Right hand returns and left hand performs cleanup" $
  --     \alloc -> alloc (yield LT >> pure (Just LT)) .| (await >> pure (Just EQ))
  -- ,
  --   runTest 1 "Left hand returns and right hand performs cleanup" $
  --     \alloc -> (yield GT >> pure (Just EQ)) .| alloc (await >> await)
  ]
  where
    runTest :: Int -> String -> ((Pipe i o IO r -> Pipe i o IO r)
            -> Pipe () Void IO (Maybe Ordering)) -> TestTree
    runTest n s act = testCase s $ do
      ref <- newIORef 0
      let withResource :: Pipe i o IO r -> Pipe i o IO r
          withResource act = do
            act <* (liftIO $ modifyIORef ref (+1))
      
      r <- runPipe $ act withResource
      assertEqual "Wrong pipe returned" (Just EQ) r
      o <- readIORef ref
      assertEqual "Wrong number of deallocations" n o

-- unit_mapOutput :: IO ()
-- unit_mapOutput = do
  -- r <- runPipe $ (show <$> await) .| pure 3
  -- r @?= 3

-- test confirms that parent bind only happens when lift is invoked
--
-- newtype ToyMonad a = ToyMonad a
--   deriving (Functor)
-- 
-- instance Applicative ToyMonad where
--   pure = ToyMonad
--   (<*>) = ap
-- 
-- instance Monad ToyMonad where
--   return = pure
--   (ToyMonad a) >>= f = f (trace "bind" a)
-- 
-- bindTest = do
--   let ToyMonad a = runDag () $ do
--         a <- pure ()
--         b <- pure (a, a)
--         c <- lift $ pure 1
--         pure $ replicate c (b, b)
--   print a
