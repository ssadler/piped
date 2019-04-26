

import Criterion.Main


main = defaultMain [
  bgroup "maybes" [ bench "maybe"  $ whnf runMaybes 1000
                  , bench "term"  $ whnf runTerm 1000
                  -- , bench "stall"  $ whnf runStall 1000
                  -- , bench "term4"  $ whnf runTerm4 1000
                  ]
  ]


newtype Await i a = Await { unAwait :: Yield i a -> a }
newtype Yield i a = Yield { unYield :: Maybe i -> Await i a -> a }


runMaybes n = unAwait (left n) right
  where
    left n =
      Await $ \yield -> do
        let v = if n == 0 then Nothing else Just ()
         in unYield yield v $ left (n-1)
      
    right = Yield $ \mi await -> maybe () (\_ -> unAwait await right) mi


--------------------------

-- faster but not by much

newtype Await2 i a = Await2 { unAwait2 :: Await2' i a }

data Yield2 i a = Yield2
  { terminate :: a
  , unYield2 :: Yield2' i a
  }

type Await2' i a = Yield2 i a -> a
type Yield2' i a = i -> Await2 i a -> a

runTerm n = unAwait2 (left n) right
  where
    left n =
      Await2 $ \yield ->
        if n == 0 then terminate yield
                  else unYield2 yield () $ left (n-1)
      
    right = Yield2 () $ \() await -> unAwait2 await right


--------------------------

-- fastest but useless

newtype Await3 i a = Await3 { unAwait3 :: Yield3 i a -> a }
newtype Yield3 i a = Yield3 { unYield3 :: i -> Await3 i a -> a }

runStall n = unAwait3 (left n) right
  where
    left n =
      Await3 $ \yield -> do
        if n == 0 then ()
                  else unYield3 yield () $ left (n-1)
      
    right = Yield3 $ \() await -> unAwait3 await right



--------------------------


-- worst of the bunch

newtype Await4 i a = Await4 { unAwait4 :: Await4' i a }

data Yield4 i a = Yield4
  { unYield4 :: Yield4' i a
  }

type Await4' i a = Yield4 i a -> a
type Yield4' i a = (a -> (i -> Await4 i a -> a) -> a) -> a

runTerm4 n = unAwait4 (left n) right
  where
    left n =
      Await4 $ \yield ->
        unYield4 yield $
          \term onval -> 
            if n == 0 then term else onval () (left (n-1))
      
    right = Yield4 $ \f -> f () $ \() await -> unAwait4 await right

