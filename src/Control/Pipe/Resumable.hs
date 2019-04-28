
module Control.Pipe.Resumable where

import Control.Pipe.Internal

import Data.Void


data ResumePipe e m a b =
    LeftPipe  b (Await e m (ResumePipe e m a b))
  | RightPipe a (Yield e m (ResumePipe e m a b))


-- | Run two pipes and return the one that didn't finish.
--
runResumable :: Monad m
                 => Pipe () e m a -> Pipe e Void m b
                 -> m (ResumePipe e m a b)
runResumable (Pipe f1) (Pipe f2) = do
   f1 resumeRight termLeft $
     Yield
       (           f2 resumeLeft termLeft         sinkVoid)
       (\i left -> f2 resumeLeft (prepend i left) sinkVoid)
  where
    prepend i left = Await $ \y -> unYield y i left
    resumeRight _ yield a = pure $ RightPipe a yield
    resumeLeft await _ b  = pure $ LeftPipe b await
