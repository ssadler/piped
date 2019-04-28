
module Control.Pipe.Resumable where

import Control.Pipe.Internal

import Data.Void


-- | Type representing a pipe that can be resumed, 
--   and a result from the other pipe that finished.
--
--   This data structure is conveniently, and neccesarily,
--   recursive because of the unification of the pipe
--   return type with the await and yield socket
--   types.
--
data ResumePipe e m a b =
    LeftPipe  (Await e m (ResumePipe e m a b)) b
  | RightPipe (Yield e m (ResumePipe e m a b)) a


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
    resumeRight _ yield a = pure $ RightPipe yield a
    resumeLeft await _ b  = pure $ LeftPipe await b
