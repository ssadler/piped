
module Piped.Resume
  ( createResumableSource
  , createResumableSink
  , runResumableSource
  , runResumableSink
  , ResumableSource(..)
  , ResumableSink(..)
  , ResumableResult(..)
  ) where


import Piped.Internal


-- | Either a resumable source or sink, plus the result of the pipe that finished.
--
data ResumableResult e m a b =
    ResumeSource (ResumableSource e m a b) b
  | ResumeSink   (ResumableSink   e m a b) a


-- | A source that may be resumed
--
newtype ResumableSource o m a b = ResumableSource (Await o m (ResumableResult o m a b))


-- | A sink that may be resumed
--
newtype ResumableSink   i m a b = ResumableSink   (Yield i m (ResumableResult i m a b))


-- | Create a resumable source from a Pipe
--
createResumableSource :: Monad m => Pipe () o m a -> ResumableSource o m a b
createResumableSource (Pipe f) = ResumableSource $
  Await $ f (\l r a -> pure $ ResumeSink (ResumableSink r) a) termLeft


-- | Create a resumable sink from a Pipe
--
createResumableSink :: Monad m => Pipe i Void m b -> ResumableSink i m a b
createResumableSink (Pipe f) = ResumableSink $
   Yield
     (           f resumeLeft termLeft             voidRight)
     (\i left -> f resumeLeft (addLeftover i left) voidRight)
  where
    resumeLeft l _ b  = pure $ ResumeSource (ResumableSource l) b


-- | Run a resumable source
--
runResumableSource :: Monad m => ResumableSource i m a b -> Pipe i Void m b -> m (ResumableResult i m a b)
runResumableSource (ResumableSource source) (Pipe f) =
  f (\l _ b -> pure $ ResumeSource (ResumableSource l) b) source voidRight


-- | Run a resumable sink
--
runResumableSink :: Monad m => Pipe () o m a -> ResumableSink o m a b -> m (ResumableResult o m a b)
runResumableSink (Pipe f) (ResumableSink sink) =
  f (\_ r a -> pure $ ResumeSink (ResumableSink r) a) termLeft sink
