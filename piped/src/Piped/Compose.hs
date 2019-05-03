
-- | Pipe composition can be supply or demand driven, which refers to how the pipeline is
--   initiated. Supply driven initiates the left side first.
--
--   When a pipe terminates, one side must return a value. Unless the pipeline is run
--   in resumable mode, the other sides may never "complete" in that they do not
--   return a value. If they allocate resources, MonadResource or similar should be
--   used to handle cleanup (see 'Piped.Extras.bracketPipe').
--
--   Right hand termination works by indicating that no more values are available.
--
--   In a mode where either side can return a value, no special termination logic is invoked,
--   execution ends the first time a side returns.
--
--   Left termination is not provided; since there is no way to indicate to the left side
--   that the right has terminated, and potential solutions involve discarding values.
--   However, the "either" modes are also suitable for left termination.
--

module Piped.Compose
  (
  -- ** Operators
  --
    (.|)
  , (|.)

  -- ** Demand driven
  --
  -- | The right hand side is run first. The left hand side is only
  --   invoked by calling `yield`.
  --
  , composeDemand
  , composeDemandEither
  
  -- ** Supply driven
  --
  -- | The left hand side is run first. If it returns immediately, the right
  --   hand side is invoked only in the case of `composeSupply`.
  --   
  , composeSupply
  , composeSupplyEither
  ) where

import Piped.Internal


-- | Demand driven; same as 'composeDemand
--
(.|) :: Monad m => Pipe i e m () -> Pipe e o m b -> Pipe i o m b
(.|) = composeDemand
{-# INLINE (.|) #-}


-- | Supply driven; same as 'composeSupplyLeft'
--
(|.) :: Monad m => Pipe i e m a -> Pipe e o m a -> Pipe i o m a
(|.) = composeSupplyEither
{-# INLINE (|.) #-}


-- | The right side is run first, only the right side may return a value.
--
composeDemand :: Monad m => Pipe i e m () -> Pipe e o m b -> Pipe i o m b
composeDemand (Pipe f1) (Pipe f2) =
  Pipe $ \rest l r ->
    f2 (\_ -> rest termLeft) (Await $ f1 (\_ r _ -> terminate r) l) r
{-# INLINE composeDemand #-}


-- | The right side is run first, either side may return a value.
--
composeDemandEither :: Monad m => Pipe i e m a -> Pipe e o m a -> Pipe i o m a
composeDemandEither (Pipe f1) (Pipe f2) =
  Pipe $ \rest l r ->
    f2 (\_ -> rest termLeft) (Await $ f1 (\l -> rest l . termRight) l) r
{-# INLINE composeDemandEither #-}


-- | The left side is run first, only the right side may return a value.
--
composeSupply :: Monad m => Pipe i e m () -> Pipe e o m b -> Pipe i o m b
composeSupply (Pipe f1) (Pipe f2) =
  Pipe $ \rest l r ->
    f1 (\_ r _ -> terminate r) l $
      Yield
        (           f2 (\_ -> rest termLeft) termLeft             r)
        (\i left -> f2 (\_ -> rest termLeft) (addLeftover i left) r)
{-# INLINE composeSupply #-}


-- | The left side is run first, either side may return a value.
--
composeSupplyEither :: Monad m => Pipe i e m a -> Pipe e o m a -> Pipe i o m a
composeSupplyEither (Pipe f1) (Pipe f2) =
  Pipe $ \rest l r ->
    f1 (\l r -> rest l $ termRight r) l $
      Yield
        (           f2 (\_ -> rest termLeft) termLeft             r)
        (\i left -> f2 (\_ -> rest termLeft) (addLeftover i left) r)
{-# INLINE composeSupplyEither #-}
