
module Piped.Compose
  ( (.|)
  , (|.)
  , composeDemand
  , composeDemandBoth
  , composeSupply
  , composeSupplyBoth
  ) where

import Piped.Internal


-- | Compose two pipes together in demand driven mode (right pipe is run first). Only the right pipe can return a value
--
(.|) :: Monad m => Pipe i e m () -> Pipe e o m b -> Pipe i o m b
(.|) = composeDemand
{-# INLINE (.|) #-}


-- | Compose two pipes together in supply driven mode (the left pipe is run first). Only the right pipe can return a value
--
(|.) :: Monad m => Pipe i e m () -> Pipe e o m b -> Pipe i o m b
(|.) = composeSupply
{-# INLINE (|.) #-}


-- | Compose two pipes together in demand driven mode (right pipe is run first). Only the right pipe can return a value
--
composeDemand :: Monad m => Pipe i e m () -> Pipe e o m b -> Pipe i o m b
composeDemand (Pipe f1) (Pipe f2) =
  Pipe $ \rest l r ->
    f2 (\_ -> rest termLeft) (Await $ f1 (\_ r _ -> terminate r) l) r
{-# INLINE composeDemand #-}


-- | Compose two pipes together in demand driven mode (right pipe is run first). Either pipe may return a value
--
composeDemandBoth :: Monad m => Pipe i e m a -> Pipe e o m a -> Pipe i o m a
composeDemandBoth (Pipe f1) (Pipe f2) =
  Pipe $ \rest l r ->
    f2 (\_ -> rest termLeft) (Await $ f1 (\l -> rest l . termRight) l) r
{-# INLINE composeDemandBoth #-}


-- | Compose two pipes together in supply driven mode (the left pipe is run first). Only the right pipe can return a value
--
composeSupply :: Monad m => Pipe i e m () -> Pipe e o m b -> Pipe i o m b
composeSupply (Pipe f1) (Pipe f2) =
  Pipe $ \rest l r ->
    f1 (\_ r _ -> terminate r) l $
      Yield
        (           f2 (\_ -> rest termLeft) termLeft                 r)
        (\i left -> f2 (\_ -> rest termLeft) (addLeftover i left)  r)
{-# INLINE composeSupply #-}


-- | Compose two pipes together in supply driven mode (the left pipe is run first). Either pipe may return a value
--
composeSupplyBoth :: Monad m => Pipe i e m a -> Pipe e o m a -> Pipe i o m a
composeSupplyBoth (Pipe f1) (Pipe f2) =
  Pipe $ \rest l r ->
    f1 (\l -> rest l . termRight) l $
      Yield
        (           f2 (\_ -> rest termLeft) termLeft                 r)
        (\i left -> f2 (\_ -> rest termLeft) (addLeftover i left)  r)
{-# INLINE composeSupplyBoth #-}
