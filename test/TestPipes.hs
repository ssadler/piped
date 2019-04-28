{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}

module TestPipes
  ( module TestPipes
  , C.ConduitT
  , P.Pipe
  , C.runConduit
  , P.runPipe
  , Void
  , runStateT
  ) where

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Control.Pipe as P
import qualified Control.Pipe.Internal as P
import Control.Monad.State

import Data.Void

class Monad m => PipeLike p i o m where
  await :: p i o m (Maybe i)
  yield :: o -> p i o m ()
  (.||) :: p i e m () -> p e o m b -> p i o m b
  infixr 5 .||
  sinkList :: p i Void m [i]

instance Monad m => PipeLike C.ConduitT i o m where
  await = C.await
  yield = C.yield
  (.||) = (C..|)
  sinkList = C.sinkList

instance Monad m => PipeLike P.Pipe i o m where
  await = P.await
  yield = P.yield
  (.||) = (P..|)
  sinkList = P.sinkList
