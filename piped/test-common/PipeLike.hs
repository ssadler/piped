{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE LambdaCase #-}

module PipeLike
  ( module PipeLike
  , C.ConduitT
  , P.Pipe
  , C.runConduit
  , P.runPipe
  , Void
  ) where

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C
import qualified Data.Conduit.List as C (sourceList)
import qualified Piped as P
import qualified Piped.Prelude as P

import Data.Void


class (Monad m, Monad (p i o m)) => PipeLike p i o m where
  await :: p i o m (Maybe i)
  yield :: o -> p i o m ()
  (.|) :: p i e m () -> p e o m b -> p i o m b
  infixr 5 .|
  sourceList :: [o] -> p () o m ()
  sinkList :: p i Void m [i]
  foldl :: (a -> i -> a) -> a -> p i o m a
  scanl :: (a -> b -> a) -> a -> p b a m ()
  sinkNull :: p i Void m ()
  awaitForever :: (i -> p i o m ()) -> p i o m ()
  take_ :: Int -> p i i m ()
  map_ :: (a -> b) -> p a b m ()
  mapMC :: (i -> m o) -> p i o m ()
  mapMC_ :: (i -> m ()) -> p i () m ()
  takeWhile_ :: (i -> Bool) -> p i i m ()

instance Monad m => PipeLike C.ConduitT i o m where
  await = C.await
  yield = C.yield
  (.|) = (C..|)
  sourceList = C.sourceList
  sinkList = C.sinkList
  foldl = C.foldl
  scanl = C.scanl
  sinkNull = C.sinkNull
  awaitForever = C.awaitForever
  take_ = C.take
  takeWhile_ = C.takeWhile
  map_ = C.map
  mapMC = C.mapM
  mapMC_ = C.mapM_

instance Monad m => PipeLike P.Pipe i o m where
  await = P.await
  yield = P.yield
  (.|) = (P..|)
  sourceList = P.sourceList
  sinkList = P.sinkList
  foldl = P.foldl
  scanl = P.scanl
  sinkNull = P.sinkNull
  awaitForever = P.awaitForever
  take_ = P.take
  takeWhile_ = P.takeWhile
  map_ = P.map
  mapMC = P.mapM
  mapMC_ = P.mapM_
