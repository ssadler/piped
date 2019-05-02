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
  map :: (a -> b) -> p a b m ()
  mapMC :: (i -> m o) -> p i o m ()
  mapMC_ :: (i -> m ()) -> p i () m ()
  take :: Int -> p i i m ()
  drop :: Int -> p i o m ()
  takeWhile :: (i -> Bool) -> p i i m ()
  dropWhile :: Monad m => (i -> Bool) -> p i o m ()
  filter :: (i -> Bool) -> p i i m ()
  leftover :: i -> p i o m ()

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
  take = C.take
  drop = C.drop
  takeWhile = C.takeWhile
  map = C.map
  mapMC = C.mapM
  mapMC_ = C.mapM_
  dropWhile = C.dropWhile
  filter = C.filter
  leftover = C.leftover

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
  take = P.take
  drop = P.drop
  takeWhile = P.takeWhile
  map = P.map
  mapMC = P.mapM
  mapMC_ = P.mapM_
  dropWhile = P.dropWhile
  filter = P.filter
  leftover = P.leftover
