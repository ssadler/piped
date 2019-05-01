{-# LANGUAGE AllowAmbiguousTypes #-}

module BenchCompare where

import Prelude hiding (foldl, scanl)
import Control.Monad.Trans

import PipeLike


source n = sourceList [0..n]

type TestPipeSet m =
  [( String
   , Integer -> Pipe () Void m ()
   , Integer -> ConduitT () Void m ()
   )]


compareIdents :: MonadIO m => TestPipeSet m
compareIdents =
  [ (show n, pIdents n, pIdents n) | n <- [0,2,4,6] ]

comparePipes :: MonadIO m => TestPipeSet m
comparePipes =
  [ ("foldl",     pFoldl,     pFoldl)
  , ("scanl",     pScanl,     pScanl)
  ]

-- For some reason, if a decleration has an argument it can be
-- polymorphic without a type signature

pFoldl n = source (n::Integer) .| (foldl (+) 0 >> pure ())

pScanl n = source n .| scanl (+) 0 .| sinkNull

pIdents s n = source n .| compose s .| sinkNull
  where
    compose 0 = map_ id
    compose s = map_ id .| compose (s-1)
