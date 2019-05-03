
module TestUtils
  ( module TestUtils
  , module T
  ) where

import Data.IORef as T
import Test.Tasty as T
import Test.Tasty.HUnit as T

import Control.Monad.State as T
import Control.Monad.Identity as T

import Piped as T
import Piped.Internal as T


runPipeI = runIdentity . runPipe
runPipeS s p = runState (runPipe p) s
alloc n = modify (++[n])

