
module Control.Pipe
  (
    Pipe
  , Void

  -- Core functions
  
  , runPipe
  , (.|)
  , await
  , yield

  -- Extras
  
  , awaitForever
  , awaitMaybe
  , awaitJust
  , sourceList
  , sinkList
  , sinkNull
  , yieldM

  ) where


import Control.Pipe.Internal
import Control.Pipe.Extras

import Data.Void

