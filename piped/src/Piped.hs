
module Piped (

  -- Core
  
    Pipe
  , await
  , yield
  , (.|)
  , (|.)
  , runPipe
  , leftover

  -- Barebones API
  
  , awaitForever
  , awaitMaybe
  , awaitJust
  , yieldM
  , sourceList
  , sinkList
  , sinkNull

  , Void

  ) where

import Piped.Extras
import Piped.Compose
import Piped.Internal

