
module Control.Pipe.Conduit where

import Data.Function

import Control.Pipe.Internal
import Control.Pipe.Extras

import Data.Conduit.Internal hiding (Pipe)


fromConduit :: Monad m => ConduitT i o m a -> Pipe i o m a
fromConduit (ConduitT c) = Pipe $
  \rest ->
    fix1 (c Done) $
      \next p l r ->
        case p of
          (Done o) -> rest l r o
          (HaveOutput p o) ->
            runYield r o $ next p l
          (NeedInput f n) ->
            runAwait l (next (n ()) l r) $
              \i await -> next (f i) await r
          (PipeM m) ->
            m >>= \p -> next p l r
          (Leftover p i) ->
            next p (injectLeftover i l) r
