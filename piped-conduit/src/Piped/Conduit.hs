module Piped.Conduit where

import Data.Conduit.Internal hiding (Pipe)

import Piped.Internal


-- | Convert a ConduitT to a Pipe.
--
fromConduit :: Monad m => ConduitT i o m a -> Pipe i o m a
fromConduit (ConduitT c) = Pipe $
  \rest ->
    fix1 (c Done) $ \next p l r ->
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
          next p (addLeftover i l) r
