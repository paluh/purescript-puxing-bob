module Pux.Routing.Bob where

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Prelude

foreign import push :: forall eff. String -> Eff (dom :: DOM | eff) Unit
