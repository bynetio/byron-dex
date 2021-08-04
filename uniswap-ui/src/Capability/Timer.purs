module Uniswap.Capability.Timer where

import Prelude
import Halogen (HalogenM)
import Halogen.Subscription as HS
import Control.Monad.Trans.Class (lift)

class
  Monad m <= Timer m where
  timer :: forall a. a -> m (HS.Emitter a)

instance timerHalogenH :: Timer m => Timer (HalogenM st act slots msg m) where
  timer = lift <<< timer
