module Uniswap.Capability.Funds where

import Prelude
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Uniswap.Data.Funds (Fund)

class
  Monad m <= ManageFunds m where
  getFunds :: m (Maybe (Array Fund))

instance managePoolHalogenM :: ManageFunds m => ManageFunds (HalogenM st act slots msg m) where
  getFunds = lift getFunds
