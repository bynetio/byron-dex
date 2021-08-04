module Uniswap.Capability.Pool where

import Prelude
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Uniswap.Data.LiquidityPool (LiquidityPool)

class
  Monad m <= ManagePool m where
  getLiquidityPools :: m (Maybe (Array LiquidityPool))
  createLiquidityPool :: LiquidityPool -> m Unit

instance managePoolHalogenM :: ManagePool m => ManagePool (HalogenM st act slots msg m) where
  getLiquidityPools = lift getLiquidityPools
  createLiquidityPool = lift <<< createLiquidityPool
