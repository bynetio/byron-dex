module Uniswap.Capability.Pool where

import Prelude
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Uniswap.Data.LiquidityPool (CloseLiquidityPool, LiquidityPool, RemoveLiquidityPool, LiquidityPoolView)

class
  Monad m <= ManagePool m where
  getLiquidityPools :: m (Maybe (Array LiquidityPoolView))
  createLiquidityPool :: LiquidityPool -> m Unit
  closeLiquidityPool :: CloseLiquidityPool -> m Unit
  addToLiquidityPool :: LiquidityPool -> m Unit
  removeFromLiquidityPool :: RemoveLiquidityPool -> m Unit

instance managePoolHalogenM :: ManagePool m => ManagePool (HalogenM st act slots msg m) where
  getLiquidityPools = lift getLiquidityPools
  createLiquidityPool = lift <<< createLiquidityPool
  closeLiquidityPool = lift <<< closeLiquidityPool
  addToLiquidityPool = lift <<< addToLiquidityPool
  removeFromLiquidityPool = lift <<< removeFromLiquidityPool
