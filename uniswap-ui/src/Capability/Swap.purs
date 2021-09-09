module Uniswap.Capability.Swap where

import Prelude
import Data.Maybe (Maybe)
import Halogen (HalogenM, lift)
import Uniswap.Data.Swap (ISwap, ISwapPreview, Swap, SwapPreview, SwapPreviewResponse)

class
  Monad m <= ManageSwap m where
  swap :: Swap -> m Unit
  swapPreview :: SwapPreview -> m Unit
  indirectSwap :: ISwap -> m Unit
  indirectSwapPreview :: ISwapPreview -> m (Maybe SwapPreviewResponse)

instance manageSwapHalogenM :: ManageSwap m => ManageSwap (HalogenM st act slots msg m) where
  swap = lift <<< swap
  swapPreview = lift <<< swapPreview
  indirectSwap = lift <<< indirectSwap
  indirectSwapPreview = lift <<< indirectSwapPreview
