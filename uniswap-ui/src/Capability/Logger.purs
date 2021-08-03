module Uniswap.Capability.Logger where

import Prelude
import Control.Monad.Trans.Class (lift)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen (HalogenM)
import Uniswap.Capability.Now (class Now)
import Uniswap.Data.Log (Log, LogReason(..), mkLog)

class
  Monad m <= Logger m where
  logger :: Log -> m Unit

instance loggerHalogenM :: Logger m => Logger (HalogenM st act slots msg m) where
  logger = lift <<< logger

log :: forall m. Logger m => Now m => LogReason -> String -> m Unit
log reason = logger <=< mkLog reason

logDebug :: forall m. Logger m => Now m => String -> m Unit
logDebug = log Debug

logInfo :: forall m. Logger m => Now m => String -> m Unit
logInfo = log Info

logWarn :: forall m. Logger m => Now m => String -> m Unit
logWarn = log Warn

logError :: forall m. Logger m => Now m => String -> m Unit
logError = log Error

logHush :: forall m a. Logger m => Now m => LogReason -> m (Either String a) -> m (Maybe a)
logHush reason action =
  action
    >>= case _ of
        Left e -> case reason of
          Debug -> logDebug e *> pure Nothing
          Info -> logInfo e *> pure Nothing
          Warn -> logWarn e *> pure Nothing
          Error -> logError e *> pure Nothing
        Right v -> pure $ Just v

debugHush :: forall m a. Logger m => Now m => m (Either String a) -> m (Maybe a)
debugHush = logHush Debug
