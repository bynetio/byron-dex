module Middleware.Capability.Time where


import Control.Concurrent     (threadDelay)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Polysemy

data Time m r where
  Sleep :: Integer -> Time m ()

makeEffect ''Time

runTime
  :: forall effs m. (MonadIO m, LastMember m effs)
  => Sem (Time ': effs)
  ~> Sem effs
runTime =
  interpret $ \case (Sleep us) -> liftIO $ threadDelay (fromIntegral us)
