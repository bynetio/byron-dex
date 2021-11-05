module Middleware.Capability.Time where


import Control.Concurrent     (threadDelay)
import Control.Monad.Freer    (Eff, LastMember, interpret, sendM, type (~>))
import Control.Monad.Freer.TH (makeEffect)
import Control.Monad.IO.Class (MonadIO, liftIO)

data Time r where
  Sleep :: Integer -> Time ()

makeEffect ''Time

runTime
  :: forall effs m. (MonadIO m, LastMember m effs)
  => Eff (Time ': effs)
  ~> Eff effs
runTime =
  interpret $ \(Sleep us) -> sendM (liftIO $ threadDelay (fromIntegral us))
