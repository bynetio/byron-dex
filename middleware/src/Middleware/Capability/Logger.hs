module Middleware.Capability.Logger where

import qualified Colog                  as L
import           Control.Monad.Freer    (Eff, LastMember, interpretM, type (~>))
import qualified Control.Monad.Freer.TH as EffTH
import           Control.Monad.IO.Class (MonadIO (liftIO))
import           Data.Text              (Text, pack)

data Logger r where
  LogInfo :: Text -> Logger ()
  LogDebug :: Text -> Logger ()
  LogError :: Text -> Logger ()

EffTH.makeEffect ''Logger

-- | Interpret the 'Logger' effect.
runColog
  :: forall effs. (LastMember IO effs)
  => Eff (Logger ': effs)
  ~> Eff effs
runColog =
  interpretM $
      \case
        LogInfo  msg -> liftIO $ withColog L.I msg
        LogError msg -> liftIO $ withColog L.E msg
        LogDebug msg -> liftIO $ withColog L.D msg

withColog :: MonadIO m => L.Severity -> Text -> m ()
withColog s = L.usingLoggerT (L.cmap L.fmtMessage L.logTextStdout) . L.log s

showText :: Show a => a -> Text
showText = pack . show
