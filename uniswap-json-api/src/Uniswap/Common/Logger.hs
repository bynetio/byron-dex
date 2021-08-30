module Uniswap.Common.Logger
  ( Logger
  , logDebug
  , logError
  , logInfo
  , runColog
  ) where

import qualified Colog                  as L
import           Control.Monad.Freer    (Eff, LastMember, Member, interpretM,
                                         type (~>))
import qualified Control.Monad.Freer.TH as EffTH
import           Control.Monad.IO.Class (MonadIO)
import           Data.Text              (Text)

data Logger r where
  LogInfo   :: Text -> Logger ()
  LogDebug  :: Text -> Logger ()
  LogError  :: Text -> Logger ()

EffTH.makeEffect ''Logger

-- | Interpret the 'Logger' effect.
runColog
  :: forall effs m. (MonadIO m, LastMember m effs)
  => Eff (Logger ': effs)
  ~> Eff effs
runColog =
  interpretM $
      \case
        LogInfo  msg -> withColog L.I msg
        LogError msg -> withColog L.E msg
        LogDebug msg -> withColog L.D msg

-- | FIXME: Change fmtMessage to fmtSimpleRichMessageDefault to add correct
withColog :: MonadIO m => L.Severity -> Text -> m ()
withColog s = L.usingLoggerT (L.cmap L.fmtMessage L.logTextStdout) . L.log s
