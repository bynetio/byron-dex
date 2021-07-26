module Uniswap.Common.NextOpID
  ( NextOpID
  , next
  , runNextOpID
  ) where

import Control.Monad.Freer    as Eff (Eff, LastMember, interpret, type (~>))
import Control.Monad.Freer.TH as Eff (makeEffect)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.UUID              (toText)
import Data.UUID.V4           as UUID (nextRandom)
import UniswapJsonApi.Types   (OperationId)

data NextOpID a where
  Next :: NextOpID OperationId

Eff.makeEffect ''NextOpID

runNextOpID
  :: forall effs m. (MonadIO m, LastMember m effs)
  => Eff (NextOpID ': effs)
  ~> Eff effs
runNextOpID = Eff.interpret $ \case
  Next -> liftIO $ toText <$> UUID.nextRandom
