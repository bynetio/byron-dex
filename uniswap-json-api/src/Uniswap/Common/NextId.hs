module Uniswap.Common.NextId
  ( NextId
  , next
  , runNextId
  ) where

import Control.Monad.Freer    as Eff (Eff, LastMember, interpret, type (~>))
import Control.Monad.Freer.TH as Eff (makeEffect)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.UUID              (toText)
import Data.UUID.V4           as UUID (nextRandom)
import UniswapJsonApi.Types   (HistoryId)

data NextId a where
  Next :: NextId HistoryId

Eff.makeEffect ''NextId

runNextId
  :: forall effs m. (MonadIO m, LastMember m effs)
  => Eff (NextId ': effs)
  ~> Eff effs
runNextId = Eff.interpret $ \case
  Next -> liftIO $ toText <$> UUID.nextRandom
