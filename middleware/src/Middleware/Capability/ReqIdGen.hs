module Middleware.Capability.ReqIdGen
  ( ReqIdGen
  , nextReqId
  , runReqIdGen
  ) where

import Control.Monad.Freer    as Eff (Eff, LastMember, interpret, type (~>))
import Control.Monad.Freer.TH as Eff (makeEffect)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text              (Text)
import Data.UUID              (toText)
import Data.UUID.V4           as UUID (nextRandom)


data ReqIdGen a where
  NextReqId :: ReqIdGen Text -- todo: change text to ???

Eff.makeEffect ''ReqIdGen

runReqIdGen
  :: forall effs m. (MonadIO m, LastMember m effs)
  => Eff (ReqIdGen ': effs)
  ~> Eff effs
runReqIdGen = Eff.interpret $ \case
  NextReqId -> liftIO $ toText <$> UUID.nextRandom
