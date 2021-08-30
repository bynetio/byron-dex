{-# LANGUAGE TypeOperators #-}

module Uniswap.Common.ServantClient
  ( ServantClient
  , runClient
  , runClient'
  , runServantClientUrl
  ) where

import           Control.DeepSeq           (NFData)
import           Control.Monad             ((>=>))
import           Control.Monad.Freer       (Eff, LastMember, Member, Members,
                                            interpretM, send, sendM, type (~>))
import           Control.Monad.Freer.Error (Error, throwError)
import           Control.Monad.Freer.TH    (makeEffect)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Network.HTTP.Client       (newManager)
import           Network.HTTP.Client.TLS   (tlsManagerSettings)
import           Servant.Client.Streaming  (ClientError, ClientM, mkClientEnv,
                                            parseBaseUrl, runClientM,
                                            withClientM)
import           Uniswap.Common.Utils      (fromEither)

data ServantClient r where
  RunClient' :: NFData o => ClientM o -> ServantClient (Either ClientError o)

makeEffect ''ServantClient

-- | Run this 'ClientM' in the 'Eff' monad.
runClient
  :: forall m effs o. (MonadIO m, LastMember m effs, Members '[ServantClient, Error ClientError, m] effs, NFData o)
  => ClientM o
  -> Eff effs o
runClient = runClient' >=> fromEither

-- | Interpret the 'ServantClient' effect.
runServantClientUrl
  :: forall m effs. (MonadIO m, LastMember m effs)
  => String
  -> Eff (ServantClient ': effs)
  ~> Eff effs
runServantClientUrl server effs  = do
  baseUrl' <- liftIO $ parseBaseUrl server
  manager  <- liftIO $ newManager tlsManagerSettings
  let env = mkClientEnv manager baseUrl'
  interpretM (\case
    RunClient' client ->
      liftIO $ runClientM client env
    ) effs
