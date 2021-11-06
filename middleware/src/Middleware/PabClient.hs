{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module Middleware.PabClient where

import Colog.Polysemy.Formatting      (WithLog, logError)
import Data.Aeson                     (ToJSON)
import Data.Aeson.Types               (Value, toJSON)
import Dex.Types                      (Request (Request))
import Formatting
import Middleware.Capability.Error
import Middleware.Capability.ReqIdGen (ReqIdGen, nextReqId)
import Middleware.PabClient.API       (API)
import Middleware.PabClient.Types
import Polysemy                       (Embed, Members, Sem, interpret, makeSem)
import Servant                        (Proxy (..), type (:<|>) ((:<|>)))
import Servant.Client.Streaming       (ClientM, client)
import Servant.Polysemy.Client        (ClientError, ServantClient, runClient, runClient')

data ManagePabClient r a where
  Status :: ContractInstanceId -> ManagePabClient r ContractState
  GetFunds :: ContractInstanceId -> ManagePabClient r [Fund]

makeSem ''ManagePabClient

data PabClient = PabClient
  { healthcheck      :: ClientM ()
      -- ^ call healthcheck method
  , instanceClient   :: ContractInstanceId -> InstanceClient
      -- ^ call methods for instance client.
  }

-- | Contract instance endpoints
data InstanceClient = InstanceClient
  { getInstanceStatus    :: ClientM ContractState
      -- ^ get instance status
  , callInstanceEndpoint :: String -> Value -> ClientM ()
      -- ^ call instance endpoint
  , stopInstance         :: ClientM ()
      -- ^ call stop instance method
  }

-- | Init pab client
pabClient :: PabClient
pabClient = PabClient{..}
  where
    (healthcheck
      :<|> toInstanceClient
      ) = client (Proxy @API)

    instanceClient cid = InstanceClient{..}
        where
          (getInstanceStatus
            :<|> callInstanceEndpoint
            :<|> stopInstance
            ) = toInstanceClient cid


runPabClient :: (WithLog r, Members '[ServantClient, ReqIdGen , Error AppError, Embed IO] r)
             => Sem (ManagePabClient ': r) a
             -> Sem r a
runPabClient =
  interpret
    (\case
        Status cid -> do
          let PabClient{instanceClient} = pabClient
              getStatus = getInstanceStatus . instanceClient $ cid
          callRes <- runClient' getStatus
          mapAppError callRes


        GetFunds cid -> do
          let PabClient{instanceClient} = pabClient
              callEndpoint = callInstanceEndpoint . instanceClient $ cid
          req <- wrapRequest ()

          let body = toJSON req

          callRes <- runClient' $ callEndpoint "funds" body
          mapAppError callRes


          -- fetch respones from api...
          undefined

    )

    where
      mapAppError ::  (WithLog r, Members '[Error AppError, Embed IO] r) => Either ClientError a -> Sem r a
      mapAppError (Left err) = do
        logError (text % shown) "Cannot fetch status from PAB, cause: " err
        throw $ HttpError err
      mapAppError (Right v) = pure v


wrapRequest :: (ToJSON a, Members '[ReqIdGen] r) => a -> Sem r (Request a)
wrapRequest content = do
  id <- nextReqId
  let randomSeed = 3 -- make random later
      req = Request id randomSeed content
  pure req
