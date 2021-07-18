{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

{-# LANGUAGE RecordWildCards   #-}
module UniswapJsonApi.Client where

import           Control.Monad.IO.Class  (MonadIO, liftIO)
import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           Data.UUID               (UUID, toText)
import           Network.HTTP.Client     (newManager)
import qualified Network.HTTP.Client     as Network.HTTP.Client.Types
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.API
import           Servant.Client
import           System.Environment
import           UniswapJsonApi.Types

type UniswapAPI =
  "api" :> "new" :> "contract" :> "instance" :> Capture "instance-id" Text :> "status" :> Get '[JSON] UniswapStatusResponse
    :<|> "api" :> "new" :> "contract" :> "instance" :> Capture "instance-id" Text :> "endpoint" :> Capture "endpoint-name" Text :> ReqBody '[JSON] Value :> Post '[JSON] ()
    :<|> "api" :> "new" :> "contract" :> "instance" :> Capture "instance-id" Text :> "stop" :> Put '[JSON] ()

uniswapApi :: Proxy UniswapAPI
uniswapApi = Proxy

uniswap = client uniswapApi

status :: Text -> ClientM UniswapStatusResponse
endpoint :: Text -> Text -> Value -> ClientM ()
stop :: Text -> ClientM ()
(status :<|> endpoint :<|> stop) = uniswap

pabRequest :: MonadIO m => PabConfig -> ClientM a -> m (Either ClientError a)
pabRequest MkPabConfig{..} client = do
  m <- liftIO $ newManager tlsManagerSettings
  liftIO $ runClientM client (mkClientEnv m (BaseUrl Http pabUrl pabPort ""))

pabStatus :: MonadIO m => PabConfig -> Instance -> m (Either ClientError UniswapStatusResponse)
pabStatus cfg = pabRequest cfg . status

pabEndpoint :: MonadIO m => PabConfig -> Text -> Text -> Value -> m (Either ClientError ())
pabEndpoint c i n v = pabRequest c $ endpoint i n v

pabStop :: MonadIO m => PabConfig -> Text -> m (Either ClientError ())
pabStop c = pabRequest c . stop

-- uniswap endpoints

uniswapCreate :: MonadIO m => PabConfig -> Instance -> UUID -> Text -> Text -> Int -> Int -> m (Either ClientError ())
uniswapCreate c i opId coinA coinB amountA amountB = pabEndpoint c i "create" v
  where
    v =
      object
        [ "cpOpId"    .= toText opId
        , "cpCoinA"   .= coinA
        , "cpCoinB"   .= coinB
        , "cpAmountA" .= amountA
        , "cpAmountB" .= amountB
        ]

uniswapSwap :: MonadIO m => PabConfig -> Instance -> UUID -> Text -> Text -> Int -> Int -> Int -> m (Either ClientError ())
uniswapSwap c i opId coinA coinB amount result slippage = pabEndpoint c i "swap" v
  where
    v =
      object
        [ "spOpId"     .= toText opId
        , "spCoinA"    .= coinA
        , "spCoinB"    .= coinB
        , "spAmount"   .= amount
        , "spResult"   .= result
        , "spSlippage" .= slippage
        ]

uniswapSwapPreview :: MonadIO m => PabConfig -> Instance -> UUID -> Text -> Text -> Int -> m (Either ClientError ())
uniswapSwapPreview c i opId coinA coinB amount = pabEndpoint c i "swapPreview" v
  where
    v =
      object
        [ "spoOpId"   .= toText opId
        , "sppCoinA"  .= coinA
        , "sppCoinB"  .= coinB
        , "sppAmount" .= amount
        ]

uniswapIndirectSwap :: MonadIO m => PabConfig -> Instance -> UUID -> Text -> Text -> Int -> Int -> Int -> m (Either ClientError ())
uniswapIndirectSwap c i opId coinA coinB amount result slippage = pabEndpoint c i "iSwap" v
  where
    v =
      object
        [ "ispOpId"     .= toText opId
        , "ispCoinA"    .= coinA
        , "ispCoinB"    .= coinB
        , "ispAmount"   .= amount
        , "ispResult"   .= result
        , "ispSlippage" .= slippage
        ]

uniswapIndirectSwapPreview :: MonadIO m => PabConfig -> Instance -> UUID -> Text -> Text -> Int -> m (Either ClientError ())
uniswapIndirectSwapPreview c i opId coinA coinB amount = pabEndpoint c i "iSwapPreview" v
  where
    v =
      object
        [ "sppOpId"   .= toText opId
        , "sppCoinA"  .= coinA
        , "sppCoinB"  .= coinB
        , "sppAmount" .= amount
        ]

uniswapClose :: MonadIO m => PabConfig -> Instance -> UUID -> Text -> Text -> m (Either ClientError ())
uniswapClose c i opId coinA coinB = pabEndpoint c i "close" v
  where
    v =
      object
        [ "clpOpId"  .= toText opId
        , "clpCoinA" .= coinA
        , "clpCoinB" .= coinB
        ]

uniswapRemove :: MonadIO m => PabConfig -> Instance -> UUID -> Text -> Text -> Int -> m (Either ClientError ())
uniswapRemove c i opId coinA coinB amount = pabEndpoint c i "remove" v
  where
    v =
      object
        [ "rpOpId"  .= toText opId
        , "rpCoinA" .= coinA
        , "rpCoinB" .= coinB
        , "rpDiff"  .= amount
        ]

uniswapAdd :: MonadIO m => PabConfig -> Instance -> UUID -> Text -> Text -> Int -> Int -> m (Either ClientError ())
uniswapAdd c i opId coinA coinB amountA amountB = pabEndpoint c i "add" v
  where
    v =
      object
        [ "apOpId"    .= toText opId
        , "apCoinA"   .= coinA
        , "apCoinB"   .= coinB
        , "apAmountA" .= amountA
        , "apAmountB" .= amountB
        ]

uniswapPools :: MonadIO m => PabConfig -> Instance -> UUID -> m (Either ClientError ())
uniswapPools c i opId = pabEndpoint c i "pools" v
  where
    v = object [ "plOpId" .= toText opId ]

uniswapFunds :: MonadIO m => PabConfig -> Instance -> UUID -> m (Either ClientError ())
uniswapFunds c i opId = pabEndpoint c i "funds" v
  where
    v = object [ "fsOpId" .= toText opId ]

uniswapStop :: MonadIO m => PabConfig -> Instance -> UUID -> m (Either ClientError ())
uniswapStop c i opId = pabEndpoint c i "stop" v
  where
    v = object [ "stOpId" .= toText opId ]

