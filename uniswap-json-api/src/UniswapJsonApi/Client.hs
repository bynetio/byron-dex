{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module UniswapJsonApi.Client where

import           Data.Aeson
import           Data.Proxy
import           Data.Text
import           Network.HTTP.Client     (newManager)
import qualified Network.HTTP.Client     as Network.HTTP.Client.Types
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.API
import           Servant.Client
import           System.Environment
import           UniswapJsonApi.Model

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

pabRequest :: Config -> ClientM a -> IO (Either ClientError a)
pabRequest c client = do
  let baseApiUrl = _apiUrl c
  let baseApiPort = _apiPort c
  m <- newManager tlsManagerSettings
  runClientM client (mkClientEnv m (BaseUrl Http baseApiUrl baseApiPort ""))

pabStatus :: Config -> Text -> IO (Either ClientError UniswapStatusResponse)
pabStatus c i = pabRequest c $ status i

pabEndpoint :: Config -> Text -> Text -> Value -> IO (Either ClientError ())
pabEndpoint c i n v = pabRequest c $ endpoint i n v

pabStop :: Config -> Text -> IO (Either ClientError ())
pabStop c i = pabRequest c $ stop i

-- uniswap endpoints

uniswapCreate :: Config -> Text -> Text -> Text -> Int -> Int -> IO (Either ClientError ())
uniswapCreate c i coinA coinB amountA amountB = pabEndpoint c i "create" v
  where
    v =
      object
        [ "cpCoinA" .= coinA,
          "cpCoinB" .= coinB,
          "cpAmountA" .= amountA,
          "cpAmountB" .= amountB
        ]

uniswapSwap :: Config -> Text -> Text -> Text -> Int -> Int -> Int -> IO (Either ClientError ())
uniswapSwap c i coinA coinB amount result slippage = pabEndpoint c i "swap" v
  where
    v =
      object
        [ "spCoinA" .= coinA,
          "spCoinB" .= coinB,
          "spAmount" .= amount,
          "spResult" .= result,
          "spSlippage" .= slippage
        ]

uniswapSwapPreview :: Config -> Text -> Text -> Text -> Int -> IO (Either ClientError ())
uniswapSwapPreview c i coinA coinB amount = pabEndpoint c i "swapPreview" v
  where
    v =
      object
        [ "sppCoinA" .= coinA,
          "sppCoinB" .= coinB,
          "sppAmount" .= amount
        ]

uniswapIndirectSwap :: Config -> Text -> Text -> Text -> Int -> Int -> Int -> IO (Either ClientError ())
uniswapIndirectSwap c i coinA coinB amount result slippage = pabEndpoint c i "iSwap" v
  where
    v =
      object
        [ "ispCoinA" .= coinA,
          "ispCoinB" .= coinB,
          "ispAmount" .= amount,
          "ispResult" .= result,
          "ispSlippage" .= slippage
        ]

uniswapIndirectSwapPreview :: Config -> Text -> Text -> Text -> Int -> IO (Either ClientError ())
uniswapIndirectSwapPreview c i coinA coinB amount = pabEndpoint c i "iSwapPreview" v
  where
    v =
      object
        [ "sppCoinA" .= coinA,
          "sppCoinB" .= coinB,
          "sppAmount" .= amount
        ]

uniswapClose :: Config -> Text -> Text -> Text -> IO (Either ClientError ())
uniswapClose c i coinA coinB = pabEndpoint c i "close" v
  where
    v =
      object
        [ "clpCoinA" .= coinA,
          "clpCoinB" .= coinB
        ]

uniswapRemove :: Config -> Text -> Text -> Text -> Int -> IO (Either ClientError ())
uniswapRemove c i coinA coinB amount = pabEndpoint c i "remove" v
  where
    v =
      object
        [ "rpCoinA" .= coinA,
          "rpCoinB" .= coinB,
          "rpDiff" .= amount
        ]

uniswapAdd :: Config -> Text -> Text -> Text -> Int -> Int -> IO (Either ClientError ())
uniswapAdd c i coinA coinB amountA amountB = pabEndpoint c i "add" v
  where
    v =
      object
        [ "apCoinA" .= coinA,
          "apCoinB" .= coinB,
          "apAmountA" .= amountA,
          "apAmountB" .= amountB
        ]

uniswapPools :: Config -> Text -> IO (Either ClientError ())
uniswapPools c i = pabEndpoint c i "funds" v
  where
    v = object []

uniswapFunds :: Config -> Text -> IO (Either ClientError ())
uniswapFunds c i = pabEndpoint c i "funds" v
  where
    v = object []

uniswapStop :: Config -> Text -> IO (Either ClientError ())
uniswapStop c i = pabEndpoint c i "stop" v
  where
    v = object []

