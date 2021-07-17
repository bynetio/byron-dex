{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module UniswapJsonApi where

import Control.Monad.Reader
import Data.Aeson
import Data.Text
import GHC.Generics
import Network.Wai.Handler.Warp
import Servant
import UniswapJsonApi.Logic     as Logic
import UniswapJsonApi.Model     as Model

type SwapApi =
  Capture "id" Text :> "create" :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text :> QueryParam "amount_a" Int :> QueryParam "amount_a" Int :> Post '[JSON] UniswapStatusResponse
    :<|> Capture "id" Text :> "swap" :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text :> QueryParam "amount_a" Int :> QueryParam "amount_a" Int :> QueryParam "slippage" Int :> Post '[JSON] UniswapStatusResponse
    :<|> Capture "id" Text :> "swap_preview" :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text :> QueryParam "amount" Int :> Post '[JSON] UniswapStatusResponse
    :<|> Capture "id" Text :> "indirect_swap" :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text :> QueryParam "amount_a" Int :> QueryParam "amount_a" Int :> QueryParam "slippage" Int :> Post '[JSON] UniswapStatusResponse
    :<|> Capture "id" Text :> "indirect_swap_preview" :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text :> QueryParam "amount" Int :> Post '[JSON] UniswapStatusResponse
    :<|> Capture "id" Text :> "close" :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text :> Post '[JSON] UniswapStatusResponse
    :<|> Capture "id" Text :> "remove" :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text :> QueryParam "amount" Int :> Post '[JSON] UniswapStatusResponse
    :<|> Capture "id" Text :> "add" :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text :> QueryParam "amount_a" Int :> QueryParam "amount_a" Int :> Post '[JSON] UniswapStatusResponse
    :<|> Capture "id" Text :> "pools" :> Get '[JSON] UniswapStatusResponse
    :<|> Capture "id" Text :> "funds" :> Get '[JSON] UniswapStatusResponse
    :<|> Capture "id" Text :> "stop" :> Get '[JSON] UniswapStatusResponse
    :<|> Capture "id" Text :> "status" :> Get '[JSON] UniswapStatusResponse

swapServer :: Config -> Server SwapApi
swapServer c =
  Logic.create c
    :<|> Logic.swap c
    :<|> Logic.swapPreview c
    :<|> Logic.indirectSwap c
    :<|> Logic.indirectSwapPreview c
    :<|> Logic.close c
    :<|> Logic.remove c
    :<|> Logic.add c
    :<|> Logic.pools c
    :<|> Logic.funds c
    :<|> Logic.stop c
    :<|> Logic.status c

swapApp :: Config -> Application
swapApp c = serve (Proxy :: Proxy SwapApi) (swapServer c)

runApp :: Config -> IO ()
runApp c = run (_port c) (swapApp c)

