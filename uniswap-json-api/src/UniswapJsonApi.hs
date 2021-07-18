{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module UniswapJsonApi where

import           Control.Exception        (try)
import           Control.Monad.Except
import           Control.Monad.Reader     (runReaderT)
import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Network.Wai.Handler.Warp
import qualified Network.Wai.Handler.Warp as W
import           Servant
import           Servant.Server
import           UniswapJsonApi.Logic     as Logic
import           UniswapJsonApi.Model     (Config (..))
import           UniswapJsonApi.Model     as Model
import           UniswapJsonApi.Types     (AppContext (..), AppM (..))

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

server :: ServerT SwapApi (AppM IO)
server =
  Logic.create
    :<|> Logic.swap
    :<|> Logic.swapPreview
    :<|> Logic.indirectSwap
    :<|> Logic.indirectSwapPreview
    :<|> Logic.close
    :<|> Logic.remove
    :<|> Logic.add
    :<|> Logic.pools
    :<|> Logic.funds
    :<|> Logic.stop
    :<|> Logic.status

api :: Proxy SwapApi
api = Proxy

runApp :: AppContext -> IO ()
runApp ctx = run (port ctx) (mkApp ctx)

mkApp :: AppContext -> Application
mkApp appContext =
  serve api $ hoistServer api (toHandler appContext) server
  where
    toHandler :: AppContext -> AppM IO a -> Handler a
    toHandler ctx a = Handler $ runReaderT (unAppM a) ctx



