{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module UniswapJsonApi where

import           Control.Monad.Reader
import           Data.Aeson
import           Data.Text
import           GHC.Generics
import           Network.Wai.Handler.Warp
import           Servant

import           UniswapJsonApi.Logic     as Logic
import           UniswapJsonApi.Model     as Model

newtype Config = Config { _port :: Int }

type SwapApi = "create"        :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text :> QueryParam "amount_a" Int :> QueryParam "amount_a" Int :> Post '[JSON] ()
          :<|> "swap"          :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text :> QueryParam "amount_a" Int :> QueryParam "amount_a" Int :> Post '[JSON] ()
          :<|> "indirect_swap" :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text :> QueryParam "amount_a" Int :> QueryParam "amount_a" Int :> Post '[JSON] ()
          :<|> "close"         :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text :> Post '[JSON] ()
          :<|> "remove"        :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text :> QueryParam "amount" Int :> Post '[JSON] ()
          :<|> "add"           :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text :> QueryParam "amount_a" Int :> QueryParam "amount_a" Int :> Post '[JSON] ()
          :<|> "pools"         :> Get '[JSON] ()
          :<|> "funds"         :> Get '[JSON] ()
          :<|> "stop"          :> Get '[JSON] ()
          :<|> "todos"         :> Capture "id" Int :> Get '[JSON] ()
          :<|> "posts"         :> Capture "id" Int :> Get '[JSON] ()


swapApp :: Application
swapApp = serve (Proxy :: Proxy SwapApi) swapServer

swapServer :: Server SwapApi
swapServer = Logic.create
        :<|> Logic.swap
        :<|> Logic.indirectSwap
        :<|> Logic.close
        :<|> Logic.remove
        :<|> Logic.add
        :<|> Logic.pools
        :<|> Logic.funds
        :<|> Logic.stop
        :<|> Logic.todos
        :<|> Logic.posts

runApp :: Reader Config (IO ())
runApp = do
  port <- asks _port
  return $ run port swapApp
