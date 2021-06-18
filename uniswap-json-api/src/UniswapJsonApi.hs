{-# LANGUAGE DataKinds         #-}
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

type SwapApi = "create"        :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text :> QueryParam "amount_a" Int :> QueryParam "amount_a" Int :> Post '[JSON] ()
          :<|> "swap"          :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text :> QueryParam "amount_a" Int :> QueryParam "amount_a" Int :> Post '[JSON] ()
          :<|> "indirect_swap" :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text :> QueryParam "amount_a" Int :> QueryParam "amount_a" Int :> Post '[JSON] ()
          :<|> "close"         :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text                                                           :> Post '[JSON] ()
          :<|> "remove"        :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text :> QueryParam "amount" Int                                :> Post '[JSON] ()
          :<|> "add"           :> QueryParam "coin_a" Text :> QueryParam "coin_b" Text :> QueryParam "amount_a" Int :> QueryParam "amount_a" Int :> Post '[JSON] ()
          :<|> "pools"                                                                                                                           :> Get  '[JSON] ()
          :<|> "funds"                                                                                                                           :> Get  '[JSON] ()
          :<|> "stop"                                                                                                                            :> Get  '[JSON] ()
          :<|> "todos"         :> Capture "id" Int                                                                                               :> Get  '[JSON] ()
          :<|> "posts"         :> Capture "id" Int                                                                                               :> Get  '[JSON] ()

swapServer :: Config -> Server SwapApi
swapServer c = Logic.create c
          :<|> Logic.swap c
          :<|> Logic.indirectSwap c
          :<|> Logic.close c
          :<|> Logic.remove c
          :<|> Logic.add c
          :<|> Logic.pools c
          :<|> Logic.funds c
          :<|> Logic.stop c
          :<|> Logic.todos c
          :<|> Logic.posts c

swapApp :: Config ->  Application
swapApp c = serve (Proxy :: Proxy SwapApi) (swapServer c)

runApp :: Config -> IO ()
runApp c = run (_port c) (swapApp c)
