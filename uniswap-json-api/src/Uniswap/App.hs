{-# LANGUAGE RecordWildCards #-}
module Uniswap.App
  ( runApp )
  where

import Control.Monad.Except         (ExceptT (..))
import Control.Monad.Freer          (Eff, runM)
import Control.Monad.Freer.Error    (runError)
import Control.Monad.IO.Class       (liftIO)
import Data.Aeson                   (encode)
import Data.Text                    (Text)
import Network.Wai.Handler.Warp     (run)
import Servant
import Servant.Client.Streaming     (ClientError)
import Uniswap.API                  (API, api)
import Uniswap.Common.AppError      (AppError, Err)
import Uniswap.Common.Logger        (Logger, runColog)
import Uniswap.Common.NextId        (NextId, runNextId)
import Uniswap.Common.ServantClient (ServantClient, runServantClientUrl)
import Uniswap.Common.Utils         (Time, runTime)
import Uniswap.PAB                  (UniswapPab, runPab)
import UniswapJsonApi.Types         (AppContext (..), PabConfig (..))

runApp :: AppContext -> IO ()
runApp ctx = do
  -- create a server
  let server =
        hoistServer
          (Proxy :: Proxy API)
          (liftToHandler ctx)
          api
      -- create application
      app = serve (Proxy :: Proxy API) server

  -- serve application
  run (port ctx) app

type AppEffs =
  '[ UniswapPab
   , ServantClient
   , NextId
   , Logger
   , AppError
   , Time
   , Handler
   ]


liftToHandler :: forall a. AppContext -> Eff AppEffs a -> Handler a
liftToHandler MkAppContext{..} effs = do
  handler <- runEffects pab effs
  either mapError pure handler

  where
    runEffects :: PabConfig -> Eff AppEffs a -> Handler (Either Err a)
    runEffects pab = runM . runTime . runError . runColog . runNextId . runServantClientUrl (toPabUrl pab) . runPab

    toPabUrl :: PabConfig -> String
    toPabUrl MkPabConfig{..} = pabUrl <> ":" <> show pabPort


mapError :: Err -> Handler a
mapError =
  throwError . \case
    err -> err400{errBody = encode . show $ err}-- FIXME: Map Errors to error codes/body

