{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TypeApplications    #-}
-- | Main application

module Middleware.App where

import           Colog.Core.IO                  (logStringStdout)
import           Colog.Polysemy                 (Log, log, runLogAction)
import           Colog.Polysemy.Formatting      (WithLog, addThreadAndTimeToLog, cmap, logInfo, logTextStderr,
                                                 logTextStdout, newLogEnv, renderThreadTimeMessage)
import           Control.Monad.Except
import           Data.Function                  ((&))
import           Formatting
import           GHC.Stack                      (HasCallStack)
import           Middleware.API
import           Middleware.Capability.CORS     (corsConfig)
import           Middleware.Capability.Config   (AppConfig (pabUrl), ConfigLoader, appConfigServer, load,
                                                 runConfigLoader)
import           Middleware.Capability.Error    hiding (Handler, throwError)
import           Middleware.Capability.ReqIdGen (runReqIdGen)
import           Middleware.PabClient           (runPabClient)
import           Network.Wai
import qualified Network.Wai.Handler.Warp       as Warp
import           Polysemy
import           Polysemy.Reader                (runReader)
import           Prelude                        hiding (log)
import           Servant
import           Servant.Polysemy.Client        (runServantClient, runServantClientUrl)
import           Servant.Polysemy.Server
import           System.IO                      (stdout)


runApp :: HasCallStack => IO (Either AppError ())
runApp = do
  logEnv <- newLogEnv stdout
  createApp
      & runConfigLoader
      & runError @AppError
      & addThreadAndTimeToLog
      & runLogAction @IO (logTextStderr & cmap (renderThreadTimeMessage logEnv))
      & runM

createApp :: (WithLog r, Members '[Embed IO, ConfigLoader, Error AppError] r) => Sem r ()
createApp = do
  appConfig <- load
  let pab = pabUrl appConfig
      serverCfg = appConfigServer appConfig
  logInfo (text % shown) "Running on port: " (Warp.getPort serverCfg)
  logInfo (text % shown) "Bind to: "         (Warp.getHost serverCfg)
  let api = Proxy @API
      app = serve api (hoistServer api (`runServer` pab) dexServer)
  runWarpServerSettings' @API serverCfg app
  where
    -- | TODO: Handle all errors, add "JSON" body for error messages and improve messages.
    handleErrors (Left (ConfigLoaderError id)) = Left err404 { errBody = "Cannot load configuration file" }
    handleErrors (Left err)    = Left err500 { errBody = "Internal Server Error" }
    handleErrors (Right value) = Right value

    liftHandler = Handler . ExceptT . fmap handleErrors

    runServer sem pabUrl = sem
      & runDex
      & runPabClient
      & runError @AppError
      & runReqIdGen
      & runServantClientUrl pabUrl
      & runM
      & liftHandler

-- | Run the given server with these Warp settings.
runWarpServerSettings'
  :: forall api r
   . ( HasServer api '[]
     , Member (Embed IO) r
     )
  => Warp.Settings
  -> Application
  -> Sem r ()
runWarpServerSettings' settings appServer = withLowerToIO $ \lowerToIO finished -> do
  Warp.runSettings settings (corsConfig appServer)
  finished
