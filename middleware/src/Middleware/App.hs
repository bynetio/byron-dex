{-# LANGUAGE OverloadedStrings #-}
-- | Main application

module Middleware.App where

import           Colog.Core.IO                (logStringStdout)
import           Colog.Polysemy               (Log, log, runLogAction)
import           Colog.Polysemy.Formatting    (WithLog, addThreadAndTimeToLog,
                                               cmap, logInfo, logTextStderr,
                                               logTextStdout, newLogEnv,
                                               renderThreadTimeMessage)
import           Control.Monad.Except
import           Data.Function                ((&))
import           Formatting
import           GHC.Stack                    (HasCallStack)
import           Middleware.Capability.Config (ConfigLoader, appConfigServer,
                                               load, runConfigLoader)
import           Middleware.Capability.Error  hiding (Handler, throwError)
import           Middleware.DummyAPI
import           Network.Wai
import qualified Network.Wai.Handler.Warp     as Warp
import           Network.Wai.Middleware.Cors
import           Polysemy
import           Polysemy.Reader              (runReader)
import           Prelude                      hiding (log)
import           Servant
import           Servant.Polysemy.Server
import           System.IO                    (stdout)

---------------------------------------------------------------------------
-- | for test only
type API = DummyAPI

api :: Proxy DummyAPI
api = Proxy


dummyServer :: Members '[ Dummy, Error AppError] r => ServerT API (Sem r)
dummyServer  = fetchUsers
---------------------------------------------------------------------------

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
  -- void $ runReader appConfig -- pab config in reader
  let serverCfg = appConfigServer appConfig
  logInfo (text % shown) "Running on port: " (Warp.getPort serverCfg)
  logInfo (text % shown) "Bind to: "         (Warp.getHost serverCfg)
  let server = hoistServerIntoSem @API (runServer dummyServer)
  runWarpServerSettings @API serverCfg server
  where
    handleErrors (Left (ConfigLoaderError id)) = Left err404 { errBody = "Cannot load configuration file" }
    handleErrors (Left err)    = Left err500 { errBody = "Internal Server Error" }
    handleErrors (Right value) = Right value

    liftHandler = Handler . ExceptT . fmap handleErrors

    runServer sem = sem
      & runError @AppError
      & runDummy
      & runM
      & liftHandler

-- | CORS config
-- | FIXME: Add corsPolicy to wai...
corsPolicy :: CorsResourcePolicy
corsPolicy =
  CorsResourcePolicy {
    corsOrigins = Nothing,
    corsMethods = methods,
    corsRequestHeaders = ["Content-Type"],
    corsExposedHeaders = Nothing,
    corsMaxAge = Nothing,
    corsVaryOrigin = True,
    corsRequireOrigin = False,
    corsIgnoreFailures = False
  }
  where
    methods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
    cont = simpleContentTypes <> ["application/json"]

corsConfig :: Middleware
corsConfig = cors (const $ Just corsPolicy)
