-- | Main application

module Middleware.App where

import           Control.Monad.Freer          (Eff, LastMember, Members, runM)
import           Control.Monad.Freer.Reader   (runReader)
import           Middleware.Capability.Config (ConfigLoader, appConfigServer, load, runConfigLoader)
import           Middleware.Capability.Error
import           Middleware.Capability.Logger (logInfo, runColog, showText)
import qualified Network.Wai.Handler.Warp     as Warp
import           Prelude                      hiding (log)


runApp :: IO (Either AppError ())
runApp = runM
        . runError @AppError
        . runConfigLoader
        $ app


app :: (Members [ ConfigLoader, Error AppError] r, LastMember IO r) => Eff r ()
app = do
  appConfig <- load
  runReader appConfig . runColog $ do
    let serverCfg = appConfigServer appConfig
    logInfo $ showText $ "Running on port: " ++ show (Warp.getPort serverCfg)
    logInfo $ showText $ "Bind to: " ++ show (Warp.getHost serverCfg)
