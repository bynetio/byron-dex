{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeOperators              #-}

module Faucet.API (app, FaucetService(..)) where

import           Cardano.Api
import qualified Colog                as Log
import           Control.Exception    (Exception, try)
import           Control.Monad.Except (ExceptT (ExceptT), MonadIO (liftIO),
                                       (<=<))
import           Data.Aeson           (ToJSON (toJSON), encode)
import           Data.Text            (Text, pack)
import qualified Data.Text            as T
import           Faucet.Data          (AddressParam, FaucetException (..),
                                       Token, TokenName (..))
import           GHC.Generics         (Generic)
import           Logger               (logger)
import           Servant

type API = "faucet" :> Capture "address" AddressParam :> ReqBody '[JSON] TokenName :> Post '[JSON] TxId
            :<|> "tokens" :> Get '[JSON] [Token]

newtype ErrorMessage = ErrorMessage
  { errorMessage :: Text
  } deriving (Generic, ToJSON)

server :: FaucetService -> Server API
server service = handleFaucet' :<|> handleTokens
  where
      handleFaucet' :: AddressParam -> TokenName -> Handler TxId
      handleFaucet' address tn = liftHandler $ handleFaucet service address tn
      handleTokens :: Handler [Token]
      handleTokens = liftHandler $ return $ getTokens service

      adaptErrors ::  Either FaucetException a -> Either ServerError a
      adaptErrors (Left (TokenNameNotSupportedError (TokenName tn))) = Left $ notFound ("Token not supported: '" <> pack tn <> "'")
      adaptErrors (Left NoUtxoToConsumeError) = Left $ conflict (pack "Wait few seconds and try again")
      adaptErrors (Left _) = Left $ internalServerError "Internal Server Error"
      adaptErrors (Right value) = Right value

      conflict = httpError err409

      internalServerError = httpError err500

      notFound = httpError err404

      httpError :: ServerError -> Text -> ServerError
      httpError err msg =
        err
          { errBody = encode $ toJSON $ ErrorMessage msg
          , errHeaders = errorHeaders
          }
        where
          errorHeaders = [("Content-Type", "application/json")]

      liftHandler :: IO a -> Handler a
      liftHandler = Handler . ExceptT . fmap adaptErrors . (logError <=< try)
        where
          logError :: Exception e =>  Either e a -> IO (Either e a)
          logError err@(Left e) = do
            liftIO $ logger Log.E $ T.pack (show e)
            return err
          logError a            = return a

proxyAPI :: Proxy API
proxyAPI = Proxy

data FaucetService = FaucetService {
  getTokens    :: [Token],
  handleFaucet :: AddressParam -> TokenName -> IO TxId
}

app :: FaucetService -> Application
app = serve proxyAPI . server
