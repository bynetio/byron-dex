{-# LANGUAGE OverloadedStrings #-}

module Faucet where

import           Cardano.Api                 (TxId)
import           Colog
import           Config
import           Control.Concurrent          (newMVar)
import           Control.Exception           (throw)
import           Control.Monad               (unless)
import           Data.Aeson.Encode.Pretty    (encodePretty)
import qualified Data.ByteString.Lazy        as BL
import           Data.Set                    (Set)
import qualified Data.Set                    as Set
import qualified Data.Text                   as T
import           Data.Text.Encoding          (decodeUtf8)
import           Faucet.API
import           Faucet.Data
import           Faucet.Internal
import           Logger                      (logger)
import           Network.Wai.Handler.Warp    (getHost, getPort, runSettings)
import           Network.Wai.Middleware.Cors (simpleCors)
import           Servant                     ()

handleFaucet' :: AppFaucetEnv AppFaucet -> Set TokenName -> AddressParam -> TokenName -> IO TxId
handleFaucet' ctx availableTokens address tn = do
  unless (Set.member tn availableTokens) $ throw (TokenNameNotSupportedError tn)
  logger I $ T.pack ("Sending " <> show tn <> " to " <> show address)
  runApp (faucet address tn) ctx

faucetApp :: IO ()
faucetApp = do
    config <- loadAppConfig
    logger I $ T.pack ("Starting server on " <> hostPort config <> " ...")
    logger I $ (decodeUtf8 . BL.toStrict) ("Config: " <> encodePretty config)
    faucetContexct <- mkFaucetContext config
    let availableTokens = TokenName . T.unpack <$> (mintConfigTokens . faucetConfigMint . appConfigFaucet) config
    policyId <- runApp mintPolicyId faucetContexct
    runSettings (appConfigServer config) $
      simpleCors $ app FaucetService {
        getTokens =  tokenFrom policyId <$> availableTokens,
        handleFaucet = handleFaucet' faucetContexct (Set.fromList availableTokens)
      }
  where
      tokenFrom policyId = Token $ (TokenCurrency . read . show) policyId
      port = getPort . appConfigServer
      host  = getHost . appConfigServer
      hostPort cfg = foldl (<>) "" [show $ host cfg, ":", show $ port cfg]
      mkFaucetContext :: AppConfig -> IO (AppFaucetEnv AppFaucet)
      mkFaucetContext (AppConfig _ faucetCfg nodeCfg) = do
        refs <- newMVar Set.empty
        return $ AppFaucetEnv nodeCfg faucetCfg refs richMessageAction
