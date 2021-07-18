{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module UniswapJsonApi.Logic where

import Control.Monad           (join)
import Control.Monad.Except    (MonadError, throwError)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Retry
import Data.Aeson              (encode)
import Data.Either.Combinators
import Data.Text
import Data.UUID               (UUID)
import Data.UUID.V4            as UUID
import Servant
import Servant.Client
import UniswapJsonApi.Client
import UniswapJsonApi.Types


processRequest :: (MonadIO m, MonadError ServerError m, Show a) => PabConfig
               -> Instance
               -> OperationId
               -> String
               -> m (Either ClientError a)
               -> m UniswapDefinition
processRequest c uId opId errorMessage endpoint = do
  endpointResponse <- retrying limitedBackoff notSuccess (const endpoint)
  case endpointResponse of
    Right _ -> do
      let statusResult _ = pabStatus c uId
      statusResponse <- retrying limitedBackoff notSuccess statusResult
      case extractStatus statusResponse of
        Right r -> pure r
        Left _ -> throwError err422{errBody = encode . pack $ "cannot fetch status of operation id: " <> show opId}
    Left _ -> throwError err422{errBody = encode . pack $ errorMessage}
  where
    limitedBackoff :: RetryPolicy
    limitedBackoff = exponentialBackoff 50 <> limitRetries 5

    notSuccess = const $ pure . isLeft

    history = observableState . cicCurrentState

    extractUniswapDef :: UniswapStatusResponse -> Either Text UniswapDefinition
    extractUniswapDef = join . maybeToRight "Operation ID not found in history" . lookupHistory opId . history

    extractStatus :: Either ClientError UniswapStatusResponse -> Either Text UniswapDefinition-- -> UniswapDefinition
    extractStatus e = extractUniswapDef =<< mapLeft (pack . show) e


create :: (MonadIO m) => Instance -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> AppM m UniswapDefinition
create uId (Just cA) (Just cB) (Just aA) (Just aB) = do
  pabCfg <- asks pab
  opId <- liftIO UUID.nextRandom
  let req = uniswapCreate pabCfg uId opId cA cB aA aB
  processRequest pabCfg uId opId "cannot create a pool" req
create uId _ _ _ _ = throwError err400

swap :: MonadIO m => Instance -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Int -> AppM m UniswapDefinition
swap uId (Just cA) (Just cB) (Just aA) (Just aB) (Just s) = do
  pabCfg <- asks pab
  opId <- liftIO UUID.nextRandom
  let req = uniswapSwap pabCfg uId opId cA cB aA aB s
  processRequest pabCfg uId opId "cannot make a swap" req
swap uId _ _ _ _ _ = throwError err400

swapPreview :: MonadIO m => Instance -> Maybe Text -> Maybe Text -> Maybe Int -> AppM m UniswapDefinition
swapPreview uId (Just cA) (Just cB) (Just a) = do
  pabCfg <- asks pab
  opId <- liftIO UUID.nextRandom
  let req = uniswapSwapPreview pabCfg uId opId cA cB a
  processRequest pabCfg uId opId "cannot make a swap (preview)" req
swapPreview uId _ _ _ = throwError err400

indirectSwap :: MonadIO m => Instance -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Int -> AppM m UniswapDefinition
indirectSwap uId (Just cA) (Just cB) (Just aA) (Just aB) (Just s) = do
  pabCfg <- asks pab
  opId <- liftIO UUID.nextRandom
  let req = uniswapIndirectSwap pabCfg uId opId cA cB aA aB s
  processRequest pabCfg uId opId "cannot make an indirect swap" req
indirectSwap uId _ _ _ _ _ = throwError err400

indirectSwapPreview :: MonadIO m => Instance -> Maybe Text -> Maybe Text -> Maybe Int -> AppM m UniswapDefinition
indirectSwapPreview uId (Just cA) (Just cB) (Just a) = do
  pabCfg <- asks pab
  opId <- liftIO UUID.nextRandom
  let req = uniswapIndirectSwapPreview pabCfg uId opId cA cB a
  processRequest pabCfg uId opId "cannot make an indirect swap preview" req
indirectSwapPreview uId _ _ _ = throwError err400

close :: MonadIO m => Instance -> Maybe Text -> Maybe Text -> AppM m UniswapDefinition
close uId (Just cA) (Just cB) = do
  pabCfg <- asks pab
  opId <- liftIO UUID.nextRandom
  let req = uniswapClose pabCfg uId opId cA cB
  processRequest pabCfg uId opId "cannot close a pool" req
close uId _ _ = throwError err400

remove :: MonadIO m => Instance -> Maybe Text -> Maybe Text -> Maybe Int -> AppM m UniswapDefinition
remove uId (Just cA) (Just cB) (Just a) = do
  pabCfg <- asks pab
  opId <- liftIO UUID.nextRandom
  let req = uniswapRemove pabCfg uId opId cA cB a
  processRequest pabCfg uId opId "cannot remove liquidity tokens" req
remove uId _ _ _ = throwError err400

add :: MonadIO m => Instance -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> AppM m UniswapDefinition
add uId (Just cA) (Just cB) (Just aA) (Just aB) = do
  pabCfg <- asks pab
  opId <- liftIO UUID.nextRandom
  let req = uniswapAdd pabCfg uId opId cA cB aA aB
  processRequest pabCfg uId opId "cannot add coins to pool" req
add uId _ _ _ _ = throwError err400

pools :: MonadIO m => Instance -> AppM m UniswapDefinition
pools uId =do
  pabCfg <- asks pab
  opId <- liftIO UUID.nextRandom
  let req = uniswapPools pabCfg uId opId
  processRequest pabCfg uId opId "cannot fetch uniwap pools" req

funds :: MonadIO m => Instance -> AppM m UniswapDefinition
funds uId = do
  pabCfg <- asks pab
  opId <- liftIO UUID.nextRandom
  let req = uniswapFunds pabCfg uId opId
  processRequest pabCfg uId opId "cannot fetch uniswap funds" req

stop :: MonadIO m => Instance -> AppM m UniswapDefinition
stop uId = do
  pabCfg <- asks pab
  opId <- liftIO UUID.nextRandom
  let req = uniswapStop pabCfg uId opId
  processRequest pabCfg uId opId "cannot stop an uniswap instance" req

status :: (MonadIO m) => Instance -> AppM m UniswapStatusResponse
status uId = do
  pabCfg <- asks pab
  result <- pabStatus pabCfg uId
  case result of
    Left _ -> throwError err400{errBody = encode . pack $ "Provided Uniswap Instance not found"}
    Right r -> pure r

