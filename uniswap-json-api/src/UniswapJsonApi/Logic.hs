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
import Data.UUID               (toText)
import Data.UUID.V4            as UUID
import Servant
import Servant.Client
import UniswapJsonApi.Client
import UniswapJsonApi.Types


processRequest :: (MonadIO m, MonadError ServerError m, Show a) => PabConfig
               -> Instance
               -> HistoryId
               -> String
               -> m (Either ClientError a)
               -> m UniswapDefinition
processRequest c uId hid errorMessage endpoint = do
  endpointResponse <- retrying limitedBackoff notSuccess (const endpoint)
  case endpointResponse of
    Right _ -> do
      let statusResult _ = pabStatus c uId
      statusResponse <- retrying limitedBackoff statusNotReady statusResult
      case extractStatus statusResponse of
        Right r -> pure r
        Left err -> throwError err422{errBody = encode . pack $ show err}
    Left _ -> throwError err422{errBody = encode . pack $ errorMessage}
  where
    limitedBackoff :: RetryPolicy
    limitedBackoff = exponentialBackoff 50 <> limitRetries 5

    notSuccess     = const $ pure . isLeft
    statusNotReady = const $ pure . isLeft . extractStatus

    history = observableState . cicCurrentState

    extractUniswapDef :: UniswapStatusResponse -> Either Text UniswapDefinition
    extractUniswapDef = join . maybeToRight "Operation ID not found in history" . lookupHistory hid . history

    extractStatus :: Either ClientError UniswapStatusResponse -> Either Text UniswapDefinition
    extractStatus e = extractUniswapDef =<< mapLeft (pack . show) e


create :: (MonadIO m) => Instance -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> AppM m UniswapDefinition
create uId (Just cA) (Just cB) (Just aA) (Just aB) = do
  pabCfg <- asks pab
  hid <- toText <$> liftIO UUID.nextRandom
  let req = uniswapCreate pabCfg uId hid cA cB aA aB
  processRequest pabCfg uId hid "cannot create a pool" req
create uId _ _ _ _ = throwError err400

swap :: MonadIO m => Instance -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Int -> AppM m UniswapDefinition
swap uId (Just cA) (Just cB) (Just aA) (Just aB) (Just s) = do
  pabCfg <- asks pab
  hid <- toText <$> liftIO UUID.nextRandom
  let req = uniswapSwap pabCfg uId hid cA cB aA aB s
  processRequest pabCfg uId hid "cannot make a swap" req
swap uId _ _ _ _ _ = throwError err400

swapPreview :: MonadIO m => Instance -> Maybe Text -> Maybe Text -> Maybe Int -> AppM m UniswapDefinition
swapPreview uId (Just cA) (Just cB) (Just a) = do
  pabCfg <- asks pab
  hid <- toText <$> liftIO UUID.nextRandom
  let req = uniswapSwapPreview pabCfg uId hid cA cB a
  processRequest pabCfg uId hid "cannot make a swap (preview)" req
swapPreview uId _ _ _ = throwError err400

indirectSwap :: MonadIO m => Instance -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Int -> AppM m UniswapDefinition
indirectSwap uId (Just cA) (Just cB) (Just aA) (Just aB) (Just s) = do
  pabCfg <- asks pab
  hid <- toText <$> liftIO UUID.nextRandom
  let req = uniswapIndirectSwap pabCfg uId hid cA cB aA aB s
  processRequest pabCfg uId hid "cannot make an indirect swap" req
indirectSwap uId _ _ _ _ _ = throwError err400

indirectSwapPreview :: MonadIO m => Instance -> Maybe Text -> Maybe Text -> Maybe Int -> AppM m UniswapDefinition
indirectSwapPreview uId (Just cA) (Just cB) (Just a) = do
  pabCfg <- asks pab
  hid <- toText <$> liftIO UUID.nextRandom
  let req = uniswapIndirectSwapPreview pabCfg uId hid cA cB a
  processRequest pabCfg uId hid "cannot make an indirect swap preview" req
indirectSwapPreview uId _ _ _ = throwError err400

close :: MonadIO m => Instance -> Maybe Text -> Maybe Text -> AppM m UniswapDefinition
close uId (Just cA) (Just cB) = do
  pabCfg <- asks pab
  hid <- toText <$> liftIO UUID.nextRandom
  let req = uniswapClose pabCfg uId hid cA cB
  processRequest pabCfg uId hid "cannot close a pool" req
close uId _ _ = throwError err400

remove :: MonadIO m => Instance -> Maybe Text -> Maybe Text -> Maybe Int -> AppM m UniswapDefinition
remove uId (Just cA) (Just cB) (Just a) = do
  pabCfg <- asks pab
  hid <- toText <$> liftIO UUID.nextRandom
  let req = uniswapRemove pabCfg uId hid cA cB a
  processRequest pabCfg uId hid "cannot remove liquidity tokens" req
remove uId _ _ _ = throwError err400

add :: MonadIO m => Instance -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> AppM m UniswapDefinition
add uId (Just cA) (Just cB) (Just aA) (Just aB) = do
  pabCfg <- asks pab
  hid <- toText <$> liftIO UUID.nextRandom
  let req = uniswapAdd pabCfg uId hid cA cB aA aB
  processRequest pabCfg uId hid "cannot add coins to pool" req
add uId _ _ _ _ = throwError err400

pools :: MonadIO m => Instance -> AppM m UniswapDefinition
pools uId =do
  pabCfg <- asks pab
  hid <- toText <$> liftIO UUID.nextRandom
  let req = uniswapPools pabCfg uId hid
  processRequest pabCfg uId hid "cannot fetch uniwap pools" req

funds :: MonadIO m => Instance -> AppM m UniswapDefinition
funds uId = do
  pabCfg <- asks pab
  hid <- toText <$> liftIO UUID.nextRandom
  let req = uniswapFunds pabCfg uId hid
  processRequest pabCfg uId hid "cannot fetch uniswap funds" req

stop :: MonadIO m => Instance -> AppM m UniswapDefinition
stop uId = do
  pabCfg <- asks pab
  hid <- toText <$> liftIO UUID.nextRandom
  let req = uniswapStop pabCfg uId hid
  processRequest pabCfg uId hid "cannot stop an uniswap instance" req

status :: (MonadIO m) => Instance -> AppM m UniswapStatusResponse
status uId = do
  pabCfg <- asks pab
  result <- pabStatus pabCfg uId
  case result of
    Left err -> throwError err400{errBody = encode . pack $ "Get Uniswap Instance status faile, cause: " ++ show err}
    Right r -> pure r

