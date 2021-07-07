{-# LANGUAGE OverloadedStrings #-}

module UniswapJsonApi.Logic where

import Control.Monad.IO.Class
import Control.Retry
import Data.Aeson (encode)
import Data.Either (isLeft)
import Data.Text
import Servant
import Servant.Client
import UniswapJsonApi.Client
import UniswapJsonApi.Model

limitedBackoff :: RetryPolicy
limitedBackoff = exponentialBackoff 50 <> limitRetries 5

processRawRequest :: Show a => IO a -> Handler a
processRawRequest action = do
  response <- liftIO action
  liftIO $ print response
  return response

processRequest :: Show a => Config -> Text -> String -> IO (Either ClientError a) -> Handler UniswapStatusResponse
processRequest c uId errorMessage action = do
  let endpointResult _ = processRawRequest action
  endpointResponse <- retrying limitedBackoff (const $ return . isLeft) endpointResult
  case endpointResponse of
    Right _ -> do
      let statusResult _ = processRawRequest $ pabStatus c uId
      statusResponse <- retrying limitedBackoff (const $ return . isLeft) statusResult
      case statusResponse of
        Right r -> return r
        Left _ -> throwError err422 {errBody = encode . pack $ "cannot fetch status of last operation"}
    Left _ -> throwError err422 {errBody = encode . pack $ errorMessage}

create :: Config -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler UniswapStatusResponse
create c uId (Just cA) (Just cB) (Just aA) (Just aB) = processRequest c uId "cannot create a pool" $ uniswapCreate c uId cA cB aA aB
create c uId _ _ _ _ = throwError err400

swap :: Config -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Int -> Handler UniswapStatusResponse
swap c uId (Just cA) (Just cB) (Just aA) (Just aB) (Just s) = processRequest c uId "cannot make a swap" $ uniswapSwap c uId cA cB aA aB s
swap c uId _ _ _ _ _ = throwError err400

swapPreview :: Config -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Handler UniswapStatusResponse
swapPreview c uId (Just cA) (Just cB) (Just a) = processRequest c uId "cannot make a swap" $ uniswapSwapPreview c uId cA cB a
swapPreview c uId _ _ _ = throwError err400

indirectSwap :: Config -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Int -> Handler UniswapStatusResponse
indirectSwap c uId (Just cA) (Just cB) (Just aA) (Just aB) (Just s) = processRequest c uId "cannot make an indirect swap" $ uniswapIndirectSwap c uId cA cB aA aB s
indirectSwap c uId _ _ _ _ _ = throwError err400

indirectSwapPreview :: Config -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Handler UniswapStatusResponse
indirectSwapPreview c uId (Just cA) (Just cB) (Just a) = processRequest c uId "cannot make an indirect swap preview" $ uniswapIndirectSwapPreview c uId cA cB a
indirectSwapPreview c uId _ _ _ = throwError err400

close :: Config -> Text -> Maybe Text -> Maybe Text -> Handler UniswapStatusResponse
close c uId (Just cA) (Just cB) = processRequest c uId "cannot close a pool" $ uniswapClose c uId cA cB
close c uId _ _ = throwError err400

remove :: Config -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Handler UniswapStatusResponse
remove c uId (Just cA) (Just cB) (Just a) = processRequest c uId "cannot remove liquidity tokens" $ uniswapRemove c uId cA cB a
remove c uId _ _ _ = throwError err400

add :: Config -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler UniswapStatusResponse
add c uId (Just cA) (Just cB) (Just aA) (Just aB) = processRequest c uId "cannot add coins to pool" $ uniswapAdd c uId cA cB aA aB
add c uId _ _ _ _ = throwError err400

pools :: Config -> Text -> Handler UniswapStatusResponse
pools c uId = processRequest c uId "cannot fetch uniwap pools" $ uniswapPools c uId

funds :: Config -> Text -> Handler UniswapStatusResponse
funds c uId = processRequest c uId "cannot fetch uniswap funds" $ uniswapFunds c uId

stop :: Config -> Text -> Handler UniswapStatusResponse
stop c uId = processRequest c uId "cannot stop an uniswap instance" $ uniswapStop c uId

status :: Config -> Text -> Handler UniswapStatusResponse
status c uId = do
  result <- processRawRequest $ pabStatus c uId
  case result of
    Left _ -> undefined
    Right r -> return r
