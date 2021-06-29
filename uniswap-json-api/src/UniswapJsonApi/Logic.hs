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

processRequest :: Show a => Config -> Text -> String -> (a -> b) -> IO (Either ClientError a) -> Handler b
processRequest c uId errorMessage f action = do
  let result _ = processRawRequest action
  response <- retrying limitedBackoff (const $ return . isLeft) result
  status <- processRawRequest $ pabStatus c uId
  case response of
    Right r -> return $ f r
    Left _ -> throwError err422 {errBody = encode . pack $ errorMessage}

create :: Config -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler ()
create c uId (Just cA) (Just cB) (Just aA) (Just aB) = processRequest c uId "cannot create a pool" (const ()) $ uniswapCreate c uId cA cB aA aB
create c uId _ _ _ _ = throwError err400

swap :: Config -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Int -> Handler ()
swap c uId (Just cA) (Just cB) (Just aA) (Just aB) (Just s) = processRequest c uId "cannot make a swap" (const ()) $ uniswapSwap c uId cA cB aA aB s
swap c uId _ _ _ _ _ = throwError err400

swapPreview :: Config -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Handler ()
swapPreview c uId (Just cA) (Just cB) (Just a) = processRequest c uId "cannot make a swap" (const ()) $ uniswapSwapPreview c uId cA cB a
swapPreview c uId _ _ _ = throwError err400

indirectSwap :: Config -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Maybe Int -> Handler ()
indirectSwap c uId (Just cA) (Just cB) (Just aA) (Just aB) (Just s) = processRequest c uId "cannot make an indirect swap" (const ()) $ uniswapIndirectSwap c uId cA cB aA aB s
indirectSwap c uId _ _ _ _ _ = throwError err400

indirectSwapPreview :: Config -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Handler ()
indirectSwapPreview c uId (Just cA) (Just cB) (Just a) = processRequest c uId "cannot make an indirect swap preview" (const ()) $ uniswapIndirectSwapPreview c uId cA cB a
indirectSwapPreview c uId _ _ _ = throwError err400

close :: Config -> Text -> Maybe Text -> Maybe Text -> Handler ()
close c uId (Just cA) (Just cB) = processRequest c uId "cannot close a pool" (const ()) $ uniswapClose c uId cA cB
close c uId _ _ = throwError err400

remove :: Config -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Handler ()
remove c uId (Just cA) (Just cB) (Just a) = processRequest c uId "cannot remove liquidity tokens" (const ()) $ uniswapRemove c uId cA cB a
remove c uId _ _ _ = throwError err400

add :: Config -> Text -> Maybe Text -> Maybe Text -> Maybe Int -> Maybe Int -> Handler ()
add c uId (Just cA) (Just cB) (Just aA) (Just aB) = processRequest c uId "cannot add coins to pool" (const ()) $ uniswapAdd c uId cA cB aA aB
add c uId _ _ _ _ = throwError err400

pools :: Config -> Text -> Handler ()
pools c uId = processRequest c uId "cannot fetch uniwap pools" (const ()) $ uniswapPools c uId

funds :: Config -> Text -> Handler ()
funds c uId = processRequest c uId "cannot fetch uniswap funds" (const ()) $ uniswapFunds c uId

stop :: Config -> Text -> Handler ()
stop c uId = processRequest c uId "cannot stop an uniswap instance" (const ()) $ uniswapStop c uId

status :: Config -> Text -> Handler UniswapStatusResponse
status c uId = do
  result <- processRawRequest $ pabStatus c uId
  case result of
    Left _ -> undefined
    Right r -> return r
