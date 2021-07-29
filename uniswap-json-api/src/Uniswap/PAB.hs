{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Uniswap.PAB
  where

import Control.DeepSeq              (NFData (..))
import Control.Monad                (join)
import Control.Monad.Freer          (Eff, LastMember, Member, Members, interpret, interpretM, send, type (~>))
import Control.Monad.Freer.Error
import Control.Monad.Freer.TH       (makeEffect)
import Control.Monad.IO.Class       (MonadIO, liftIO)
import Data.Aeson                   (ToJSON (toJSON), Value)
import Data.Aeson.Text
import Data.Either.Combinators
import Data.Functor
import Data.Text                    (Text, pack)
import Data.Text.Lazy               (toStrict)
import PyF
import Servant                      (Capture, Get, JSON, Post, Proxy (..), Put, ReqBody, type (:<|>),
                                     type (:>))
import Servant.API                  (type (:<|>) ((:<|>)))
import Servant.Client.Streaming     (ClientError, ClientM, client)
import Uniswap.Common.AppError
import Uniswap.Common.Logger
import Uniswap.Common.NextId
import Uniswap.Common.ServantClient
import Uniswap.Common.Utils
import Uniswap.PAB.Types
import UniswapJsonApi.Types

data UniswapPab r where
  Pools               :: Instance -> UniswapPab UniswapDefinition
  Funds               :: Instance -> UniswapPab UniswapDefinition
  Stop                :: Instance -> UniswapPab UniswapDefinition
  Status              :: Instance -> UniswapPab UniswapStatusResponse
  Create              :: Instance -> CreateParams -> UniswapPab UniswapDefinition
  Close               :: Instance -> CloseParams -> UniswapPab UniswapDefinition
  Add                 :: Instance -> AddParams -> UniswapPab UniswapDefinition
  Remove              :: Instance -> RemoveParams -> UniswapPab UniswapDefinition
  Swap                :: Instance -> SwapParams -> UniswapPab UniswapDefinition
  SwapPreview         :: Instance -> SwapPreviewParams -> UniswapPab UniswapDefinition
  IndirectSwap        :: Instance -> IndirectSwapParams -> UniswapPab UniswapDefinition
  IndirectSwapPreview :: Instance -> ISwapPreviewParams -> UniswapPab UniswapDefinition

makeEffect ''UniswapPab

type UniswapPabEffs m =
  '[ ServantClient
   , NextId
   , Logger
   , AppError
   , Time
   , m
   ]

type UniswapAPI =
  "api" :> "new" :> "contract" :> "instance" :> Capture "instance-id" Text :> "status" :> Get '[JSON] UniswapStatusResponse
        :<|> "api" :> "new" :> "contract" :> "instance" :> Capture "instance-id" Text :> "endpoint" :> Capture "endpoint-name" Text :> ReqBody '[JSON] Value :> Post '[JSON] ()
        :<|> "api" :> "new" :> "contract" :> "instance" :> Capture "instance-id" Text :> "stop" :> Put '[JSON] ()

uniswapApi :: Proxy UniswapAPI
uniswapApi = Proxy

uniswap = client uniswapApi

statusAPI :: Text -> ClientM UniswapStatusResponse
endpointAPI :: Text -> Text -> Value -> ClientM ()
stopAPI :: Text -> ClientM ()
(statusAPI :<|> endpointAPI :<|> stopAPI) = uniswap


-- | Interpret the 'UniswapPab' effect.
runPab
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Eff (UniswapPab ': effs)
  ~> Eff effs
runPab =
  interpret (\case
      Pools i                 -> getPools i
      Funds i                 -> getFunds i
      Status i                -> fetchStatus i
      Stop i                  -> doStop i
      Create       i p        -> doCreate i p
      Close        i p        -> doClose i p
      Add          i p        -> doAdd i p
      Remove       i p        -> doRemove i p
      Swap         i p        -> doSwap i p
      SwapPreview  i p        -> doSwapPreview i p
      IndirectSwap i p        -> doIndirectSwap i p
      IndirectSwapPreview i p -> doIndirectSwapPreview i p
    )

getPools
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> Eff effs UniswapDefinition
getPools uId =
  doReq uId () "pools" "cannot fetch uniswap pools"

getFunds
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> Eff effs UniswapDefinition
getFunds uId =
  doReq uId () "funds" "cannot fetch uniswap funds"

doStop
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> Eff effs UniswapDefinition
doStop uId =
  doReq uId () "stop" "cannot stop an uniswap instance"

doCreate
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> CreateParams
  -> Eff effs UniswapDefinition
doCreate uId params = do
  doReq uId params  "create" "cannot create a pool"

doClose
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> CloseParams
  -> Eff effs UniswapDefinition
doClose uId params = do
  doReq uId params "close" "cannot close a pool"

doAdd
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> AddParams
  -> Eff effs UniswapDefinition
doAdd uId params = do
  doReq uId params "add" "cannot add coins to pool"

doRemove
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> RemoveParams
  -> Eff effs UniswapDefinition
doRemove uId params = do
  doReq uId params "remove" "cannot remove liquidity tokens"

doSwap
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> SwapParams
  -> Eff effs UniswapDefinition
doSwap uId params = do
  doReq uId params "swap" "cannot make a swap"

doSwapPreview
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> SwapPreviewParams
  -> Eff effs UniswapDefinition
doSwapPreview uId params = do
  doReq uId params "swapPreview" "cannot make a swap (preview)"

doIndirectSwap
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> IndirectSwapParams
  -> Eff effs UniswapDefinition
doIndirectSwap uId params = do
  doReq uId params "iSwap" "cannot make a indirect swap"

doIndirectSwapPreview
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> ISwapPreviewParams
  -> Eff effs UniswapDefinition
doIndirectSwapPreview uId params = do
  doReq uId params "iSwapPreview" "cannot make a indirect swap (preview)"

doReq
  :: forall m effs a. (ToJSON a, MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> a
  -> Text
  -> Text
  -> Eff effs UniswapDefinition
doReq uId a endpoint errMsg = do
  hid <- next
  let ahid  = WithHistoryId hid a
      value = toJSON ahid
  logDebug $ toStrict . encodeToLazyText $ ahid
  callRes <- runClient' $ endpointAPI uId endpoint value
  fromEither $ mapLeft (EndpointRequestFailed errMsg) callRes
  response <- fetchStatus uId
  fromEither $ mapLeft (EndpointRequestFailedRes errMsg) (extractUniswapDef hid response)

fetchStatus
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> Eff effs UniswapStatusResponse
fetchStatus uID = do
  hid <- next
  logDebug [fmt|Fetching status for {uID}|]
  result <- runClient' . statusAPI $ uID
  logDebug [fmt|[Status for {uID} is {result:s}|]
  case result of
    Left err -> throwError $ GetStatusFailed err
    Right r  -> pure r


history :: UniswapStatusResponse -> History (Either Text UniswapDefinition)
history = observableState . cicCurrentState

extractUniswapDef :: HistoryId -> UniswapStatusResponse -> Either Text UniswapDefinition
extractUniswapDef hid = join . maybeToRight "Operation ID not found in history" . lookupHistory hid . history

extractStatus :: HistoryId -> Either ClientError UniswapStatusResponse -> Either Text UniswapDefinition
extractStatus hid e = extractUniswapDef hid =<< mapLeft showText e

doRequest
  :: forall effs a m. (ToJSON a, MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> a
  -> Eff effs UniswapDefinition
doRequest uid a = do
  hid <- next
  res <- doEndpointRequest uid hid a
  whenLeft res (throwError . CallEndpointFailed uid hid) -- FIXME: When pab endpoint request ends with error then throw error.
  def <- retryRequest 5 $ getStatusByHistoryId uid hid
  fromEither $ mapLeft (StatusNotFound hid) def


data ShouldRetry a b
  = Retry a
  | Fail a
  | Ok b

retryRequest
  :: forall effs e a. (Members '[Logger, Time] effs) -- FIXME: Add type alias for Members...
  => Integer
  -- ^ limit of retries
  -> Eff effs (ShouldRetry e a)
  -- ^ retryable action
  -> Eff effs (Either e a)
retryRequest maxRetries action = retryInner 0
  where
   retryInner numRetries = do
     res <- action
     case res of
       Retry err | numRetries > maxRetries -> do
         logDebug [fmt|Request failed. Giving up after {maxRetries} retries|]
         pure $ Left err
       Retry _ -> do
         sleep 50 -- Sleep 50 millis before next call
         logDebug [fmt|Request failed. So far we have retried {numRetries} times.|]
         retryInner (numRetries + 1)
       Fail err -> do
         logDebug "Request failed due to error response"
         pure $ Left err
       Ok resp ->
         pure $ Right resp


getStatusByHistoryId
  :: forall effs m. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> HistoryId
  -> Eff effs (ShouldRetry Text UniswapDefinition)
getStatusByHistoryId uid hid = do
  status <- fetchStatusR uid
  pure $ either Retry Ok . extractUniswapDef hid $ status

fetchStatus'
  :: forall effs. (Members '[ServantClient] effs)
  => Instance
  -> Eff effs (ShouldRetry ClientError UniswapStatusResponse)
fetchStatus' uID = do
  result <- runClient' . statusAPI $ uID
  pure $ either Fail Ok result

doSingleEndpointRequest
  :: forall effs req. (ToJSON req, Member ServantClient effs)
  => Instance
  -> HistoryId
  -> req
  -> Eff effs (ShouldRetry ClientError ())
doSingleEndpointRequest uId hid req = do
  let v = toJSON req
  resp <- runClient' $ endpointAPI uId hid v
  pure $ either Fail Ok resp -- change Fail to Retry

fetchStatusR
  :: forall effs m. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> Eff effs UniswapStatusResponse
fetchStatusR uid = do
  res <- retryRequest 5 . fetchStatus' $ uid
  case res of
    Left err -> throwError $ CallStatusFailed uid err
    Right r  -> pure r

doEndpointRequest
  :: forall effs m a. (ToJSON a, MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> HistoryId
  -> a
  -> Eff effs (Either ClientError ())
doEndpointRequest uId hid req =
  retryRequest 5 $ doSingleEndpointRequest uId hid req
