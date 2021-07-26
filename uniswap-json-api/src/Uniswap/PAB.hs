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
import Data.Either.Combinators
import Data.Functor
import Data.Text                    (Text, pack)
import PyF
import Servant                      (Capture, Get, JSON, Post, Proxy (..), Put, ReqBody, type (:<|>),
                                     type (:>))
import Servant.API                  (type (:<|>) ((:<|>)))
import Servant.Client.Streaming     (ClientError, ClientM, client)
import Uniswap.Common.AppError
import Uniswap.Common.Logger
import Uniswap.Common.NextOpID
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
   , NextOpID
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
  doReq uId PoolsParams "pools" "cannot fetch uniswap pools"

getFunds
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> Eff effs UniswapDefinition
getFunds uId =
  doReq uId FundsParams "funds" "cannot fetch uniswap funds"

doStop
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> Eff effs UniswapDefinition
doStop uId =
  doReq uId StopParams "stop" "cannot stop an uniswap instance"

doCreate
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> CreateParams
  -> Eff effs UniswapDefinition
doCreate uId params = do
  doReq' uId params cpOpId "create" "cannot create a pool"

doClose
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> CloseParams
  -> Eff effs UniswapDefinition
doClose uId params = do
  doReq' uId params clpOpId "close" "cannot close a pool"

doAdd
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> AddParams
  -> Eff effs UniswapDefinition
doAdd uId params = do
  doReq' uId params apOpId "add" "cannot add coins to pool"

doRemove
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> RemoveParams
  -> Eff effs UniswapDefinition
doRemove uId params = do
  doReq' uId params rpOpId "remove" "cannot remove liquidity tokens"

doSwap
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> SwapParams
  -> Eff effs UniswapDefinition
doSwap uId params = do
  doReq' uId params spOpId "swap" "cannot make a swap"

doSwapPreview
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> SwapPreviewParams
  -> Eff effs UniswapDefinition
doSwapPreview uId params = do
  doReq' uId params sppOpId "swapPreview" "cannot make a swap (preview)"

doIndirectSwap
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> IndirectSwapParams
  -> Eff effs UniswapDefinition
doIndirectSwap uId params = do
  doReq' uId params ispOpId "iSwap" "cannot make a indirect swap"

doIndirectSwapPreview
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> ISwapPreviewParams
  -> Eff effs UniswapDefinition
doIndirectSwapPreview uId params = do
  doReq' uId params isppOpId "iSwapPreview" "cannot make a indirect swap (preview)"


doReq
  :: forall m effs p. (ToJSON p, MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> (OperationId -> p)
  -> Text
  -> Text
  -> Eff effs UniswapDefinition
doReq uId p endpoint errMsg = do
  opId <- next
  let value = toJSON . p $ opId
  callRes <- runClient' $ endpointAPI uId endpoint value
  fromEither $ mapLeft (EndpointRequestFailed errMsg) callRes
  response <- fetchStatus uId
  fromEither $ mapLeft (EndpointRequestFailedRes errMsg) (extractUniswapDef opId response)

doReq'
  :: forall m effs p. (ToJSON p, MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> p
  -> (p -> OperationId)
  -> Text
  -> Text
  -> Eff effs UniswapDefinition
doReq' uId p opId endpoint errMsg = do
  let value = toJSON p
  callRes <- runClient' $ endpointAPI uId endpoint value
  fromEither $ mapLeft (EndpointRequestFailed errMsg) callRes
  response <- fetchStatus uId
  fromEither $ mapLeft (EndpointRequestFailedRes errMsg) (extractUniswapDef (opId p) response)

fetchStatus
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> Eff effs UniswapStatusResponse
fetchStatus uID = do
  opId <- next
  logDebug [fmt|Fetching status for {uID}|]
  result <- runClient' . statusAPI $ uID
  logDebug [fmt|[Status for {uID} is {result:s}|]
  case result of
    Left err -> throwError $ GetStatusFailed err
    Right r  -> pure r


history :: UniswapStatusResponse -> History (Either Text UniswapDefinition)
history = observableState . cicCurrentState

extractUniswapDef :: OperationId -> UniswapStatusResponse -> Either Text UniswapDefinition
extractUniswapDef opId = join . maybeToRight "Operation ID not found in history" . lookupHistory opId . history

extractStatus :: OperationId -> Either ClientError UniswapStatusResponse -> Either Text UniswapDefinition
extractStatus opId e = extractUniswapDef opId =<< mapLeft showText e

doRequest
  :: forall effs a m. (ToJSON a, MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> a
  -> Eff effs UniswapDefinition
doRequest uid a = do
  opId <- next
  res <- doEndpointRequest uid opId a
  whenLeft res (throwError . CallEndpointFailed uid opId) -- FIXME: When pab endpoint request ends with error then throw error.
  def <- retryRequest 5 $ getStatusByOpId uid opId
  fromEither $ mapLeft (StatusNotFound opId) def


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


getStatusByOpId
  :: forall effs m. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> OperationId
  -> Eff effs (ShouldRetry Text UniswapDefinition)
getStatusByOpId uid opId = do
  status <- fetchStatusR uid
  pure $ either Retry Ok . extractUniswapDef opId $ status

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
  -> OperationId
  -> req
  -> Eff effs (ShouldRetry ClientError ())
doSingleEndpointRequest uId opId req = do
  let v = toJSON req
  resp <- runClient' $ endpointAPI uId opId v
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
  -> OperationId
  -> a
  -> Eff effs (Either ClientError ())
doEndpointRequest uId opId req =
  retryRequest 5 $ doSingleEndpointRequest uId opId req


