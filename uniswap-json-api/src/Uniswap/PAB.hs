{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Uniswap.PAB
  where

import           Control.DeepSeq              (NFData (..))
import           Control.Monad                (join)
import           Control.Monad.Freer          (Eff, LastMember, Member, Members,
                                               interpret, interpretM, send,
                                               type (~>))
import           Control.Monad.Freer.Error    (catchError, handleError,
                                               runError, throwError)
import           Control.Monad.Freer.TH       (makeEffect)
import           Control.Monad.IO.Class       (MonadIO, liftIO)
import           Data.Aeson                   (ToJSON (toJSON), Value)
import           Data.Aeson.Text              (encodeToLazyText)
import           Data.Either.Combinators      (mapLeft, maybeToRight)
import           Data.Text                    (Text, pack)
import           Data.Text.Lazy               (toStrict)
import           PyF                          (fmt)
import           Servant                      (Capture, Get, JSON, Post,
                                               Proxy (..), Put, ReqBody,
                                               type (:<|>), type (:>))
import           Servant.API                  (type (:<|>) ((:<|>)))
import           Servant.Client.Streaming     (ClientError, ClientM, client)
import           Uniswap.Common.AppError      (AppError,
                                               Err (CallStatusFailed, EndpointRequestFailed, GetStatusFailed, StatusNotFound))
import           Uniswap.Common.Logger        (Logger, logDebug, logError)
import           Uniswap.Common.NextId        (NextId, next)
import           Uniswap.Common.ServantClient (ServantClient, runClient')
import           Uniswap.Common.Utils         (Time, fromEither, showText,
                                               sleep)
import           Uniswap.PAB.Types            (AddParams, CloseParams,
                                               CreateParams, ISwapPreviewParams,
                                               IndirectSwapParams, RemoveParams,
                                               SwapParams, SwapPreviewParams,
                                               WithHistoryId (WithHistoryId))
import           UniswapJsonApi.Types         (History (..), HistoryId,
                                               Instance,
                                               UniswapCurrentState (observableState),
                                               UniswapMethodResult,
                                               UniswapStatusResponse (UniswapStatusResponse, cicCurrentState),
                                               lookupHistory)

data UniswapPab r where
  Pools               :: Instance -> UniswapPab UniswapMethodResult
  Funds               :: Instance -> UniswapPab UniswapMethodResult
  Stop                :: Instance -> UniswapPab UniswapMethodResult
  Status              :: Instance -> UniswapPab UniswapStatusResponse
  Create              :: Instance -> CreateParams -> UniswapPab UniswapMethodResult
  Close               :: Instance -> CloseParams -> UniswapPab UniswapMethodResult
  Add                 :: Instance -> AddParams -> UniswapPab UniswapMethodResult
  Remove              :: Instance -> RemoveParams -> UniswapPab UniswapMethodResult
  Swap                :: Instance -> SwapParams -> UniswapPab UniswapMethodResult
  SwapPreview         :: Instance -> SwapPreviewParams -> UniswapPab UniswapMethodResult
  IndirectSwap        :: Instance -> IndirectSwapParams -> UniswapPab UniswapMethodResult
  IndirectSwapPreview :: Instance -> ISwapPreviewParams -> UniswapPab UniswapMethodResult

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
      Pools i                 -> doRequest i () "pools" "cannot fetch uniswap pools"
      Funds i                 -> doRequest i () "funds" "cannot fetch uniswap funds"
      Status i                -> fetchStatus i
      Stop i                  -> doRequest i () "stop" "cannot stop an uniswap instance"
      Create       i params   -> doRequest i params "create" "cannot create a pool"
      Close        i params   -> doRequest i params "close" "cannot close a pool"
      Add          i params   -> doRequest i params "add" "cannot add coins to pool"
      Remove       i params   -> doRequest i params "remove" "cannot remove liquidity tokens"
      Swap         i params   -> doRequest i params "swap" "cannot make a swap"
      SwapPreview  i params   -> doRequest i params "swapPreview" "cannot make a swap (preview)"
      IndirectSwap i params   -> doRequest i params "iSwap" "cannot make a indirect swap"
      IndirectSwapPreview i p -> doRequest i p "iSwapPreview" "cannot make a indirect swap (preview)"
    )

doRequest
  :: forall m effs a. (ToJSON a, MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> a
  -> Text
  -> Text
  -> Eff effs UniswapMethodResult
doRequest uid a endpoint errMsg = do
  hid <- next
  doEndpointRequest uid hid a endpoint errMsg
  getStatusByHistoryId uid hid


fetchStatus
  :: forall m effs. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> Eff effs UniswapStatusResponse
fetchStatus uID = do
  logDebug [fmt|Fetching status for {uID}|]
  result <- runClient' . statusAPI $ uID
  logDebug [fmt|[Status for {uID} is {result:s}|]
  case result of
    Left err -> throwError $ GetStatusFailed err
    Right r  -> pure r

history :: UniswapStatusResponse -> History UniswapMethodResult
history = observableState . cicCurrentState


extractUniswapDef :: HistoryId -> UniswapStatusResponse -> Either Text UniswapMethodResult
extractUniswapDef hid r = maybeToRight "History ID not found in history" $ lookupHistory hid $ history r


extractStatus :: HistoryId -> Either ClientError UniswapStatusResponse -> Either Text UniswapMethodResult
extractStatus hid e = extractUniswapDef hid =<< mapLeft showText e


data ShouldRetry a b
  = Retry a
  | Ok b

retryRequest
  :: forall effs e a. (Members '[Logger, Time] effs, Show e, Show a)
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
       Retry e -> do
         sleep (100000 * 2^numRetries) -- metody typu add i create wymagają odczekania koło 10 sekund
         logError $ showText e
         logDebug [fmt|Request failed. So far we have retried {numRetries} times.|]
         retryInner (numRetries + 1)
       Ok resp -> do
         pure $ Right resp


getStatusByHistoryId
  :: forall m effs a. (MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> HistoryId
  -> Eff effs UniswapMethodResult
getStatusByHistoryId uid hid =
  extractDefRetry
    where
      -- | I should use fetchStatus instead
      fetchStatus' = do
        result <- runClient' $ statusAPI uid
        pure $ case result of
          Left err -> Retry . GetStatusFailed $ err
          Right r  -> Ok r

      fetchStatusRetry = do
        res <- retryRequest 4 fetchStatus'
        fromEither res
      extractDef = do
        status <- fetchStatusRetry
        pure $ either Retry Ok . extractUniswapDef hid $ status
      extractDefRetry = do
        def <- retryRequest 20 extractDef
        fromEither $ mapLeft (StatusNotFound hid) def


doEndpointRequest
  :: forall m effs a. (ToJSON a, MonadIO m, LastMember m effs, Members (UniswapPabEffs m) effs)
  => Instance
  -> HistoryId
  -> a
  -> Text
  -> Text
  -> Eff effs ()
doEndpointRequest uId hid a endpoint errMsg = do
  callRes <- retryRequest 4 doSingle
  fromEither $ mapLeft (EndpointRequestFailed errMsg) callRes
    where
      doSingle = do
        let ahid  = WithHistoryId hid a
            value = toJSON ahid
        logDebug $ toStrict . encodeToLazyText $ ahid
        res <- runClient' $ endpointAPI uId endpoint value
        pure $ either Retry Ok res

