module Uniswap.Api.Request
  ( ApiURL(..)
  , RequestMethod(..)
  , RequestOptions(..)
  , defaultRequest
  ) where

import Prelude
import Affjax (Request)
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Data.Argonaut.Core (Json)
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Routing.Duplex (print)
import Uniswap.Api.Endpoint (Endpoint, endpointCodec)
import Uniswap.Data.Wallet (Wallet)

newtype ApiURL
  = ApiURL String

data RequestMethod
  = Get
  | Post (Maybe Json)

type RequestOptions
  = { endpoint :: Endpoint
    , method :: RequestMethod
    }

defaultRequest :: Wallet -> ApiURL -> RequestOptions -> Request Json
defaultRequest w (ApiURL apiUrl) { endpoint, method } =
  { method: Left requestMethod
  , url: apiUrl <> print (endpointCodec w) endpoint
  , headers: []
  , content: RB.json <$> body
  , username: Nothing
  , password: Nothing
  , timeout: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  }
  where
  Tuple requestMethod body = case method of
    Get -> Tuple GET Nothing
    Post body -> Tuple POST body
