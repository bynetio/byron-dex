module Uniswap.Api.Utils (mkRequest, decode) where

import Prelude
import Affjax (request)
import Data.Argonaut.Core (Json)
import Data.Bifunctor (rmap)
import Data.Codec.Argonaut (JsonCodec, printJsonDecodeError)
import Data.Codec.Argonaut as CA
import Data.Either (Either(..), hush)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff, liftAff)
import Halogen.Store.Monad (class MonadStore, getStore)
import Uniswap.Api.Request (RequestOptions, defaultRequest)
import Uniswap.Capability.Logger (class Logger, logError)
import Uniswap.Capability.Now (class Now)
import Uniswap.Store (Action, Store)

mkRequest ::
  forall m.
  MonadAff m =>
  MonadStore Action Store m =>
  RequestOptions ->
  m (Maybe Json)
mkRequest opts = do
  { apiUrl, currentWallet } <- getStore
  case currentWallet of
    Just wallet -> do
      response <- liftAff $ request $ defaultRequest wallet apiUrl opts
      pure $ hush $ rmap _.body response
    Nothing -> pure Nothing

decode :: forall m a. Logger m => Now m => JsonCodec a -> Maybe Json -> m (Maybe a)
decode _ Nothing = logError "Response malformed" *> pure Nothing

decode codec (Just json) = case CA.decode codec json of
  Left err -> logError (printJsonDecodeError err) *> pure Nothing
  Right response -> pure (Just response)
