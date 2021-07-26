{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Uniswap.Common.ServantServer
  where

import           Control.Monad.Except      (ExceptT (..))
import           Control.Monad.Freer
import           Control.Monad.Freer.Error
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Data                 (Proxy (..))
import           Data.Function             ((&))
import           Data.Proxy                (Proxy (..))
import           GHC.TypeLits              (Nat)
import qualified Network.Wai.Handler.Warp  as Warp
import           Servant                   (Application, Handler (..), HasServer, Header, Headers, JSON,
                                            NoContent (..), Server, ServerError, ServerT, StdMethod (GET),
                                            ToHttpApiData, Verb, addHeader, hoistServer, runHandler, serve)


-- -- | Make a Servant 'Handler' run in a Freer 'Eff' instead.
-- liftHandler
--   :: forall effs a. (LastMember IO effs, Members '[Error ServerError, IO] effs)
--   => Handler a
--   -> Eff effs a
-- liftHandler handler = do
--    handler' <- liftIO $ runHandler handler
--    fromEither handler'

-- | Turn a 'Eff' that can throw 'ServerError's into a Servant 'Handler'.
effHandler
  :: (forall x. Eff effs x -> IO x)
  -> Eff (Error ServerError ': effs) a
  -> Handler a
effHandler lowerToIO =
  Handler . ExceptT . lowerToIO . runError




