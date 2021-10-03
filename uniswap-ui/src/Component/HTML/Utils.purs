module Uniswap.Component.HTML.Utils where

import Prelude
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Network.RemoteData (RemoteData(..))
import Routing.Duplex (print)
import Uniswap.Data.Funds (Fund)
import Uniswap.Data.LiquidityPool (LiquidityPoolView)
import Uniswap.Data.Route (Route, routeCodec)

css :: forall r i. String -> HH.IProp ( class :: String | r ) i
css = HP.class_ <<< HH.ClassName

safeHref :: forall r i. Route -> HH.IProp ( href :: String | r ) i
safeHref = HP.href <<< append "#" <<< print routeCodec

whenElem :: forall p i. Boolean -> (Unit -> HH.HTML p i) -> HH.HTML p i
whenElem cond f = if cond then f unit else HH.text ""

maybeElem :: forall p i a. Maybe a -> (a -> HH.HTML p i) -> HH.HTML p i
maybeElem (Just x) f = f x

maybeElem _ _ = emptyText

emptyText :: forall w i. HH.HTML w i
emptyText = HH.text ""

toRight :: forall a w i. Maybe a -> Either (HH.HTML w i) a
toRight mba = note emptyText mba

type PoolsWithFundsRemote
  = { pools :: RemoteData String (Array LiquidityPoolView)
    , funds :: RemoteData String (Array Fund)
    }

type PoolsWithFunds
  = { pools :: Array LiquidityPoolView
    , funds :: Array Fund
    }

loadRemoteData :: forall w i. PoolsWithFundsRemote -> Either (HH.HTML w i) PoolsWithFunds
loadRemoteData input = do
  pools <- load input.pools "pools"
  funds <- load input.funds "wallets funds"
  pure $ { pools, funds }

load :: forall a w i. RemoteData String (Array a) -> String -> Either (HH.HTML w i) (Array a)
load remoteData what = case remoteData of
  NotAsked -> Left $ HH.text (what <> " not loaded")
  Loading -> Left $ HH.text ("Loading  " <> what <> "...")
  Failure err -> Left $ HH.text ("Error during loading " <> what <> ": " <> err)
  Success xa -> Right xa
