module Main where

import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen (liftEffect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import Uniswap.Api.Request (ApiURL(..))
import Uniswap.AppM (runAppM)
import Uniswap.Component.Router as Router
import Uniswap.Data.Route (routeCodec)
import Uniswap.Data.Wallet (Wallet)
import Uniswap.Store (Store)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    let
      currentWallet :: Maybe Wallet
      currentWallet = Nothing

      apiUrl :: ApiURL
      apiUrl = ApiURL "http://localhost:3000"

      initialStore :: Store
      initialStore = { apiUrl, currentWallet }
    rootComponent <- runAppM initialStore Router.component
    halogenIO <- runUI rootComponent unit body
    void $ liftEffect
      $ matchesWith (parse routeCodec) \old new ->
          when (old /= Just new) do
            launchAff_ $ halogenIO.query $ H.mkTell $ Router.Navigate new
