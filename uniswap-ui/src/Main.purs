module Main where

import Prelude
import Uniswap.AppM (runAppM)
import Uniswap.Data.Wallet (Wallet)
import Uniswap.Store (Store)
import Uniswap.Component.Router as Router
import Uniswap.Data.Route (routeCodec)
import Routing.Duplex (parse)
import Routing.Hash (matchesWith)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Halogen as H
import Halogen (liftEffect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main =
  HA.runHalogenAff do
    body <- HA.awaitBody
    let
      currentWallet :: Maybe Wallet
      currentWallet = Nothing

      initialStore :: Store
      initialStore = { currentWallet }
    rootComponent <- runAppM initialStore Router.component
    halogenIO <- runUI rootComponent unit body
    void $ liftEffect
      $ matchesWith (parse routeCodec) \old new ->
          when (old /= Just new) do
            launchAff_ $ halogenIO.query $ H.mkTell $ Router.Navigate new
