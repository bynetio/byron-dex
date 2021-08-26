module Uniswap.Component.HTML.Header where

import Prelude
import Data.Maybe (Maybe, isJust, isNothing)
import Data.Monoid (guard)
import Halogen.HTML as HH
import Uniswap.Component.HTML.Utils (css, safeHref, whenElem)
import Uniswap.Data.Route (Route(..))
import Uniswap.Data.Wallet (Wallet)

header :: forall i p. (Maybe Wallet) -> Route -> HH.HTML i p
header currentWallet route =
  HH.nav
    [ css "navbar is-fixed-top is-info" ]
    [ HH.div
        [ css "navbar-brand" ]
        [ HH.a
            [ css "navbar-item"
            , safeHref Home
            ]
            [ HH.text "Uniswap" ]
        , whenElem (isJust currentWallet) \_ ->
            navItem Pools [ HH.text " Pools" ]
        , whenElem (isJust currentWallet) \_ ->
            navItem Funds [ HH.text " Funds" ]
        , whenElem (isJust currentWallet) \_ ->
            navItem Swap [ HH.text " Swap" ]
        , whenElem (isNothing currentWallet) \_ ->
            navItem ConnectWallet [ HH.text "Connect Wallet" ]
        ]
    ]
  where
  navItem r html =
    HH.a
      [ css "navbar-item" ]
      [ HH.a
          [ css $ "navbar-link" <> guard (route == r) " is-active"
          , safeHref r
          ]
          html
      ]
