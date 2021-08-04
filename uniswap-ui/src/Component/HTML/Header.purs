module Uniswap.Component.HTML.Header where

import Prelude
import Data.Maybe (Maybe, isJust, isNothing)
import Data.Monoid (guard)
import Halogen.HTML as HH
import Uniswap.Component.HTML.Utils (css, safeHref, whenElem)
import Uniswap.Data.Route (Route(..))
import Uniswap.Data.Wallet (Wallet)

header :: forall i p r. (Maybe Wallet) -> Route -> HH.HTML i p
header currentWallet route =
  HH.nav
    [ css "navbar navbar-default navbar-fixed-top" ]
    [ HH.div
        [ css "container" ]
        [ HH.a
            [ css "navbar-brand"
            , safeHref Home
            ]
            [ HH.text "uniswap" ]
        , HH.ul
            [ css "nav navbar-nav pull-xs-right" ]
            [ navItem Pools
                [ HH.text "Pools" ]
            , whenElem (isNothing currentWallet) \_ ->
                navItem (ConnectWallet)
                  [ HH.text "Connect Wallet" ]
            ]
        ]
    ]
  where
  navItem r html =
    HH.li
      [ css "nav-item" ]
      [ HH.a
          [ css $ "nav-link" <> guard (route == r) " active"
          , safeHref r
          ]
          html
      ]
