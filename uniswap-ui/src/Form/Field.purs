module Uniswap.Form.Field where

import Prelude
import DOM.HTML.Indexed (HTMLinput)
import Data.Either (Either(..), either)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Uniswap.Component.HTML.Utils (css)

submit :: forall i p. String -> HH.HTML i p
submit buttonText =
  HH.input
    [ css "button is-link is-light"
    , HP.type_ HP.InputSubmit
    , HP.value buttonText
    ]

field :: forall i p. { label :: String, help :: Either String String } -> Array (HH.HTML i p) -> HH.HTML i p
field config contents =
  HH.div
    [ css "field" ]
    [ HH.div
        [ css "label" ]
        [ HH.text config.label ]
    , HH.div
        [ css "control" ]
        contents
    , case config.help of
        Left str -> helpError' str
        Right str -> help' str
    ]
  where
  help' str = HH.p [ css "help" ] [ HH.text str ]

  helpError' str = HH.p [ css "help is-danger" ] [ HH.text str ]

type InputConfig
  = { label :: String, help :: Either String String, placeholder :: String, inputType :: HP.InputType }

input' :: forall i p. InputConfig -> Array (HH.IProp HTMLinput p) -> HH.HTML i p
input' config props =
  field
    { label: config.label, help: config.help }
    [ HH.input
        $ [ HP.type_ config.inputType
          , either (const $ css "input is-danger") (const $ css "input") config.help
          , HP.placeholder config.placeholder
          ]
        <> props
    ]
