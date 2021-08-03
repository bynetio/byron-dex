module Uniswap.Form.Field where

import Prelude
import Uniswap.Component.HTML.Utils (css, maybeElem)
import Uniswap.Form.Validation (errorToString)
import Uniswap.Form.Validation as V
import DOM.HTML.Indexed (HTMLinput)
import Data.Newtype (class Newtype)
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Formless as F
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Type.Proxy (Proxy)
import Type.Row as Row

submit :: forall i p. String -> HH.HTML i p
submit buttonText =
  HH.input
    [ css "btn btn-lg btn-primary pull-xs-right"
    , HP.type_ HP.InputSubmit
    , HP.value buttonText
    ]

input ::
  forall form act slots m sym fields inputs out t0 t1.
  IsSymbol sym =>
  Newtype (form Record F.FormField) { | fields } =>
  Newtype (form Variant F.InputFunction) (Variant inputs) =>
  Row.Cons sym (F.FormField V.FormError String out) t0 fields =>
  Row.Cons sym (F.InputFunction V.FormError String out) t1 inputs =>
  Proxy sym ->
  form Record F.FormField ->
  Array (HH.IProp HTMLinput (F.Action form act)) ->
  F.ComponentHTML form act slots m
input sym form props =
  HH.fieldset
    [ css "form-group" ]
    [ HH.input
        ( append
            [ css "form-control form-control-lg"
            , HP.value $ F.getInput sym form
            , HE.onValueInput $ F.setValidate sym
            ]
            props
        )
    , maybeElem (F.getError sym form) \err ->
        HH.div
          [ css "error-messages" ]
          [ HH.text $ errorToString err ]
    ]
