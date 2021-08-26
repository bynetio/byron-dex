module Uniswap.Form.Validation where

import Prelude
import Data.BigInt (fromString)
import Data.Decimal (Decimal)
import Data.Decimal as Decimal
import Data.Either (Either(..), note)
import Data.Lens (preview)
import Data.Maybe (Maybe(..), maybe)
import Data.Show (show)
import Data.String as String
import Formless as F
import Uniswap.Data.Amount (Amount(..))
import Uniswap.Data.Coin (Coin(..), CurrencySymbol(..), TokenName(..))
import Uniswap.Data.Fee (Fee)

data FormError
  = Required
  | TooShort
  | TooLong
  | InvalidTokenName
  | InvalidCurrencySymbol
  | InvalidAmount
  | InvalidFee

instance toTextFieldError :: ToText FormError where
  toText Required = "This field is required."
  toText TooShort = "Not enough characters entered"
  toText TooLong = "Too many characters entered"
  toText InvalidTokenName = "Invalid Token Name"
  toText InvalidCurrencySymbol = "Invalid Currency Symbol"
  toText InvalidAmount = "Invalid Amount Format"
  toText InvalidFee = "Invalid Fee format"

errorToString :: FormError -> String
errorToString = case _ of
  Required -> "This field is required."
  TooShort -> "Not enough characters entered"
  TooLong -> "Too many characters entered"
  InvalidTokenName -> "Invalid Token Name"
  InvalidCurrencySymbol -> "Invalid Currency Symbol"
  InvalidAmount -> "Invalid Amount Format"
  InvalidFee -> "Invalid Fee format"

required :: forall form m a. Eq a => Monoid a => Monad m => F.Validation form m FormError a a
required = F.hoistFnE_ $ cond (_ /= mempty) Required

exists :: forall form m a. Monad m => F.Validation form m FormError (Maybe a) a
exists = F.hoistFnE_ $ maybe (Left Required) Right

minLength :: forall form m. Monad m => Int -> F.Validation form m FormError String String
minLength n = F.hoistFnE_ $ cond (\str -> String.length str > n) TooShort

maxLength :: forall form m. Monad m => Int -> F.Validation form m FormError String String
maxLength n = F.hoistFnE_ $ cond (\str -> String.length str <= n) TooLong

tokenNameFormat :: forall form m. Monad m => F.Validation form m FormError String TokenName
tokenNameFormat = F.hoistFnE_ $ map TokenName <<< cond (\str -> String.length str > 0) InvalidTokenName

currencySymbolFormat :: forall form m. Monad m => F.Validation form m FormError String CurrencySymbol
currencySymbolFormat = F.hoistFnE_ $ map CurrencySymbol <<< cond (\str -> String.length str > 0) InvalidCurrencySymbol

amountFormat :: forall form m. Monad m => F.Validation form m FormError String Amount
amountFormat = F.hoistFnE_ $ condMap (_ /= mempty) (\str -> note InvalidAmount (Amount <$> fromString str)) InvalidAmount

-- | We should represent Fee as an arbitrary precision number instead of fractional
feeFormat :: forall form m. Monad m => F.Validation form m FormError String Fee
feeFormat = F.hoistFnE_ $ condMap positiveDec (\str -> note InvalidFee (toFee <$> Decimal.fromString str)) InvalidFee
  where
  positiveDec :: String -> Boolean
  positiveDec str = case Decimal.fromString str of
    Just i
      | (i >= Decimal.fromInt 0) && Decimal.isFinite i -> true
    _ -> false

  toFee :: Decimal -> Fee
  toFee _ = { numerator: 20, denominator: 500 }

cond :: forall a. (a -> Boolean) -> FormError -> a -> Either FormError a
cond f err a = if f a then pure a else Left err

condMap :: forall a b. (a -> Boolean) -> (a -> Either FormError b) -> FormError -> a -> Either FormError b
condMap p f err a = if p a then f a else Left err

toOptional ::
  ∀ form m a b.
  Monoid a =>
  Eq a =>
  Monad m =>
  F.Validation form m FormError a b ->
  F.Validation form m FormError a (Maybe b)
toOptional v =
  F.Validation \form val -> case val == mempty of
    true -> pure (pure Nothing)
    _ -> (map <<< map) Just (F.runValidation v form val)

class ToText item where
  toText :: item -> String

instance toTextString :: ToText String where
  toText = identity

instance toTextCoin :: ToText Coin where
  toText (Coin { tokenName }) = show tokenName

instance toTextTokenName :: ToText TokenName where
  toText = show

formErrorToString :: forall e o. ToText e => F.FormFieldResult e o -> Maybe String
formErrorToString = map toText <<< preview F._Error

resultToHelp :: forall t e. ToText e => String -> F.FormFieldResult e t -> Either String String
resultToHelp str = case _ of
  F.NotValidated -> Right str
  F.Validating -> Right "validating..."
  other -> maybe (Right str) Left $ formErrorToString other
