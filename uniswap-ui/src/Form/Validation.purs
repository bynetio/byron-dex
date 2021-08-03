module Uniswap.Form.Validation where

import Prelude
import Data.Either (Either(..), note)
import Data.Int (fromString)
import Data.Maybe (Maybe(..), isJust)
import Data.String as String
import Data.String.Pattern (Pattern(..))
import Formless as F
import Uniswap.Data.Amount (Amount(..))
import Uniswap.Data.Coin (CurrencySymbol(..), TokenName(..))
import Uniswap.Data.Fee (Fee)

data FormError
  = Required
  | TooShort
  | TooLong
  | InvalidTokenName
  | InvalidCurrencySymbol
  | InvalidAmount
  | InvalidFee

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

minLength :: forall form m. Monad m => Int -> F.Validation form m FormError String String
minLength n = F.hoistFnE_ $ cond (\str -> String.length str > n) TooShort

maxLength :: forall form m. Monad m => Int -> F.Validation form m FormError String String
maxLength n = F.hoistFnE_ $ cond (\str -> String.length str <= n) TooLong

tokenNameFormat :: forall form m. Monad m => F.Validation form m FormError String TokenName
tokenNameFormat = F.hoistFnE_ $ map TokenName <<< cond (\str -> String.length str > 0) InvalidTokenName

currencySymbolFormat :: forall form m. Monad m => F.Validation form m FormError String CurrencySymbol
currencySymbolFormat = F.hoistFnE_ $ map CurrencySymbol <<< cond (\str -> String.length str > 0) InvalidCurrencySymbol

amountFormat :: forall form m. Monad m => F.Validation form m FormError String Amount
amountFormat = F.hoistFnE_ $ map Amount <<< condMap (_ /= mempty) (\str -> note InvalidAmount (fromString str)) InvalidAmount

feeFormat :: forall form m. Monad m => F.Validation form m FormError String Fee
feeFormat = F.hoistFnE_ $ map (\str -> toFee $ String.split (Pattern ".") str) <<< cond (\i -> isJust (String.indexOf (Pattern ".") i)) InvalidFee
  where
  toFee :: Array String -> Fee
  toFee _ = { numerator: 2, denominator: 200 }

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
