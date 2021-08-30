{-# LANGUAGE EmptyDataDecls #-}

module Uniswap.Common.Utils
  where

import           Control.Concurrent        (threadDelay)
import           Control.Monad.Freer       (Eff, LastMember, Member, interpret,
                                            sendM, type (~>))
import           Control.Monad.Freer.Error (Error, throwError)
import           Control.Monad.Freer.TH    (makeEffect)
import           Control.Monad.IO.Class    (MonadIO, liftIO)
import           Data.Char                 (toLower)
import           Data.Text                 (Text, pack)
import           Deriving.Aeson            (CustomJSON, FieldLabelModifier,
                                            StringModifier (..), StripPrefix)

-- | Freer Error Helper
fromEither
  :: forall effs e a. Member (Error e) effs
  => Either e a
  -> Eff effs a
fromEither (Left e)  = throwError e
fromEither (Right a) = pure a

showText :: Show a => a -> Text
showText = pack . show

data Time r where
  Sleep :: Integer -> Time ()

makeEffect ''Time

runTime
  :: forall effs m. (MonadIO m, LastMember m effs)
  => Eff (Time ': effs)
  ~> Eff effs
runTime =
  interpret $ \(Sleep us) -> sendM (liftIO $ threadDelay (fromIntegral us))

type PrefixedCamelCase a = CustomJSON '[FieldLabelModifier '[StripPrefix a, ToLower]]

data ToLower
instance StringModifier ToLower where
  getStringModifier ""       = ""
  getStringModifier (c : xs) = toLower c : xs
