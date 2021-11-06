-- | Definition of throwing and catching effects

module Middleware.Capability.Error
  ( AppError (..)
  , module E
  ) where

import Control.Exception as E hiding (catch, catchJust, fromException, throw, try, tryJust)
import Data.Text         (Text)
import Polysemy.Error    as E

data AppError
  = PabError Text
  | ConfigLoaderError IOException
  | OtherError Text
  deriving Show
