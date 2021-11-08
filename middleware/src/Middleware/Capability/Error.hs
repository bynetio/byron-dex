-- | Definition of throwing and catching effects

module Middleware.Capability.Error
  ( AppError (..)
  , module E
  ) where

import Control.Exception as E hiding (catch, catchJust, fromException, throw, try, tryJust)
import Data.Text         (Text)
import Polysemy.Error    as E
import Servant.Client    (ClientError)

data AppError
  = PabError Text
  | ConfigLoaderError IOException
  | HttpError ClientError
  | OtherError Text
  deriving Show
