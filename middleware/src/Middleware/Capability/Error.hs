-- | Definition of throwing and catching effects

module Middleware.Capability.Error
  ( AppError (..)
  , module E
  ) where

import Control.Exception         as E
import Control.Monad.Freer.Error as E
import Data.Text                 (Text)

data AppError
  = PabError Text
  | ConfigLoaderError IOException
  | OtherError Text
  deriving Show
