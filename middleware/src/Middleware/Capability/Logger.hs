module Middleware.Capability.Logger where

import Data.Text

showText :: Show a => a -> Text
showText = pack . show
