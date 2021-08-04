module Uniswap.Types where

data PabConfig = MkPabConfig
  { pabUrl  :: String
  , pabPort :: Int
  } deriving Show

data AppContext = MkAppContext
  { pab  :: PabConfig
  , port :: Int
  } deriving Show
