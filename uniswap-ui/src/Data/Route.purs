module Uniswap.Data.Route where

import Prelude hiding ((/))
import Data.Generic.Rep (class Generic)
import Routing.Duplex (RouteDuplex', root)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Route
  = Home
  | ConnectWallet
  | Pools
  | AddPool
  | Funds
  | Swap

derive instance genericRoute :: Generic Route _

derive instance eqRoute :: Eq Route

derive instance ordRoute :: Ord Route

routeCodec :: RouteDuplex' Route
routeCodec =
  root
    $ sum
        { "Home": noArgs
        , "ConnectWallet": "connect" / noArgs
        , "Pools": "pools" / noArgs
        , "AddPool": "add" / noArgs
        , "Funds": "funds" / noArgs
        , "Swap": "swap" / noArgs
        }
