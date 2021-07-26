module Uniswap.API
  where

import Control.Monad.Freer          (Eff, LastMember, Members)
import Data.Text                    (Text)
import Servant.Server               (Handler, HasServer (ServerT))
import Uniswap.Common.AppError      (AppError)
import Uniswap.Common.Logger        (Logger)
import Uniswap.Common.NextOpID      (NextOpID)
import Uniswap.Common.ServantClient (ServantClient)
import Uniswap.Common.Utils         (Time)
import Uniswap.LiquidityPool.API    (LiquidityPoolAPI, liquidityPoolAPI)
import Uniswap.PAB                  (UniswapPab)
import UniswapJsonApi.Types         (AppContext (..), AppM (..), UniswapDefinition, UniswapStatusResponse)


type API =
  LiquidityPoolAPI

api
  :: (LastMember Handler effs, Members
  '[ UniswapPab
   , ServantClient
   , NextOpID
   , Logger
   , AppError
   , Time
   , Handler
   ] effs)
  => ServerT API (Eff effs)
api = liquidityPoolAPI


