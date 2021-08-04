{-# LANGUAGE RecordWildCards #-}
module Uniswap.LiquidityPool.API
  ( LiquidityPoolAPI
  , liquidityPoolAPI
  )
  where

import           Control.Monad.Freer         (Eff, LastMember, Members)
import           Data.Aeson                  (Value)
import           Data.Text                   (Text)
import           Servant
import           Servant.Server              (Handler, HasServer (ServerT))
import           Uniswap.LiquidityPool.Types
import qualified Uniswap.PAB                 as PAB
import           Uniswap.PAB.Types

type LiquidityPoolEffs = PAB.UniswapPab ': PAB.UniswapPabEffs Handler

type LiquidityPoolAPI =
       Capture "id" Text :> "create" :> ReqBody '[JSON] CreatePoolForm :> Post '[JSON] Value
  :<|> Capture "id" Text :> "swap" :> ReqBody '[JSON] SwapForm :> Post '[JSON] Value
  :<|> Capture "id" Text :> "swap-preview" :> ReqBody '[JSON] SwapPreviewForm :> Post '[JSON] Value
  :<|> Capture "id" Text :> "indirect-swap" :> ReqBody '[JSON] IndirectSwapForm :> Post '[JSON] Value
  :<|> Capture "id" Text :> "indirect-swap-preview" :> ReqBody '[JSON] ISwapPreviewForm :> Post '[JSON] Value
  :<|> Capture "id" Text :> "close" :> ReqBody '[JSON] CloseForm :> Post '[JSON] Value
  :<|> Capture "id" Text :> "remove" :> ReqBody '[JSON] RemoveForm :> Post '[JSON] Value
  :<|> Capture "id" Text :> "add" :> ReqBody '[JSON] AddForm :> Post '[JSON] Value
  :<|> Capture "id" Text :> "pools" :> Get '[JSON] Value
-- not bounded to Liquidity Pool
  :<|> Capture "id" Text :> "funds" :> Get '[JSON] Value
  :<|> Capture "id" Text :> "stop" :> Get '[JSON] Value
  :<|> Capture "id" Text :> "status" :> Get '[JSON] UniswapStatusResponse


liquidityPoolAPI
  :: (LastMember Handler effs, Members LiquidityPoolEffs effs)
  => ServerT LiquidityPoolAPI (Eff effs)
liquidityPoolAPI =
       create
  :<|> swap
  :<|> swapPreview
  :<|> indirectSwap
  :<|> indirectSwapPreview
  :<|> close
  :<|> remove
  :<|> add
  :<|> pools
  :<|> PAB.funds
  :<|> PAB.stop
  :<|> PAB.status


create
  :: (LastMember Handler effs, Members LiquidityPoolEffs effs)
  => Text
  -> CreatePoolForm
  -> Eff effs Value
create uid CreatePoolForm{..} = do
  let params = CreateParams{..}
  PAB.create uid params

swap
  :: (LastMember Handler effs, Members LiquidityPoolEffs effs)
  => Text
  -> SwapForm
  -> Eff effs Value
swap uid SwapForm{..} = do
  let params = SwapParams{..}
  PAB.swap uid params

swapPreview
  :: (LastMember Handler effs, Members LiquidityPoolEffs effs)
  => Text
  -> SwapPreviewForm
  -> Eff effs Value
swapPreview uid SwapPreviewForm{..} = do
  let params = SwapPreviewParams{..}
  PAB.swapPreview uid params

indirectSwap
  :: (LastMember Handler effs, Members LiquidityPoolEffs effs)
  => Text
  -> IndirectSwapForm
  -> Eff effs Value
indirectSwap uid IndirectSwapForm{..} = do
  let params = IndirectSwapParams{..}
  PAB.indirectSwap uid params

indirectSwapPreview
  :: (LastMember Handler effs, Members LiquidityPoolEffs effs)
  => Text
  -> ISwapPreviewForm
  -> Eff effs Value
indirectSwapPreview uid ISwapPreviewForm{..} = do
  let params = ISwapPreviewParams{..}
  PAB.indirectSwapPreview uid params

add
  :: (LastMember Handler effs, Members LiquidityPoolEffs effs)
  => Text
  -> AddForm
  -> Eff effs Value
add uid AddForm{..} = do
  let params = AddParams{..}
  PAB.add uid params

remove
  :: (LastMember Handler effs, Members LiquidityPoolEffs effs)
  => Text
  -> RemoveForm
  -> Eff effs Value
remove uid RemoveForm{..} = do
  let params = RemoveParams{..}
  PAB.remove uid params

close
  :: (LastMember Handler effs, Members LiquidityPoolEffs effs)
  => Text
  -> CloseForm
  -> Eff effs Value
close uid CloseForm{..} = do
  let params = CloseParams{..}
  PAB.close uid params

pools
  :: (LastMember Handler effs, Members LiquidityPoolEffs effs)
  => Text
  -> Eff effs Value
pools uid =
  PAB.pools uid


