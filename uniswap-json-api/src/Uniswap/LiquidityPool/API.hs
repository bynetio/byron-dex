{-# LANGUAGE RecordWildCards #-}
module Uniswap.LiquidityPool.API
  ( LiquidityPoolAPI
  , liquidityPoolAPI
  )
  where

import           Control.Monad.Freer         (Eff, LastMember, Members)
import           Data.Text                   (Text)
import           Servant
import           Servant.Server              (Handler, HasServer (ServerT))
import           Uniswap.LiquidityPool.Types
import qualified Uniswap.PAB                 as PAB
import           Uniswap.PAB.Types
import           UniswapJsonApi.Types        (AppContext (..), AppM (..), UniswapDefinition,
                                              UniswapStatusResponse)

type LiquidityPoolEffs = PAB.UniswapPab ': PAB.UniswapPabEffs Handler

type LiquidityPoolAPI =
       Capture "id" Text :> "create" :> ReqBody '[JSON] CreatePoolForm :> Post '[JSON] UniswapDefinition
  :<|> Capture "id" Text :> "swap" :> ReqBody '[JSON] SwapForm :> Post '[JSON] UniswapDefinition
  :<|> Capture "id" Text :> "swap-preview" :> ReqBody '[JSON] SwapPreviewForm :> Post '[JSON] UniswapDefinition
  :<|> Capture "id" Text :> "indirect-swap" :> ReqBody '[JSON] IndirectSwapForm :> Post '[JSON] UniswapDefinition
  :<|> Capture "id" Text :> "indirect-swap-preview" :> ReqBody '[JSON] ISwapPreviewForm :> Post '[JSON] UniswapDefinition
  :<|> Capture "id" Text :> "close" :> ReqBody '[JSON] CloseForm :> Post '[JSON] UniswapDefinition
  :<|> Capture "id" Text :> "remove" :> ReqBody '[JSON] RemoveForm :> Post '[JSON] UniswapDefinition
  :<|> Capture "id" Text :> "add" :> ReqBody '[JSON] AddForm :> Post '[JSON] UniswapDefinition
  :<|> Capture "id" Text :> "pools" :> Get '[JSON] UniswapDefinition
-- not bounded to Liquidity Pool
  :<|> Capture "id" Text :> "funds" :> Get '[JSON] UniswapDefinition
  :<|> Capture "id" Text :> "stop" :> Get '[JSON] UniswapDefinition
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
  -> Eff effs UniswapDefinition
create uid CreatePoolForm{..} = do
  let params = CreateParams{..}
  PAB.create uid params

swap
  :: (LastMember Handler effs, Members LiquidityPoolEffs effs)
  => Text
  -> SwapForm
  -> Eff effs UniswapDefinition
swap uid SwapForm{..} = do
  let params = SwapParams{..}
  PAB.swap uid params

swapPreview
  :: (LastMember Handler effs, Members LiquidityPoolEffs effs)
  => Text
  -> SwapPreviewForm
  -> Eff effs UniswapDefinition
swapPreview uid SwapPreviewForm{..} = do
  let params = SwapPreviewParams{..}
  PAB.swapPreview uid params

indirectSwap
  :: (LastMember Handler effs, Members LiquidityPoolEffs effs)
  => Text
  -> IndirectSwapForm
  -> Eff effs UniswapDefinition
indirectSwap uid IndirectSwapForm{..} = do
  let params = IndirectSwapParams{..}
  PAB.indirectSwap uid params

indirectSwapPreview
  :: (LastMember Handler effs, Members LiquidityPoolEffs effs)
  => Text
  -> ISwapPreviewForm
  -> Eff effs UniswapDefinition
indirectSwapPreview uid ISwapPreviewForm{..} = do
  let params = ISwapPreviewParams{..}
  PAB.indirectSwapPreview uid params

add
  :: (LastMember Handler effs, Members LiquidityPoolEffs effs)
  => Text
  -> AddForm
  -> Eff effs UniswapDefinition
add uid AddForm{..} = do
  let params = AddParams{..}
  PAB.add uid params

remove
  :: (LastMember Handler effs, Members LiquidityPoolEffs effs)
  => Text
  -> RemoveForm
  -> Eff effs UniswapDefinition
remove uid RemoveForm{..} = do
  let params = RemoveParams{..}
  PAB.remove uid params

close
  :: (LastMember Handler effs, Members LiquidityPoolEffs effs)
  => Text
  -> CloseForm
  -> Eff effs UniswapDefinition
close uid CloseForm{..} = do
  let params = CloseParams{..}
  PAB.close uid params

pools
  :: (LastMember Handler effs, Members LiquidityPoolEffs effs)
  => Text
  -> Eff effs UniswapDefinition
pools uid =
  PAB.pools uid


