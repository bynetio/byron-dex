module Main
    ( main
    ) where

import qualified Spec.LiquidityPoolSpec
import qualified Spec.Model
import qualified Spec.WalletHistorySpec
import           Test.Tasty


main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Uniswap"
    [
      Spec.Model.tests,
      Spec.LiquidityPoolSpec.tests,
      Spec.WalletHistorySpec.tests
    ]
