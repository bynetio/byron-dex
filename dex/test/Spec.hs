-- {-# LANGUAGE DataKinds        #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Test.Tasty

import qualified Spec.UseCases
--import Plutus.V1.Ledger.Scripts (fromCompiledCode, Script)
--import PlutusTx.TH (compile)
--import Dex.OnChain
--import Test.Tasty.Plutus.Size (fitsOnChain)

-- script :: Script
-- script = fromCompiledCode $$(compile [|| mkDexValidator ||])

-- wiksa :: TestTree
-- wiksa = testGroup "On-chain size" $ [
--   fitsOnChain "dex validator" script
--   ]

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "dex" [ Spec.UseCases.tests ]
