{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Faucet.Internal (faucet, AppFaucetEnv(..), AppFaucet, runApp, mintPolicyId) where

import           Cardano.Api
import           Cardano.Api.Shelley                               (ProtocolParameters)
import           Cardano.CLI.Shelley.Key                           (deserialiseInputAnyOf)
import           Colog
import           Config
import           Control.Arrow                                     (Arrow (second))
import           Control.Concurrent                                (MVar,
                                                                    modifyMVar)
import           Control.Exception                                 (throw)
import           Control.Monad.Reader
import qualified Data.ByteString                                   as BS
import qualified Data.ByteString.Char8                             as C
import qualified Data.Map                                          as Map
import qualified Data.Set                                          as Set
import qualified Data.Text                                         as T
import           Faucet.Data
import           GHC.Exception.Type                                (Exception)
import           Ouroboros.Network.Protocol.LocalTxSubmission.Type (SubmitResult (SubmitFail, SubmitSuccess))
import           Prelude                                           hiding (log)

data AppFaucetEnv m = AppFaucetEnv {
  nodeConfig   :: NodeConfig,
  faucetConfig :: FaucetConfig,
  utxRefs      :: MVar (Set.Set TxIn),
  logAction    :: LogAction m Message
}

instance HasLog (AppFaucetEnv m) Message m where
    getLogAction :: AppFaucetEnv m -> LogAction m Message
    getLogAction = logAction

    setLogAction :: LogAction m Message -> AppFaucetEnv m -> AppFaucetEnv m
    setLogAction newLogAction env = env { logAction = newLogAction }

newtype AppFaucet a = AppFaucet {
  unApp :: ReaderT (AppFaucetEnv AppFaucet) IO a
} deriving newtype (Functor, Applicative, Monad, MonadIO, MonadReader (AppFaucetEnv AppFaucet))

runApp :: AppFaucet a -> AppFaucetEnv AppFaucet -> IO a
runApp app = runReaderT (unApp app)

faucet :: WithLog env Message AppFaucet => AddressParam -> TokenName -> AppFaucet TxId
faucet (AddressParam receiver) tn = do
    ctx <- ask
    utxRefs' <- asks utxRefs
    pp <- queryProtocolParams
    sender <- walletAddress
    receiver' <- liftIO $ parseAddress (T.pack receiver)
    lovelace <- walletLovelace
    mTokenQ <- mintedTokenQuantity
    sKey <- signingKey
    utxos <- Map.toList . unUTxO <$> queryUtxo sender
    liftIO $ modifyMVar utxRefs' $ \refs -> do
        let utxos' = flip filter utxos $ \(txIn, _) -> not $ Set.member txIn refs
        utxo@(txIn, _) <- liftMaybeIO (mayHead utxos') NoUtxoToConsumeError
        txId <- runApp (faucet' pp utxo sKey sender receiver' lovelace (tn, mTokenQ)) ctx
        let refs' = Set.intersection (Set.insert txIn refs) (Set.fromList $ fst <$> utxos)
        return (refs', txId)

mayHead :: [a] -> Maybe a
mayHead (a : _) = Just a
mayHead _       = Nothing

signingKey :: AppFaucet (SigningKey PaymentKey)
signingKey = do
    cfg <- asks faucetConfig
    let keyPath = walletConfigSkey $ faucetConfigWallet cfg
    liftIO $ BS.readFile keyPath >>= skeyFrom
    where
      skeyFrom :: BS.ByteString -> IO (SigningKey PaymentKey)
      skeyFrom content = case deserialiseInputAnyOf bech32Types textEnvTypes content of
        Right key -> return key
        Left err  -> throw $ SkeyDeserialiseError $ show err
        where
          bech32Types = [FromSomeType (AsSigningKey AsPaymentKey) id]
          textEnvTypes = [FromSomeType (AsSigningKey AsPaymentKey) id]

mintedTokenQuantity :: AppFaucet Integer
mintedTokenQuantity = asks $ mintConfigQuantity . faucetConfigMint . faucetConfig

walletConfig :: AppFaucet WalletConfig
walletConfig = asks $ faucetConfigWallet . faucetConfig

walletLovelace :: AppFaucet Lovelace
walletLovelace = quantityToLovelace . Quantity . walletConfigLovelace <$> walletConfig

parseAddress :: T.Text -> IO AddressAny
parseAddress addr = liftMaybeIO maybeAddress (AddressDecodingError $ T.unpack addr)
  where
    maybeAddress :: Maybe AddressAny
    maybeAddress = toAddressAny <$> deserialiseAddress AsShelleyAddress addr

walletAddress :: AppFaucet AddressAny
walletAddress = do
    cfg <- walletConfig
    let addr = walletConfigAddress cfg
    liftIO $ parseAddress addr

mintScriptWitness :: AppFaucet (ScriptWitness witctx AlonzoEra)
mintScriptWitness = do
  sKey <- signingKey
  let pkh = verificationKeyHash$ getVerificationKey sKey
  return $ requireSigWitness pkh

mintPolicyId :: AppFaucet PolicyId
mintPolicyId = do
  mintScriptWitness' <- mintScriptWitness
  case scriptWitnessScript mintScriptWitness' of
    ScriptInEra _ script -> return $ scriptPolicyId script

faucet' :: WithLog env Message AppFaucet => ProtocolParameters -> (TxIn, TxOut CtxUTxO AlonzoEra) -> SigningKey PaymentKey -> AddressAny -> AddressAny -> Lovelace -> (TokenName, Integer) -> AppFaucet TxId
faucet' pParams (txIn, txOut) sKey sender receiver lovelace (tn, quantity) = do
  policyId <- mintPolicyId
  scriptWitness <- mintScriptWitness
  (txId, tx) <- second (signTx sKey) <$> txBody policyId scriptWitness
  submitTx tx
  log I $ T.pack ("Transaction " <> show txId <> " succesfully submited")
  return txId
  where
    mintValue policyId = valueFromList [ (AssetId policyId assetName, Quantity quantity) ]
      where
        assetName = AssetName $ C.pack (unTokenName tn)

    txMintedValue policyId scriptWitness = TxMintValue MultiAssetInAlonzoEra (mintValue policyId) $ BuildTxWith $ Map.fromList [(policyId, scriptWitness)]

    senderZeroTxOut = txOutFromLovelace sender mempty

    balance :: TxBodyContent BuildTx AlonzoEra -> Either TxBodyError (TxBodyContent BuildTx AlonzoEra)
    balance content = let body = makeTransactionBody content
                          fee = evaluateTransactionFee pParams <$> body <*> pure 1 <*> pure 0
                          dfee = liftM2 (<>) fee fee
                          diff = liftM2 (<>) dfee $ pure  lovelace
                          change = liftM2 (<>) (negateValue . lovelaceToValue <$> diff) (pure $ txOutValue txOut)
                        in liftM3 update (pure content) dfee change
      where
        txOutValue :: TxOut CtxUTxO AlonzoEra -> Value
        txOutValue (TxOut _ value _) = txOutValueToValue value
        update :: TxBodyContent BuildTx AlonzoEra -> Lovelace -> Value -> TxBodyContent BuildTx AlonzoEra
        update c fee out = c {
          txFee = TxFeeExplicit TxFeesExplicitInAlonzoEra fee,
          txOuts = txOutFromValue sender out : filter (/= senderZeroTxOut) (txOuts c)
        }

    txBody policyId scriptWitness = liftIO $ liftEitherIO $ flip fmap (balance txContent >>= makeTransactionBody) $ \b -> (getTxId b, b)
      where
        txContent  = mkEmptyTxBodyContent {
          txIns = [(txIn, BuildTxWith $ KeyWitness KeyWitnessForSpending)],
          txInsCollateral = TxInsCollateral CollateralInAlonzoEra [],
          txOuts = [txOutFromValue receiver $ lovelaceToValue lovelace <> mintValue policyId, senderZeroTxOut],
          txMintValue = txMintedValue policyId scriptWitness
        }

requireSigWitness :: Hash PaymentKey -> ScriptWitness witctx AlonzoEra
requireSigWitness pkh = SimpleScriptWitness SimpleScriptV1InAlonzo SimpleScriptV1 script
  where
    script :: SimpleScript SimpleScriptV1
    script = Cardano.Api.RequireSignature pkh

txOutFromLovelace :: AddressAny -> Lovelace -> TxOut CtxTx AlonzoEra
txOutFromLovelace addr l = txOutFromValue addr $ lovelaceToValue l

txOutFromValue :: AddressAny -> Value -> TxOut CtxTx AlonzoEra
txOutFromValue addr v = TxOut addressInEra (TxOutValue MultiAssetInAlonzoEra v) TxOutDatumNone
  where
    addressInEra = case anyAddressInEra AlonzoEra addr of
      Just addr' -> addr'
      _          -> throw $ SomeError "Not address in alozno era"

mkEmptyTxBodyContent :: TxBodyContent BuildTx AlonzoEra
mkEmptyTxBodyContent = TxBodyContent {
        txIns = [],
        txInsCollateral = TxInsCollateral CollateralInAlonzoEra [],
        txOuts = [],
        txFee = TxFeeExplicit TxFeesExplicitInAlonzoEra mempty,
        txValidityRange = (TxValidityNoLowerBound, TxValidityNoUpperBound ValidityNoUpperBoundInAlonzoEra),
        txMetadata = TxMetadataNone ,
        txAuxScripts = TxAuxScriptsNone,
        txExtraKeyWits = TxExtraKeyWitnessesNone,
        txProtocolParams = BuildTxWith Nothing,
        txWithdrawals = TxWithdrawalsNone,
        txCertificates = TxCertificatesNone,
        txUpdateProposal = TxUpdateProposalNone,
        txMintValue = TxMintNone,
        txScriptValidity = TxScriptValidityNone
      }

signTx :: SigningKey PaymentKey -> TxBody AlonzoEra -> Tx AlonzoEra
signTx sKey txBody = makeSignedTransaction [ makeShelleyKeyWitness txBody (WitnessPaymentKey sKey) ] txBody

nodeConn :: AppFaucet (LocalNodeConnectInfo CardanoMode)
nodeConn = do
    cfg <- asks nodeConfig
    return $ connInfo (net cfg) (socketConfigPath $ nodeConfigSocket cfg)
    where
        net :: NodeConfig -> NetworkId
        net (NodeConfig (Just netMagic) "Testnet"  _) = Testnet (NetworkMagic $ read netMagic)
        net cfg = throw $ UknownNetworkIdError $ show cfg
        connInfo :: NetworkId -> FilePath -> LocalNodeConnectInfo CardanoMode
        connInfo = LocalNodeConnectInfo (CardanoModeParams (EpochSlots 432000))

submitTx :: Tx AlonzoEra -> AppFaucet ()
submitTx tx = do
  conn <- nodeConn
  res <- liftIO $ submitTxToNodeLocal conn $ TxInMode tx AlonzoEraInCardanoMode
  case res of
    SubmitSuccess     -> return ()
    SubmitFail reason -> throw $ SubmitTxError $ show reason

liftEitherIO :: Show a => Either a b -> IO b
liftEitherIO (Left err)    = throw $ SomeError $ show err
liftEitherIO (Right value) = return value

liftMaybeIO :: Exception e =>  Maybe a -> e -> IO a
liftMaybeIO (Just a) _ = return a
liftMaybeIO _ e        = throw e

queryProtocolParams :: AppFaucet ProtocolParameters
queryProtocolParams = do
    conn <- nodeConn
    tip <- liftIO $ getLocalChainTip conn
    paramsOrErr <- liftIO $ queryNodeLocalState conn (Just $ chainTipToChainPoint tip) query
    case paramsOrErr of
        Right (Right p) -> return p
        err             -> throw $ QueryProtocolParamsError $ show err
    where
        query = QueryInEra AlonzoEraInCardanoMode $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo QueryProtocolParameters

queryUtxo :: AddressAny -> AppFaucet (UTxO AlonzoEra)
queryUtxo addr = do
    conn <- nodeConn
    tip <- liftIO $ getLocalChainTip conn
    utxosOrErr <- liftIO $ queryNodeLocalState conn (Just $ chainTipToChainPoint tip) query
    case utxosOrErr of
        Right (Right utxos) -> return utxos
        err                 -> throw $ QueryUtxoError $ show err
    where
        query = QueryInEra AlonzoEraInCardanoMode $ QueryInShelleyBasedEra ShelleyBasedEraAlonzo (QueryUTxO queryByAddress)
        queryByAddress :: QueryUTxOFilter
        queryByAddress = QueryUTxOByAddress $ Set.singleton addr
