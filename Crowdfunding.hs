{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module CrowdFunding where

import           Control.Monad          hiding (fmap)
import           Data.Aeson             (ToJSON, FromJSON)
import           Data.Map               as Map
import           Data.Text              (Text)
import           Data.Void              (Void)
import           GHC.Generics           (Generic)
import           Plutus.Contract
import           PlutusTx               (Data (..))
import qualified PlutusTx
import           PlutusTx.Prelude       hiding (Semigroup(..), unless)
import           Ledger                 hiding (singleton)
import           Ledger.Constraints     (TxConstraints)
import qualified Ledger.Constraints     as Constraints
import qualified Ledger.Typed.Scripts   as Scripts
import           Ledger.Ada             as Ada
import           Playground.Contract    (printJson, printSchemas, ensureKnownCurrencies, stage, ToSchema)
import           Playground.TH          (mkKnownCurrencies, mkSchemaDefinitions)
import           Playground.Types       (KnownCurrency (..))
import           Prelude                (IO, Semigroup (..), Show (..), String)
import           Text.Printf            (printf)

--STATIC DATA
validRedeemer :: PaymentPubKeyHash
validRedeemer = PaymentPubKeyHash "a2c20c77887ace1cd986193e4e75babd8993cfd56995cd5cfce609c2" --which is "Wallet 1"

--THE ONCHAIN CODE
data CrowdFundingDatum = CrowdFundingDatum
                  { beneficiary :: PaymentPubKeyHash
                  , amount :: Integer
                  } deriving Show

PlutusTx.unstableMakeIsData ''CrowdFundingDatum

{-# INLINABLE crowdfundingValidator #-}
crowdfundingValidator :: CrowdFundingDatum -> () -> ScriptContext -> Bool
crowdfundingValidator dat () ctx = traceIfFalse "Beneficiary's signature missing" signedByBeneficiary
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        signedByBeneficiary :: Bool
        signedByBeneficiary = txSignedBy info $ unPaymentPubKeyHash $ beneficiary dat

data CrowdFunding
instance Scripts.ValidatorTypes CrowdFunding where
    type instance DatumType CrowdFunding = CrowdFundingDatum
    type instance RedeemerType CrowdFunding = ()

typedValidator :: Scripts.TypedValidator CrowdFunding
typedValidator = Scripts.mkTypedValidator @CrowdFunding
    $$(PlutusTx.compile [|| crowdfundingValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @CrowdFundingDatum @()

validator :: Validator
validator = Scripts.validatorScript typedValidator

valHash :: Ledger.ValidatorHash
valHash = Scripts.validatorHash typedValidator

scrAddress :: Ledger.Address
scrAddress = scriptAddress validator

--THE OFFCHAIN CODE
data RedeemParams = RedeemParams
                    { rpBeneficiary :: !PaymentPubKeyHash
                    , rpAmount      :: !Integer
                    } deriving (Generic, ToJSON, FromJSON, ToSchema)

type CrowdFundingSchema = Endpoint "Donate" Integer .\/ Endpoint "Redeem" RedeemParams

donate :: AsContractError e => Integer -> Contract w s e ()
donate amount = do
    if amount <= 0
        then logInfo @String $ "You can't send empty or negative value!"
        else do
            let dat = CrowdFundingDatum
                    { beneficiary = validRedeemer
                    , amount = amount
                    }
                tx = Constraints.mustPayToTheScript dat $ Ada.lovelaceValueOf $ amount
            ledgerTx <- submitTxConstraints typedValidator tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            logInfo @String $ printf "You made a donation of %d lovelace" amount

redeem :: forall w s e. AsContractError e => RedeemParams -> Contract w s e ()
redeem rp = do
    pkh   <- ownPaymentPubKeyHash
    utxos <- utxosAt scrAddress
    if pkh /= validRedeemer
        then logInfo @String $ "You aren't a valid redeemer!"
        else do
            if Map.null utxos
                then logInfo @String $ "No available donation!"
                else do
                    let amount      = rpAmount rp
                        beneficiary = address (rpBeneficiary rp) pkh
                        totalUTXOValue = total $ Map.toList utxos
                        remainder = totalUTXOValue - amount
                        dat     = CrowdFundingDatum
                                  { beneficiary = validRedeemer
                                  , amount = remainder
                                  }
                    if totalUTXOValue < amount
                        then do
                            logInfo @String $ printf "Balance is not enough for redeeming %d lovelaces" amount
                            logInfo @String $ printf "Remaining balance on the script is %d lovelaces" totalUTXOValue
                        else do
                            let orefs   = fst <$> Map.toList utxos
                                lookups = Constraints.unspentOutputs utxos <>
                                          Constraints.otherScript validator <>
                                          Constraints.typedValidatorLookups typedValidator
                                tx      = mconcat [Constraints.mustSpendScriptOutput oref (Redeemer $ PlutusTx.toBuiltinData dat) | oref <- orefs] <>
                                          Constraints.mustPayToTheScript dat (Ada.lovelaceValueOf $ remainder) <>
                                          Constraints.mustPayToPubKey beneficiary (Ada.lovelaceValueOf amount)
                            ledgerTx <- submitTxConstraintsWith @CrowdFunding lookups tx
                            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
                            logInfo @String $ "Successfully redeem " <> show amount <> " lovelaces to " <> show beneficiary
                            logInfo @String $ printf "Remaining balance on the script is %d lovelace" remainder
    where
        address :: PaymentPubKeyHash -> PaymentPubKeyHash -> PaymentPubKeyHash
        address receiver own
            | (unPaymentPubKeyHash receiver /= "") = receiver
            | otherwise                            = own

        datum :: ChainIndexTxOut -> Integer
        datum o = case _ciTxOutDatum o of
                  Left  _ -> traceError "Datum does not exist"
                  Right (Datum e) -> case PlutusTx.fromBuiltinData e of
                                                 Nothing -> traceError "Unknown datum type"
                                                 Just d  -> amount d

        total :: [(TxOutRef, ChainIndexTxOut)] -> Integer
        total [] = 0
        total (x:xs) = (datum $ snd $ x) + (total xs)

endpoints :: Contract () CrowdFundingSchema Text ()
endpoints = awaitPromise (donate' `select` redeem') >> endpoints
    where
        donate' = endpoint @"Donate" donate
        redeem' = endpoint @"Redeem" redeem

mkSchemaDefinitions ''CrowdFundingSchema

mkKnownCurrencies []