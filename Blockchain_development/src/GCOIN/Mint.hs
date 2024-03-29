{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}
​
module GCOIN.Mint where
​
import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise
import Control.Monad hiding (fmap)
import Data.Aeson (FromJSON, ToJSON)
import Data.ByteString.Lazy qualified as LB
import Data.ByteString.Short qualified as SBS
import Data.Monoid (Last (..), (<>))
import Data.Text (Text)
import Data.Void (Void)
import GCOIN.HandlerContract
import GHC.Generics (Generic)
import Ledger (CurrencySymbol, PaymentPubKey, PaymentPubKeyHash (unPaymentPubKeyHash), TxId, TxOutRef (..), getCardanoTxId, pubKeyHashAddress)
import Ledger hiding (mint, singleton)
import Ledger.Ada (Ada)
import Ledger.Ada qualified as Ada
import Ledger.Constraints as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value as Value
import Plutus.Contract as Contract
import Plutus.Script.Utils.V1.Scripts (scriptCurrencySymbol)
import Plutus.Trace.Emulator as Emulator
import Plutus.V1.Ledger.Api
import Plutus.V1.Ledger.Contexts
import PlutusTx qualified
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Text.Printf (printf)
import Wallet.Emulator.Wallet
import Prelude (IO, Semigroup (..), Show (..), String, div)
import Prelude qualified

-- In the MintRedeemer datatype, MintCoin takes the mint amount, and BurnCoin takes both the burn amount and the user PaymentPubKeyHash.
data MintRedeemer = MintCoin Integer | BurnCoin Integer PaymentPubKeyHash
  deriving (Show)

PlutusTx.unstableMakeIsData ''MintRedeemer
PlutusTx.makeLift ''MintRedeemer


-- lovelaces function used to extract a number of lovelace from a Value type.
{-# INLINEABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

-- naming a token.
{-# INLINEABLE gcoinTokenName #-}
gcoinTokenName :: TokenName
gcoinTokenName = tokenName "GCOIN"

-- mkPolicy function is a parameterized Policy with Three parameters: Handler(import from HandlerContract), TokenName and the Address of the Handler.
{-# INLINEABLE mkPolicy #-}
mkPolicy :: Handler -> TokenName -> Address -> MintRedeemer -> ScriptContext -> Bool
mkPolicy handler tn addr re ctx =
  case re of
    MintCoin mintAmount -> traceIfFalse "insufficient ada value to mint" $ checkMintValue (calculateValue mintAmount mintRate)
    BurnCoin burnAmount user ->
      traceIfFalse "Invalid ada value " (feesPaid (getInput (user)) user burnAmount burnRate)
        && traceIfFalse "The amount is a positive number. " (burnAmount < 0)
        && traceIfFalse "GCOIN in the specified quantity was not burned." (checkBurnValue user burnAmount)
  where
    info :: TxInfo -- Creating an instance to access the pending transactions and related types.
    info = scriptContextTxInfo ctx

    -- function to get the input utxo that have NFT.
    ownInput :: TxOut
    ownInput =
      let ins =
            [ o
              | i <- txInfoInputs info,
                let o = txInInfoResolved i,
                inputHasNFT o
            ]
       in case ins of
            [o] -> o
            _ -> traceError "expected exactly one handler input"

    -- function to check the actual exchange rate and state of handler.
    handlerValue' :: HandlerDatum
    handlerValue' = case (handlerValue (txOutDatumHash ownInput >>= flip findDatum info)) of
      Nothing -> traceError "handler value not found"
      Just x -> x

    -- getInput function takes PaymentPubKeyHash and return input utxo.
    getInput :: PaymentPubKeyHash -> TxOut
    getInput pkh =
      let os =
            [ o
              | i <- txInfoInputs info,
                let o = txInInfoResolved i,
                txOutAddress o == pubKeyHashAddress pkh Nothing
            ]
       in case os of
            [o] -> o
            _ -> traceError "expected exactly one inputs from given PaymentPubKeyHash"
    
    -- inputHasNFT function checks that input contains the handler nft.
    inputHasNFT :: TxOut -> Bool
    inputHasNFT i = assetClassValueOf (txOutValue i) (handlerAsset handler) == 1