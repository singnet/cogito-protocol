{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Week06.Oracle.Core
( Handler (..)
, HandlerRedeemer (..)
, handlerTokenName
, handlerValue
, handlerAsset
, typedHandlerValidator
, handlerValidator
, handlerAddress
) where

import           Control.Monad        hiding (fmap)
import           Data.Aeson           (FromJSON, ToJSON)
import           Data.Monoid          (Last (..))
import qualified Data.Map             as Map
import           Data.Text            (Text)
import           GHC.Generics         (Generic)
import           Ledger               hiding (singleton)
import           Ledger.Constraints   as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import           Ledger.Ada           as Ada
import           Ledger.Value
import           Playground.Contract  (ToSchema)
import           Plutus.Contract      as Contract
import qualified PlutusTx
import           PlutusTx.Prelude     hiding (Semigroup(..), unless)
import           Plutus.Contracts.Currency as Currency
import           Prelude              (Semigroup (..), Show (..), String)
import qualified Prelude



data Handler = Handler
    { hSymbol   :: !CurrencySymbol
    , hOperator :: !PubKeyHash
    } deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq, Prelude.Ord)   

data HandlerRedeemer = Update | Use
    deriving Show    

PlutusTx.unstableMakeIsData ''HandlerRedeemer  

-- creating a tokenname
{-# INLINABLE handlerTokenName #-}
handlerTokenName :: TokenName
handlerTokenName = TokenName emptyByteString

{- A function that creates an AssetClass by taking the CurrencySymbol designated for the NFT from the Handler Data Type, and the handlerTokenName for the NFT that is initially set to emptyByteString.-}
{-# INLINABLE handlerAsset #-} -- Making our function INLINABLE so that the data structure is suitable to compile to Plutus. 
handlerAsset :: Handler -> AssetClass
handlerAsset handler = AssetClass (hSymbol handler, handlerTokenName)

-- function that extracts the datum from utxo
{-# INLINABLE handlerValue #-}
handlerValue :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe Bool
handlerValue o f = do
    dh      <- txOutDatumHash o
    Datum d <- f dh
    PlutusTx.fromData d


{-# INLINABLE mkHandlerValidator #-}
mkHandlerValidator :: Handler -> Bool -> HandlerRedeemer -> ScriptContext -> Bool
mkHandlerValidator handler x r ctx =
    traceIfFalse "token missing from input"  inputHasToken  && 
    traceIfFalse "token missing from output" outputHasToken && 
    case r of -- HandlerRedemmer's value deciding whether to update/change the previously set STATE (valid for the superuser only) or to use the handler contract.
        Update -> traceIfFalse "operator signature missing" (txSignedBy info $ hOperator handler) && -- checking if the contract is singed by the superuser whose PubKeyHash is stored in hOperator.
                  traceIfFalse "invalid output datum"       validOutputDatum -- Checks if there is a STATE to change.
        Use    -> traceIfFalse "handler value changed"       (outputDatum == Just x) -- checking to see if the datum value from the previous UTXO matches with the one we get from our off-chain code.
​
    where
        info :: TxInfo -- Creating an instance to access the pending transactions and related types.
        info = scriptContextTxInfo ctx 

        -- function to get the input utxo from the ScriptContext
        ownInput :: TxOut
        ownInput = case findOwnInput ctx of
            Nothing -> traceError "handler input missing"
            Just i  -> txInInfoResolved i

        -- function to check if the input utxo has the NFT
        inputHasNFT :: Bool
        inputHasNFT = assetClassValueOf (txOutValue ownInput) (handlerAsset handler) == 1
​
        --A Function that checks if we have exactly one output UTXO and returns that UTXO to us.
        ownOutput :: TxOut
        ownOutput = case getContinuingOutputs ctx of
            [o] -> o
            _   -> traceError "expected exactly one handler output"

        --A Function that checks if the NFT is present in output UTXO
        outputHasNFT :: Bool
        outputHasNFT = assetClassValueOf (txOutValue ownOutput) (handlerAsset handler) == 1

        -- function to check that a valid datum is present in the utxo
        validOutputDatum :: Bool
        validOutputDatum = isJust outputDatum


--data Handling Provide instance of the ValidatorTypes class to record Datum and Redeemer type 
data Handling
instance Scripts.ValidatorTypes Handling where
    type instance DatumType Handling = Bool
    type instance RedeemerType Handling = HandlerRedeemer

--function that Compile mkHandlerValidator to Plutus Core 
typedHandlerValidator :: Handler -> Scripts.TypedValidator Handling
typedHandlerValidator handler = Scripts.mkTypedValidator @Handling
    ($$(PlutusTx.compile [|| mkHandlerValidator ||]) `PlutusTx.applyCode` PlutusTx.liftCode handler)
    $$(PlutusTx.compile [|| wrap ||])
 where
    wrap = Scripts.wrapValidator @Bool @HandlerRedeemer --add a wrap function to be able to translate the strong types from the low level version. 

​--function that creates a validator
handlerValidator :: Handler -> Validator
handlerValidator = Scripts.validatorScript . typedHandlerValidator
​
​--function that generate the script address
handlerAddress :: Handler -> Ledger.Address
handlerAddress = scriptAddress . handlerValidator


