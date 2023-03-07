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