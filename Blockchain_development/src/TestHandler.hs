{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}

module GCOIN.TestHandler where

import           Control.Monad              hiding (fmap)
import           Control.Monad.Freer.Extras ()
import           Data.Default               (Default (..))
import qualified Data.Map                   as Map
import           Data.Monoid                (Last (..))
import           Data.Text                  (Text)
import           Ledger
import           Ledger.Value ()
import           Ledger.Ada                 as Ada
import           Plutus.Contract            as Contract hiding (when)
import           Plutus.Trace.Emulator      as Emulator
import           PlutusTx.Prelude           hiding (Semigroup(..), unless)
import           Prelude                    (Semigroup(..), IO, show)
import           Wallet.Emulator.Wallet
import           GCOIN.HandlerContract
import qualified Control.Monad.Freer.Extras as Extras


test :: IO ()
test = runEmulatorTraceIO' def emCfg myTrace
  where
    emCfg :: EmulatorConfig
    emCfg = EmulatorConfig (Left $ Map.fromList $ [(knownWallet i, v) | i <- [1 .. 4]]) def
    v :: Value
    v = Ada.lovelaceValueOf  10000000
    
checkHandler :: Handler -> Contract () w Text ()
checkHandler handler = do
    m <- findHandlerOutput handler
    case m of
        Nothing        -> return ()
        Just (_, _, x) -> Contract.logInfo $ "Handler value: " ++ show x
    Contract.waitNSlots 1 >> checkHandler handler