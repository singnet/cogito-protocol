
-- Getting currecy symbol of the token
tokenCurSymbol :: TxOutRef -> TokenName -> CurrencySymbol
tokenCurSymbol oref tn = scriptCurrencySymbol $ tokenPolicy oref tn

-- Generating specific script for the input parameters of TokenName and reference to TxOut
plutusScript :: TxOutRef -> TokenName -> Script
plutusScript oref tn = unMintingPolicyScript $ tokenPolicy oref tn

-- Generating a validator
validator :: TxOutRef -> TokenName -> Validator
validator oref tn = Validator $ unMintingPolicyScript $ tokenPolicy oref tn

-- the following three funcitons are used to serialize the script
scriptAsCbor :: TxOutRef -> TokenName -> LB.ByteString
scriptAsCbor oref tn = serialise $ validator oref tn

apiExamplePlutusMintingScript :: TxOutRef -> TokenName -> PlutusScript PlutusScriptV1
apiExamplePlutusMintingScript oref tn = PlutusScriptSerialised . SBS.toShort $ LB.toStrict (scriptAsCbor oref tn)

mintingScriptShortBs :: TxOutRef -> TokenName -> SBS.ShortByteString
mintingScriptShortBs oref tn = SBS.toShort . LB.toStrict $ scriptAsCbor oref tn
