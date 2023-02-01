/*

GCOIN mint
Contract used for minting, as well as storing collateral

Deployment
GCOIN mint contracts are deployed and permissioned from the governance system, meaning that a new type of collateral may be added at any time after a governance proposal succeeds and is executed.

Description
A GCOIN mint is the smart contract that mints GCOIN to users for placing collateral. GCOIN mint can accept any kind of cryptocurrency, but low volatility pools are preferred at inception since they do not change the collateral ratio erratically. 

The pool has a ceiling (the maximum allowable % of total collateral) for each asset and a price feed for the asset. The initial collateral at genesis will be USDC (USD Coin) and USDT (Tether) due to their large market capitalization, stability, and availability on various chains.

The pools operate through permissioned calls to interact with cogito.stablizer and cogito.reserve, such that the collaterals are split into liquid and illiquid part, where liquid will be used for trading (i.e. stabilizer) and illiquid will be used for investment.

*/

/*

The smart contract should have following variables: 
address collateral_address: Address of the collateral token.
address[] pool_owners: List of the pool owners.
address oracle_address: Address of the oracle contract.
address gcoin_contract_address: Address of the GCOIN contract.
address cgv_contract_address: Address of the CGV contract.
address timelock_address: Address of the timelock contract.
address cogito_stabilizer: Address of the liquid reserve
address cogito_treasury: Address of the illiquid reserve
int[] pool_ceiling: Maximum amount of collateral the pool can take.
MINT_PAUSER: AccessControl role for the mint pauser.
REDEEM_PAUSER: AccessControl role for the redeem pauser.
CAR: capital adequacy ratio. Refer to whitepaper for calculation.
*/


/*

The smart contract should do the following: 

*/

/* Getters */

// mintPaused(): Whether or not minting is paused.

// redeemPaused(): Whether or not redeem is paused.

// availableCollateral(): Return the total amount of available collateral.

// collatralDollarBalance(): Return the pool's total balance of the collateral token, in USD. This should be the sum of amountTrade() and amountInvestment() in market value in USD. 

// availableExcessCollateral(): Return the pool's excess balance of the collateral token (over that required by the collateral ratio), in USD.

// getCollateralPrice(): Return the price of the pool's collateral in USD.

// availableTrade(): Returns the maximum available amount for trading. This should be calculated by the total collateral value and capital adequacy ratio. 

// balanceTrade(): Returns the balance available for trading, in USD.

// availableInvestment(): Returns the maximum amount in dollar for investment.

// balanceInvestment(): Returns the balance of investment portion, in USD.


/* Setters */

// mintGCOIN(): Mint GCOIN from collateral, netting the transaction fee.

// redeemGCOIN(): call sell() function in cogito_stabilizer. Valid only when the redeemPaused = false.

// buyBackCGV(): Function can be called by an CGV holder to have the protocol buy back CGV with excess collateral value from a desired collateral pool. This can happen if the collateral ratio > target CAR.  

// updateCAR(): Return the CAR ratio of the GCOIN mint. Note: We need to constantly poll this value and raise an alert when the ratio is < 75%

// setCollateralCeiling(): Set the collateral_ceiling, which is the % of total collateral value that the contract can hold for any particular collateral type.

// setOracle(): Set the oracle_address.

// setCollateralAdd(): Set the collateral_address.

// addOwner(): Add an address to the array of owners.

// removeOwner(): Remove an owner from the owners array.
