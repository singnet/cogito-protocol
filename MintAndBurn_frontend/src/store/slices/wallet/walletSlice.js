import {createAsyncThunk, createSlice} from "@reduxjs/toolkit";
import {progress} from "../../../data/constants/constants";
import {getWalletsCollection, updateWalletsCollection} from "../../../utils/HTTPrequests";
import {savePool} from "../event/eventActions";

/*
    walletObj = {
        address: string,
        extension: string
    }
*/

const getCollection = createAsyncThunk(
  'wallet/getWalletsCollection',
  async (address) => {
      const response = await getWalletsCollection(address)
      console.log("getWalletsCollection(address)");
      console.log(response);
      return response.data
  }
)

const updateCollection = createAsyncThunk(
  'wallet/updateWalletsCollection',
  async (payload) => {
      const response = await updateWalletsCollection(payload)
      console.log("updateWalletsCollection(address)");
      console.log(response);
      return response.data
  }
)

const WalletSlice = createSlice({
    name: "wallets",
    initialState: {
        currentWallet: {},
        isCollectionLocked: false,
        ethereumWallets: [],
        cardanoWallets: [],
        walletsCount: 0
    },
    reducers:  {
        addEthereumWallet: (state, action) => {
            const currentState = state.ethereumWallets;
            const newState = [action.payload, ...currentState];
            state.ethereumWallets = newState;
            state.currentWallet = {...action.payload, blockchain: "Ethereum"};
            state.walletsCount++;
        },
        addCardanoWallet: (state, action) => {
            const currentState = state.cardanoWallets;
            const newState = [action.payload, ...currentState];
            state.cardanoWallets = newState;
            state.currentWallet = {...action.payload, blockchain: "Cardano"};
            state.walletsCount++;
        },
        removeEthereumWallet: (state, action) => {
            const currentState = state.ethereumWallets;
            const addressOfWalletToRemove = action.payload.address;
            const newState = currentState.filter(wallet => wallet.address !== addressOfWalletToRemove);
            state.ethereumWallets = newState;
            state.walletsCount--;
        },
        removeAllEthereumWallets: (state) => {
            const ethereumWalletsLength = state.ethereumWallets.length;
            state.ethereumWallets = [];
            state.walletsCount -= ethereumWalletsLength;
        },
        removeCardanoWallet: (state, action) => {
            const currentState = state.cardanoWallets;
            const addressOfWalletToRemove = action.payload.address;
            const newState = currentState.filter(wallet => wallet.address !== addressOfWalletToRemove);
            state.cardanoWallets = newState;
            state.walletsCount--;
        },
        removeAllCardanoWallets: (state) => {
            const cardanoWalletsLength = state.cardanoWallets.length;
            state.cardanoWallets = [];
            state.walletsCount -= cardanoWalletsLength;
        },
        setCurrentAddress: (state, action) => {
            const currentState = state.currentWallet;
            const blockchainName = action.payload.blockchainName;
            const currentWallet = action.payload.wallet;
            state.currentWallet = {...currentWallet, blockchain: blockchainName};
        },
        setCurrentAddressAsNull: (state) => {
            state.currentWallet = null;
        }
    },
    extraReducers: (builder) => {
        builder.addCase(getCollection.pending, (state) => {
            state.loading = progress.PROCESSING;
        });
        builder.addCase(getCollection.fulfilled, (state, action) => {
            state.ethereumWallets = action.payload?.ethereum_wallets.reduce((acc, curr) => {
                acc.push({
                    address: curr.address,
                    extension: curr.extension
                })
            }, [])
            state.cardanoWallets = action.payload?.cardano_wallets.reduce((acc, curr) => {
                acc.push({
                    address: curr.address,
                    extension: curr.extension
                })
            }, [])
            state.isCollectionLocked = action.payload?.is_locked;
            state.loading = progress.IDLE;
        });
    },
});

export const { addEthereumWallet, addCardanoWallet, removeCardanoWallet, removeEthereumWallet, setCurrentAddress, removeAllEthereumWallets, removeAllCardanoWallets, setCurrentAddressAsNull } = WalletSlice.actions;
export default WalletSlice;
