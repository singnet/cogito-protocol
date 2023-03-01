import { useDispatch } from 'react-redux';
import { Box } from '@mui/material';
import { supportedWallets, availableBlockchains } from '../../../data/constants/constants';
import { addEthereumWallet, addCardanoWallet } from '../../../store/slices/wallet/walletSlice';
import { setError } from '../../../store/slices/errors/errorSlice';
import { useSelector } from 'react-redux';
import useInjectableEthereumHook from '../../../lib/ethereumWalletHook/useInjectableEthereumHook';
import useInjectableCardanoHook from '../../../lib/cardanoWalletHook/useInjectableCardanoHook';
import SnetBlockchainListHeader from '../../ui/snet-blockchain-list-header';
import ExtensionList from './extensions-list';
import style from './style';
import isNil from 'lodash/isNil';
import propTypes from 'prop-types';
import { setIsLoading } from '../../../store/slices/application/appSlice';

const ConnectOptions = ({ blockchain, onWalletConnect }) => {
    const isLoading = useSelector(state => state.application.isLoading);
    const ETHEREUM_CHAIN_ID = process.env.REACT_APP_INFURA_NETWORK_ID;
    const CARDANO_CHAIN_ID = process.env.REACT_APP_CARDANO_NETWORK_ID;
    const { connectEthereumWallet, requetEthChainId } = useInjectableEthereumHook();
    const { connectCardanoWallet, requetCardanoChainId } = useInjectableCardanoHook();
    const cardanoAddresses = useSelector(state => state.wallets.cardanoWallets);
    const ethereumAddresses = useSelector(state => state.wallets.ethereumWallets);
    // const {cacheData, getCachedData, isDataInCache} = useCacheCore();
    const BLOCKCHAIN_IDENTIFIER = blockchain.name.toUpperCase();
    const dispatch = useDispatch();

    const constructWalletObject = (address, extension) => {
        return {
            address: address,
            extension: extension
        }
    }

    const isAddressAvailable = (address) => {
        return !isNil(address);
    }

    const isAddressConnected = (address, addresses) => {
        const matches = addresses.filter(addr => {
            return addr.address === address
        });
        return matches.length > 0;
    }

    const isAtExpectedNetwork = (blockchain, chainID) => {
        switch (blockchain) {
            case availableBlockchains.ETHEREUM: {
                const isAtExpectedEthNetwork = chainID === Number(ETHEREUM_CHAIN_ID);
                return isAtExpectedEthNetwork;
            }
            case availableBlockchains.CARDANO: {
                const isAtExpectedEthNetwork = chainID === Number(CARDANO_CHAIN_ID);
                return isAtExpectedEthNetwork;
            }
            default: {
                setError({showError: true, message: "Cannot validate chainID!"})
            }
        }
    }

    const connectEthereumWalletIfNotConnected = async (extension) => {
        const address = await connectEthereumWallet(extension);
        if(!isAddressAvailable(address)) {
            dispatch(setError({showError: true, message: extension.wallet + ": extension is not present or it is locked!"}));
            return false;
        }
        if(!isAtExpectedNetwork(availableBlockchains.ETHEREUM, await requetEthChainId(extension))){
            dispatch(setError({showError: true, message: extension.wallet + ": is not in expected network!"}));
            return false;
        }
        if(isAddressConnected(address, ethereumAddresses)){
            dispatch(setError({showError: true, message: extension.wallet + ": You are connecting with the same wallet address! Please, switch accounts!"}));
            return false;
        } else {
            const walletObject = constructWalletObject(address, extension.identifier);
            dispatch(addEthereumWallet(walletObject));
            return true;
        }
    }

    const connectCardanoWalletIfNotConnected = async (extension) => {
        const address = await connectCardanoWallet(extension);
        if(!isAddressAvailable(address)) {
            dispatch(setError({showError: true, message: extension.wallet + ": extension is not present or it is locked!"}));
            return false;
        }
        if(!isAtExpectedNetwork(availableBlockchains.CARDANO, await requetCardanoChainId(extension))){
            dispatch(setError({showError: true, message: extension.wallet + ": is not in expected network!"}));
            return false;
        }
        if(isAddressConnected(address, cardanoAddresses)){
            dispatch(setError({showError: true, message: extension.wallet + ": You are connecting with the same wallet address! Please, switch accounts!"}));
            return false
        } else {
            const walletObject = constructWalletObject(address, extension.identifier);
            dispatch(addCardanoWallet(walletObject));
            return true;
        }
    }

    const connectWallet = async (extension) => {
        try {
            switch (blockchain.name) {
                case availableBlockchains.ETHEREUM: {
                    const isWalletConnectedSuccessfully = await connectEthereumWalletIfNotConnected(extension);
                    if(isWalletConnectedSuccessfully){
                        onWalletConnect(false);
                    }
                    break;
                }
                case availableBlockchains.CARDANO: {
                    const isWalletConnectedSuccessfully = await connectCardanoWalletIfNotConnected(extension);
                    if(isWalletConnectedSuccessfully){
                        onWalletConnect(false);
                    }
                    break;
                }
                default: {
                    setError({showError: true, message: "Cannot connect using this method!"})
                }
            }
            dispatch(setIsLoading(false));
        } catch (error) {
            console.log('Error while connecting wallet', error);
            throw error;
        }
    }

    return (
        <Box sx={(style.box, style.customBox)}>
            <SnetBlockchainListHeader blockchain={blockchain} />
            <Box display="flex" alignItems="center" marginTop={4} marginBottom={4}>
                <ExtensionList connectWallet={connectWallet} supportedExtensions={supportedWallets[BLOCKCHAIN_IDENTIFIER]} />
            </Box>
        </Box>
    );
};

ConnectOptions.propTypes = {
    blockchain: propTypes.object.isRequired,
    onWalletConnect: propTypes.func.isRequired
};

export default ConnectOptions;
