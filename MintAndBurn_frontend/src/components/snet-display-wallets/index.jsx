import { useSelector, useDispatch} from 'react-redux';
import { Box } from '@mui/system';
import { blockchains, supportedWallets } from '../../data/constants/constants';
import SnetButton from '../ui/snet-button';
import SnetDialog from '../snet-dialog';
import SnetBlockchainListHeader from '../ui/snet-blockchain-list-header';
import WalletAddressBox from './wallet-address-box';
import propTypes from 'prop-types';
import {removeAllEthereumWallets, removeAllCardanoWallets, setCurrentAddressAsNull} from '../../store/slices/wallet/walletSlice'

const SnetDisplayWallets = ({ isDialogOpen, onDialogClose, onWalletConnect}) => {
    const dispatch = useDispatch();
    const cardanoAddresses = useSelector(state => state.wallets.cardanoWallets);
    const ethereumAddresses = useSelector(state => state.wallets.ethereumWallets);
    const isCollectionLocked = useSelector(state => state.wallets.isCollectionLocked);
    //const isLoading = useSelector(state => state.application.isLoading);
    const isCardanoAddressesEmpty = cardanoAddresses.length !== 0;
    const isEthereumAddressesEmpty = ethereumAddresses.length !== 0;

    const walletsContent = {
        [blockchains[0].name]: isEthereumAddressesEmpty,
        [blockchains[1].name]: isCardanoAddressesEmpty
    }

    const isEthereumBlockchain = (blockchain) => {
        return blockchain.name === blockchains[0].name;
    }

    const walletsByBlockchain = (blockchain) => {
        return isEthereumBlockchain(blockchain) ? ethereumAddresses : cardanoAddresses;
    }

    const extensionsByBlockchain = (blockchain) => {
        return isEthereumBlockchain(blockchain) ? mapArrayToObject(supportedWallets.ETHEREUM) : mapArrayToObject(supportedWallets.CARDANO);
    }

    const mapArrayToObject = (bArray) => {
        const result = {}
        bArray.forEach(element => {
            const id = element.identifier;
            result[id] = element;
        });
        return result;
    }

    const isToRenderByWalletsContent = (identifier) => {
        return walletsContent[identifier];
    }

    const isToRenderContent = () => {
        return walletsContent[blockchains[0].name] || walletsContent[blockchains[1].name]
    }

    const ConnectedWallets = () => {
        return  ( 
            blockchains.map((blockchain, index) => {
                return isToRenderByWalletsContent(blockchain.name) ? (
                    <Box key={index}>
                        <SnetBlockchainListHeader blockchain={blockchain} />
                        <WalletAddressBox 
                            isCollectionLocked={isCollectionLocked} 
                            blockchain={blockchain} 
                            wallets={walletsByBlockchain(blockchain)} 
                            extensions={extensionsByBlockchain(blockchain)}
                            onDialogClose={onDialogClose}
                        />
                    </Box>
                ) 
                : null
            }))
    }
    const onDisconnectAll = () => {
        dispatch(removeAllEthereumWallets());
        dispatch(removeAllCardanoWallets());
        dispatch(setCurrentAddressAsNull());
        onDialogClose();
    }
    return isToRenderContent() ? (
        <>
            <SnetDialog title="Connected Wallets" onDialogClose={onDialogClose} isDialogOpen={isDialogOpen}>
                <ConnectedWallets onDialogClose={onDialogClose}/>
                <SnetButton onClick={onWalletConnect} name='Connect more' />
                <SnetButton onClick={onDisconnectAll} name='Log out' />
            </SnetDialog>
        </>
    ) : null//setIsLoading()
}

SnetDisplayWallets.propTypes = {
    isDialogOpen: propTypes.bool.isRequired,
    onDialogClose: propTypes.func.isRequired,
    onWalletConnect: propTypes.func.isRequired
};

export default SnetDisplayWallets;