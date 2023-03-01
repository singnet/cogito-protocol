import { Box } from '@mui/system';
import WalletAddressInfo from './wallet-address-info';
import style from './style';
import propTypes from 'prop-types';

const WalletAddressBox = ({ blockchain, extensions, wallets, isCollectionLocked, onDialogClose }) => {
    
    const ConnectedAddresses = () => {
        return wallets.map((wallet) => {
            return (
                <WalletAddressInfo 
                    isCollectionLocked={isCollectionLocked} 
                    onDialogClose={onDialogClose}
                    blockchain={blockchain} 
                    walletObj={wallet} 
                    extension={extensions[wallet.extension]} 
                    key={wallet.address} />
            )
        })
    }

    return (
        <Box sx={style.walletInfoBox}>
            <ConnectedAddresses/>
        </Box>
    );
};

WalletAddressBox.propTypes = {
    onDialogClose: propTypes.func.isRequired,
    extensions: propTypes.object.isRequired,
    blockchain: propTypes.object.isRequired,
    wallets: propTypes.array.isRequired,
    isCollectionLocked: propTypes.bool.isRequired
}

export default WalletAddressBox;