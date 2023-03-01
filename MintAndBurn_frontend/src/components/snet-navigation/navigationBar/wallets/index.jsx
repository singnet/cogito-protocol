import { useSelector } from 'react-redux';
import AccountBalanceWalletIcon from '@mui/icons-material/AccountBalanceWallet';
import IconButton from '@mui/material/IconButton';
import Typography from '@mui/material/Typography';
import Box from '@mui/material/Box';
import SnetButton from '../../../ui/snet-button';
import useNavbarStyles from '../../style';
import propTypes from 'prop-types';

import MintandburnButtonSmall from '../../../ui/mintandburn-button-small';

const Wallets = ({ openModal, onWalletConnect}) => {
    const classes  = useNavbarStyles();
    const walletsCount = useSelector(state => state.wallets.walletsCount);

    const isAnyWalletConnected = () => {
        return walletsCount > 0;
    };

    const openWalletInfo = () => {
        onWalletConnect(false);
        openModal();
    }

    const openWalletConnector = () => {
        onWalletConnect(true);
        openModal();
    }

    return isAnyWalletConnected() ? (
        <Box onClick={openWalletInfo} className={classes.walletConnectionInfo}>
            <IconButton>
                <AccountBalanceWalletIcon />
            </IconButton>
            <Box>
                <Typography>Wallets</Typography>
                <span>{walletsCount} Connected</span>
            </Box>
        </Box>
    ) : (
        <>
            <SnetButton name="Connect Wallets" onClick={openWalletConnector} />
            <MintandburnButtonSmall name="Connect Wallets" onClick={openWalletConnector}></MintandburnButtonSmall>
        </>
        
    )
};

Wallets.propTypes = {
    openModal: propTypes.func.isRequired,
    onWalletConnect: propTypes.func.isRequired
};


export default Wallets;