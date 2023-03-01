import { useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import { Typography, Tooltip } from '@mui/material';
import { Box } from '@mui/system';
import { setError } from '../../../../store/slices/errors/errorSlice';
import { availableBlockchains, blockchains } from '../../../../data/constants/constants'
import { removeCardanoWallet, removeEthereumWallet, setCurrentAddress } from '../../../../store/slices/wallet/walletSlice';
import Button from '@mui/material/Button';
import Stack from '@mui/material/Stack';
import WalletIcon from '@mui/icons-material/AccountBalanceWallet';
import CopyOrEditIcon from '@mui/icons-material/ContentCopy';
import LogoutIcon from '@mui/icons-material/Logout';
import style from './style';
import propTypes from 'prop-types';

const WalletAddressInfo = ({ blockchain, walletObj, isCollectionLocked, extension, onDialogClose }) => {
    const dispatch = useDispatch();
    const wallet = walletObj;
    const walletAddress = walletObj.address;
    const [copyButtonText, setCopyButtonText] = useState('Copy');
    const currentWallet = useSelector(state => state.wallets.currentWallet);
    const walletsCount = useSelector(state => state.wallets.walletsCount);

    const currentStatusContent = {
        CURRENT: 'Current',
        NOTCURRENT: 'Set current'
    };

    const onClickCopy = () => {
        onCopyAddress(walletAddress);
        setCopyButtonText('Copied');
        setTimeout(() => {
            setCopyButtonText('Copy');
        }, 3000);
    };

    const onCopyAddress = (address) => {
        navigator.clipboard.writeText(address);
    };

    const isCardanoBlockchain = () => {
        return blockchain.name === blockchains[1].name;
    }

    const addEllipsisInBetweenString = (str) => {
        if (isCardanoBlockchain()) {
            return `${str.substr(0, 25)}...${str.substr(str.length - 25)}`;
        }
        return str;
    };

    const isOneWalletConnected = () => {
        return walletsCount === 1;
    }

    const isAvailableToDisconnectCurrent = () => {
        return isOneWalletConnected();
    }

    const onDisconnect = () => {
        if (isWalletCurrent(wallet) && !isAvailableToDisconnectCurrent()) {
            dispatch(setError({ message: "You are not allowed to disconnect CURRENT wallet if there are other wallets. Please, set CURRENT status to another wallet or diconnect any other wallets if you want to disconnect this one.", showError: true }));
        } else {
            isCardanoBlockchain() ? dispatch(removeCardanoWallet(wallet)) : dispatch(removeEthereumWallet(wallet));
        }
        if(isOneWalletConnected()) {
            onDialogClose();
        }
    }

    const isWalletCurrent = (wallet) => {
        return wallet.address === currentWallet.address;
    }

    const DisconnectWalletButton = () => {
        return !isCollectionLocked ? (
            <Button
                onClick={onDisconnect}
                variant="text"
                sx={[style.disconnectBtn, style.buttonWalletAddress]}
                startIcon={<LogoutIcon />}
            >
                Disconnect
            </Button>
        ) : null;
    }

    const WalletAddressBlock = () => {
        return (
            <Typography variant="caption" color="text.primary" fontSize="16px">
                {addEllipsisInBetweenString(walletAddress)}
            </Typography>
        );
    }

    const CopyAddressButton = () => {
        return (
            <Button
                sx={style.buttonWalletAddress}
                padding="0"
                variant="text"
                onClick={onClickCopy}
                startIcon={<CopyOrEditIcon />}
            >
                {copyButtonText}
            </Button>
        )
    }

    const isExtensionAvailable = (blockchainName, wallet) => {
        try {
            switch (blockchainName) {
                case availableBlockchains.ETHEREUM: {
                    const ethereumExtension = window?.ethereum?.isMetaMask;
                    if(ethereumExtension) return true;
                    break;
                }
                case availableBlockchains.CARDANO: {
                    const cardanoExtension = window?.cardano[wallet];
                    if(cardanoExtension) return true;
                    break;
                }
            }
            return false
        }
        catch (error) {
            return false
        }

    }

    const onclickSetCUrrent = () => {
        if (!isExtensionAvailable(blockchain.name, extension.identifier)) {
            dispatch(setError({ message: 'Cannot set this address as CURRENT. ' + extension.wallet + ' extension is not found!', showError: true }));
            return;
        }
        dispatch(setCurrentAddress({ blockchainName: blockchain.name, wallet: wallet }));
    }

    const SetCurrentAddress = () => {
        const status = isWalletCurrent(wallet) ? currentStatusContent.CURRENT : currentStatusContent.NOTCURRENT;
        return (
            <Button
                sx={[style.buttonWalletAddress, style.currentButton, status === currentStatusContent.CURRENT ? style.currentBold : null]}
                padding="0"
                variant="text"
                onClick={onclickSetCUrrent}
            >
                {status}
            </Button>
        )
    }

    return (
        <Box sx={style.flexBox}>
            <Box sx={style.flexNoShrinkBox}>
                <Tooltip title={extension.wallet}>
                    <img alt={extension.wallet} src={extension.logo} />
                </Tooltip>
            </Box>
            <Box sx={style.addressInfo}>
                <Stack direction="row" alignItems="center">
                    <WalletIcon sx={style.icon} color="grey" />
                    <WalletAddressBlock />
                </Stack>
                <Stack direction="row" sx={style.btnsAfterConnectOrAdd}>
                    <CopyAddressButton />
                    <DisconnectWalletButton />
                    <SetCurrentAddress />
                </Stack>
            </Box>
        </Box>
    );
};

WalletAddressInfo.propTypes = {
    onDialogClose: propTypes.func.isRequired,
    blockchain: propTypes.object.isRequired,
    walletObj: propTypes.object.isRequired,
    isCollectionLocked: propTypes.bool.isRequired,
    extension: propTypes.object.isRequired
};

export default WalletAddressInfo;