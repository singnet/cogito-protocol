import { blockchains } from '../../data/constants/constants'
import SnetDialog from '../snet-dialog';
import SnetTypography from '../snet-typography';
import ConnectOptions from './connect-options';
import propTypes from 'prop-types';
import { Backdrop, CircularProgress } from '@mui/material';
import {useStyles} from './styles';
import { useSelector } from 'react-redux';

const SnetConnectWallets = ({ isDialogOpen, onDialogClose, onWalletConnect }) => {

  const isLoading = useSelector(state => state.application.isLoading);
  const classes = useStyles();

  const WalletConnectorByBlockchain = () => {
    return blockchains.map((blockchain) => {
      if(blockchain.name === 'Cardano'){
      return (
        <ConnectOptions blockchain={blockchain} key={blockchain.name} onWalletConnect={onWalletConnect}/>
      )
      }
    })
  }

  return (
    <>
      <SnetDialog title="Connect Your Wallets" onDialogClose={onDialogClose} isDialogOpen={isDialogOpen}>
        <WalletConnectorByBlockchain />
        <SnetTypography />
      </SnetDialog>
    </>
  );
};

SnetConnectWallets.propTypes = {
  isDialogOpen: propTypes.bool.isRequired,
  onDialogClose: propTypes.func.isRequired,
  onWalletConnect: propTypes.func.isRequired
};

export default SnetConnectWallets;
