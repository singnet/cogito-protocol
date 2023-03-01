import AppBar from '@mui/material/AppBar';
import Box from '@mui/material/Box';
import Wallets from './wallets';
import VotingLogo from './logo';
import navbarStyles from '../style';
import propTypes from 'prop-types';

const NavigationBar = ({ openModal, onWalletConnect }) => {
  const classes = navbarStyles();

  return (
    <AppBar position="static" color="white" sx={{ padding: 2 }} className={classes.header}>
      <Box className={classes.items}>
        <Box display="flex" alignItems="center" justifyContent="space-between">
          <VotingLogo/>
        </Box>
        <Wallets openModal={openModal} onWalletConnect={onWalletConnect}/>
      </Box>
    </AppBar>
  );
};

NavigationBar.propTypes = {
  openModal: propTypes.func.isRequired,
  onWalletConnect: propTypes.func.isRequired
};

export default NavigationBar;
