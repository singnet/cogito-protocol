import Box from '@mui/material/Box';
import { useNavigate } from 'react-router-dom';
import Paths from '../../../../router/paths';
import LogoImage from '../../../../assets/images/logo/MintAndBurn_logo.svg';
import useNavbarStyles from '../../style';

const VotingLogoBlock = () => {
  const classes = useNavbarStyles();
  const navigate = useNavigate();

  const onClickLogo = () => {
    navigate(Paths.Voting);
  };

  return (
    <Box className={`${classes.flex}`}>
      <img src={LogoImage} alt="Logo" className={classes.logo} /> 
    </Box>
  );
};

export default VotingLogoBlock;
