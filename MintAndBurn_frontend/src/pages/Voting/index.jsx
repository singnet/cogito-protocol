import { Helmet } from 'react-helmet';
import { Backdrop, Box, CircularProgress, Typography } from '@mui/material';
import { useEffect, useState } from 'react';
import GeneralLayout from '../../layouts/GeneralLayout';
import SnetSnackbar from '../../components/ui/snet-snackbar';
import { useStyles } from './styles';
import ColorCodes from '../../assets/theme/colorCodes';
import { useDispatch, useSelector } from 'react-redux';
import { getProposals, getPools} from "../../store/slices/event/eventActions";

import MintAndBurnForms from '../../components/mint-and-burn-forms/';
import MintAndBurnLastOrders from '../../components/mint-and-burn-last-orders/';

const Voting = () => {
  console.log("COMPONENT VOTING PAGE")
  const isLoading = useSelector(state => state.application.isLoading);
  const [error, setError] = useState({ showError: false, message: '' });
  const dispatch = useDispatch();
  const { proposals } = useSelector((state) => state.event);

  useEffect(() => {
    if (!proposals.length) {
      dispatch(getProposals());
    }
  });

  useEffect(() => {
    if (!proposals.length) {
      dispatch(getPools());
    }
  });
  const closeError = () => {
    setError({ showError: false, message: '' });
  };

  const classes = useStyles();

  return (
    <>
      <Backdrop className={classes.backdrop} open={isLoading}>
        <CircularProgress color="white" />
      </Backdrop>
      <Helmet>
        <title>SingularityNet Voting</title>
      </Helmet>
      <GeneralLayout>
        <Box display="flex" flexDirection="column" justifyContent="center" alignItems="center" width="100%">
          <MintAndBurnForms></MintAndBurnForms>
          <MintAndBurnLastOrders></MintAndBurnLastOrders>
        </Box>
      </GeneralLayout>
    </>
  );
};
export default Voting;
