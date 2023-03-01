import { useNavigate } from 'react-router-dom';
import { useSelector, useDispatch} from 'react-redux';
import Snackbar from '@mui/material/Snackbar';
import { Button } from '@mui/material';
import { isNil } from 'lodash';
import { closeError } from '../../../store/slices/errors/errorSlice';
import propTypes from 'prop-types';
import { useEffect, useState } from 'react';

const SnetSnackbar = ({ redirectTo }) => {
  const dispatch = useDispatch();
  const navigate = useNavigate();
  const message = useSelector(state => state.errors.message);
  const isOpenSnackbar = useSelector(state => state.errors.showError);

  const formatMessage = () => {
    console.log(message);
    return isNil(message) ? '' : JSON.stringify(message);
  };


  const closeSnetSnackbar = () => {
    dispatch(closeError());
  }

  return (
    <Snackbar
      anchorOrigin={{ vertical: 'top', horizontal: 'center' }}
      open={isOpenSnackbar}
      autoHideDuration={6000}
      onClick={closeSnetSnackbar}
      onClose={closeSnetSnackbar}
      message={formatMessage()}
    />
  );
};

SnetSnackbar.propTypes = {
  redirectTo: propTypes.string
};

SnetSnackbar.defaultProps = {
  redirectTo: null
};

export default SnetSnackbar;
