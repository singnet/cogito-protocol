import PropTypes from 'prop-types';
import Box from '@mui/material/Box';
import styles from '../mintandburn-button-small/styles';
const MintandburnButton = ({ name, onClick, variant, disabled }) => {
  return (
    <Box sx={[styles.base, styles.variants[variant]]}>
      <button disabled={disabled} onClick={onClick}>
        {name}
      </button>
    </Box>
  );
};

MintandburnButton.propTypes = {
  name: PropTypes.string,
  onClick: PropTypes.func,
  variant: PropTypes.string,
  disabled: PropTypes.bool
};

MintandburnButton.defaultProps = {
  variant: 'contained',
  disabled: false
};

export default MintandburnButton;
