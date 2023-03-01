import Button from '@mui/material/Button';
import PropTypes from 'prop-types';

const SnetButton = ({ name, onClick, variant, disabled }) => {
  return (
    <Button disabled={disabled} onClick={onClick} variant={variant} color="primary" style={{marginRight: '20px'}} id={`snet-button-${name}`}>
      {name}
    </Button>
  );
};

SnetButton.propTypes = {
  name: PropTypes.string,
  onClick: PropTypes.func,
  variant: PropTypes.string,
  disabled: PropTypes.bool
};

SnetButton.defaultProps = {
  variant: 'contained',
  disabled: false
};

export default SnetButton;
