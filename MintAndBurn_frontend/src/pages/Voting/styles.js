import { makeStyles } from '@mui/styles';
import ColorCodes from '../../assets/theme/colorCodes';

export const useStyles = makeStyles({
  container: {
    backgroundColor: ColorCodes.white,
    borderRadius: 4,
    boxShadow: '0 1px 1px 0 rgba(0,0,0,0.07), 0 2px 1px -1px rgba(0,0,0,0.07), 0 1px 3px 0 rgba(0,0,0,0.1)',
    '@media (min-width:1023px)': {
      width: 690
    }
  },
  headerContainer: {
    height: 50,
    borderBottom: `1px solid ${ColorCodes.border}`,
    alignItems: 'center',
    paddingLeft: 25,
    display: 'flex',
    marginBottom: 40
  },
  successMsg: {
    fontSize: 14,
    height: 45,
    border: `1px solid ${ColorCodes.successMsgBorder}`,
    borderRadius: 3,
    backgroundColor: ColorCodes.successMsgBg
  },
  submitBtn: {
    height: 36,
    width: 177,
    fontSize: 14,
    letterSpacing: 1.25,
    lineHeight: 16,
    alignItems: 'center',
    justifyContent: 'center',
    marginBottom: 20
  },
  backdrop: {
    animationDelay: '1s',
    width: '100vw',
    height: '100vh',
    pointerEvents: 'none !important',
    zIndex: 2099
  }
});
