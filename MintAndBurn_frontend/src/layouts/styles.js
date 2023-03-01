import { makeStyles } from '@mui/styles';
import BackgroundImage from '../assets/images/background/main_background.png';

// eslint-disable-next-line import/prefer-default-export
export const useStyles = makeStyles({
  BaseStyles: {
    '--base-font-family':'Montserrat',
    '--base-font-color': '#FFFFFF',
    '--base-font-weight': '600',

    '--form-header-font-size': '2.1875rem',
    '--form-header-font-weight': '700',

    '--forms-container-header-font-size': '2.8125rem',
    '--forms-container-header-weight': '700',

    '--accent-font-color-1':'#FFC086',
    '--accent-font-color-2': '#FFA723',

    '--form-header-font-size': '2.1875rem',
    '--form-header-font-weight': '700',
    '--form-content-font-size': '1.25rem',
    '--input-border-color-1': '#DCB9FF',
    '--input-border-color-2': 'rgba(255, 182, 115, 1)',

    '--last-orders-container-header-font-size': '2.6875rem',
    '--last-orders-container-header-weight': '700',

    '--last-orders-dummy-font-weight': '500',
  },

  BackgroundHolder: {
    background: `url(${BackgroundImage}), linear-gradient(153.37deg, #2C2352 12.94%, #100E13 91.8%)`,
  },
  mainContainer: {
    padding: '4.0625rem 0 5rem 0',
    backgroundSize: 'cover',
    minHeight: '70vh',
  },
  wrapper: {
    maxWidth: 1280,
    margin: '0 auto',
    '@media(max-width: 1200px)': { padding: '0 20px' }
  }
});