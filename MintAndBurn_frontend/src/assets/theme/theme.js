import { red, grey, orange } from '@mui/material/colors';
import { createTheme } from '@mui/material/styles';
import colorCode from './colorCodes';
import MuliRegular from '../fonts/muli/Muli-Regular.ttf';
import MuliLight from '../fonts/muli/Muli-Light.ttf';

const theme = createTheme({
  typography: {
    fontFamily: ['MuliRegular', 'MuliLight'].join(','),
    fontSize: 18,
    h6: {
      fontSize: '14px'
    },
    h5: {
      fontSize: '16px'
    },
    h3: {
      fontSize: '20px'
    },
    caption: {
      fontSize: '12px',
      color: grey[500]
    }
  },
  components: {
    MuiCssBaseline: {
      styleOverrides: `
        @font-face {
          font-family: 'MuliRegular';
          font-style: normal;
          font-display: swap;
          font-weight: 400;
          src: url(${MuliRegular}) format('truetype');
        },
        @font-face {
          font-family: 'MuliLight';
          font-style: light;
          font-display: swap;
          font-weight: 400;
          src: url(${MuliLight}) format('truetype');
        }
      `
    }
  },
  palette: {
    primary: {
      main: colorCode.lightBlue
    },
    secondary: {
      main: colorCode.gray
    },
    error: {
      main: red.A400
    },
    white: {
      main: colorCode.white
    },
    grey: {
      main: grey[600]
    },
    warning: {
      main: orange[500]
    }
  }
});

export default theme;
