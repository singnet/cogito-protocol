import { makeStyles } from '@mui/styles';

const useMenubarStyles = makeStyles(() => ({
  navigations: {
    padding: 0,
    margin: '0 0 0 20%',
    display: 'flex',
    '& li': {
      paddingRight: '24%',
      listStyle: 'none',
      '&:last-of-type': { paddingRight: 0 },
      '&:hover': {
        '& a': {
          color: '#4F13E0',
          fontWeight: 'bold'
        }
      },
      '& a': {
        color: '#9B9B9B',
        fontSize: 16,
        lineHeight: '20px'
      }
    },
    '@media(max-width: 960px)': { display: 'none' }
  },
  active: {
    '& a': {
      color: '#4F13E0 !important',
      fontWeight: 'bold'
    }
  },
  header: {
    lineHeight: '70px',
    padding: '1rem 3.75rem',
    boxShadow: '0 2px 3px 0 rgba(0,0,0,0.1)',

    background: 'linear-gradient(180deg, rgba(58, 50, 115, 0.75), rgba(97, 87, 166, 0.75), rgba(123, 112, 200, 0.75), rgba(95, 86, 163, 0.75), rgba(58, 50, 115, 0.75))',

    '@media(max-width: 1280px)': { padding: '9px 10px' },

    
  },
  logo: { height: '40px' },
  flex: {
    display: 'flex',
    alignItems: 'center'
  },
  items: {
    display: 'flex',
    justifyContent: 'space-between',
    alignItems: 'center',
    '@media(max-width: 480px)': {
      flexDirection: 'column',
      alignItems: 'flex-start'
    },
    '& button': {
      '@media(max-width: 600px)': { fontSize: 12 }
    }
  },
  walletConnectionInfo: {
    display: 'flex',
    cursor: 'pointer',
    '& svg': {
      color: '#fff',
      padding: 7,
      border: '1px solid #CCC',
      backgroundColor: '#9B9B9B',
      fontSize: 20,
      boxSizing: 'content-box',
      borderRadius: 25
    },
    '& p': {
      'margin-top': '7px',
      color: 'rgba(33,33,33,0.87)',
      fontSize: 16,
      fontWeight: 600,
      lineHeight: '20px',
      '@media(max-width: 600px)': { fontSize: 12 }
    },
    '& span': {
      color: '#4086FF',
      display: 'block',
      fontSize: 11,
      fontWeight: 600,
      lineHeight: '14px',
      textTransform: 'uppercase'
    }
  }
}));

export default useMenubarStyles;
