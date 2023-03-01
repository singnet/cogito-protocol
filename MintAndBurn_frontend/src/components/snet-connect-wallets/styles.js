import { makeStyles } from '@mui/styles';

// eslint-disable-next-line import/prefer-default-export
export const useStyles = makeStyles({
  connectWalletContent: {
    width: 800,
    padding: '16px 24px',
    '& > div': {
      paddingBottom: 23,
      borderBottom: '1px solid #D6D6D6',
      marginBottom: 23,
      '&:last-of-type': {
        paddingBottom: 0,
        border: 'none',
        marginBottom: 0
      },
      '& > div': {
        flexDirection: 'column',
        alignItems: 'flex-start',
        '& > div': {
          '&:first-of-type': {
            '@media(max-width: 600px)': { marginBottom: '15px' }
          }
        },
        '@media(max-width: 600px)': {
          flexDirection: 'column',
          alignItems: 'center'
        }
      }
    },
    '@media(max-width: 800px)': { width: '100%' }
  },
  connectWalletActions: {
    padding: '0 24px',
    marginBottom: 32,
    display: 'flex',
    flexDirection: 'column',
    alignItems: 'center',
    '& ul': {
      width: '100%',
      padding: '16px',
      border: '1.5px solid #4086FF',
      borderRadius: 4,
      margin: '0 0 32px',
      backgroundColor: '#DEEAFF',
      '& li': {
        listStyle: 'none',
        '& p': {
          fontSize: 14,
          fontWeight: 'bold',
          letterSpacing: -0.01,
          lineHeight: '28px',
          '&:last-of-type': { display: 'inline-block' }
        },
        '& span': {
          paddingLeft: 5,
          color: '#4086ff',
          cursor: 'pointer',
          fontSize: 13,
          fontWeight: 600,
          letterSpacing: -0.14,
          lineHeight: '16px'
        }
      }
    },
    '& button': {
      fontSize: '14px',
      padding: '5px 76px !important',
      '@media(max-width: 600px)': { marginTop: 15 }
    },
    '@media(max-width: 600px)': { flexDirection: 'column' }
  }
});
