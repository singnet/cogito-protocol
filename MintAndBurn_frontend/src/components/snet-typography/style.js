import { makeStyles } from '@mui/styles';

// eslint-disable-next-line import/prefer-default-export
export const useStyles = makeStyles({
    snetTypography: {
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
        '& ul': {
            margin: '0 0 10px',
            width: '100%',
            padding: '16px',
            border: '1px solid #4086FF',
            backgroundColor: '#DEEAFF',
            borderRadius: '4px',
            '& li': {
                listStyle: 'none',
                '& p': {
                    fontSize: '16px',
                    fontWeight: 'bold',
                    letterSpacing: -0.01,
                    lineHeight: '28px',
                    textAlign: 'justify',
                    '&:last-of-type': { display: 'inline-block' }
                }
            }
        }
    }
});
