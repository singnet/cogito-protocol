import { makeStyles } from '@mui/styles';

export const useStyles = makeStyles({
    OrdersDummy: {
        width:'100%',
        height: '100%',
        display: 'flex',
        flexDirection: 'column',
        justifyContent: 'center',
        alignItems: 'center',
    },
    Content: {
        
        padding: '2.6875rem 0',
        display: 'flex',
        alignItems: 'center',
        fontSize: '2.1875rem',
        color: 'var(--base-font-color)',
        fontWeight: 'var(--last-orders-dummy-font-weight)',

        '& svg':{
            flexShrink: '0',
            width: '1.875rem',
            marginRight: '0.9375rem',
        }
    }
})