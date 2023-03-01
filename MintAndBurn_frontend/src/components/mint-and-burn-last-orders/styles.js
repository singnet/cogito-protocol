import { makeStyles } from '@mui/styles';

export const useStyles = makeStyles({
    MintAndBurnLastOrders: {
        width:'100%',
        display: 'flex',
        flexDirection: 'column',
    },
    Header2: {
        margin: 'auto',
        marginBottom: '2.5rem',
        color: 'var(--base-font-color)',
        fontWeight: 'var(--forms-container-header-weight)',
        fontSize: 'var(--last-orders-container-header-font-size)',
        fontFamily: 'var(--base-font-family)',
    },
    LastOrdersContainer: {
        width:'100%',
        borderRadius: '1.25rem',
        background: 'linear-gradient(180deg, #FF9797 0%, rgba(243, 103, 255, 0) 100%)',
        padding: '3.125rem',

        display:'flex',
        flexDirection: 'column',
        justifyContent: 'space-between',
        fontFamily: 'var(--base-font-family)',
    }
})