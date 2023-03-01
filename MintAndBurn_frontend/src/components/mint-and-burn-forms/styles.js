import { makeStyles } from '@mui/styles';

export const useStyles = makeStyles({
    MintAndBurnForms: {
        width:'100%',
        display: 'flex',
        flexDirection: 'column',
        marginBottom: '4.0625rem',
    },
    Header: {
        margin: 'auto',
        marginBottom: '4rem',
        color: 'var(--base-font-color)',
        fontWeight: 'var(--forms-container-header-weight)',
        fontSize: 'var(--forms-container-header-font-size)',
        fontFamily: 'var(--base-font-family)',
    },
    FormsContainer: {
        display:'flex',
        width:'100%',
        justifyContent: 'space-between',
        // background: 'red',
    }
})