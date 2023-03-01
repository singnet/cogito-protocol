import Box from '@mui/material/Box';
import { useStyles } from './styles';

const AmountInput = ({variant}) => {
    const classes = useStyles();
    return (
        
        // <Box className={`classes. variant-${variant}`}>
        <Box className={[classes.AmountContainer, `variant-${variant}`]}>
            <Box className='right-side-container'>
                Balance: 0 
            </Box>
            <Box className='input-container'>
                <input type="text" placeholder='Enter'/>
                <Box className='max-amount-container'>
                    Max
                </Box>
            </Box>
            <Box className='right-side-container'>
                Min amount: 0
            </Box>
        </Box>
    )
}
export default AmountInput;