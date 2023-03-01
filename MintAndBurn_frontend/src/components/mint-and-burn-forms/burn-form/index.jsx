import Box from '@mui/material/Box';
import { useStyles } from './styles';

import MintandburnButton from '../../ui/mintandburn-button';
import AmountInput from '../amount-input';

const BurnForm = () => {
    const classes = useStyles();
    return (
        // <section className={classes.testSection}>
            <Box className={classes.mintForm}>
                <Box className="header">
                    <Box className="icon-holder">
                    <svg width="65" height="63" viewBox="0 0 65 63" fill="none" xmlns="http://www.w3.org/2000/svg">
<rect x="1.5" y="1.5" width="62" height="60" rx="18.5" fill="url(#paint0_linear_715_64)"/>
<rect x="1.5" y="1.5" width="62" height="60" rx="18.5" stroke="black" strokeWidth="3"/>
<rect x="1.5" y="1.5" width="62" height="60" rx="18.5" stroke="url(#paint1_linear_715_64)" strokeWidth="3"/>
<defs>
<linearGradient id="paint0_linear_715_64" x1="11" y1="3.88307e-07" x2="54" y2="63" gradientUnits="userSpaceOnUse">
<stop stopColor="#FFF6A2"/>
<stop offset="1" stopColor="#2BFE40" stopOpacity="0.2"/>
</linearGradient>
<linearGradient id="paint1_linear_715_64" x1="11" y1="3.88307e-07" x2="54" y2="63" gradientUnits="userSpaceOnUse">
<stop stopColor="#FFF6A2"/>
<stop offset="1" stopColor="#2BFE40" stopOpacity="0.2"/>
</linearGradient>
</defs>
</svg>

                    </Box>
                    <Box className="gap">

                    </Box>
                    <Box className="text-holder margin-right">
                        Burn Gcoin
                    </Box>
                </Box>
                <AmountInput variant='2'></AmountInput>
                <Box className='details-list'>


                    <Box className='details-item'>
                        <Box className='property-name'>
                            You will pay
                        </Box>
                        <Box className='property-value'>
                            -
                        </Box>
                    </Box>
                    <Box className='details-item'>
                        <Box className='property-name'>
                            Fees
                        </Box>
                        <Box className='property-value'>
                            -
                        </Box>
                    </Box>
                    <Box className='details-item'>
                        <Box className='property-name'>
                            Total
                        </Box>
                        <Box className='property-value'>
                            -
                        </Box>
                    </Box>
                    <Box className='details-item'>
                        <Box className='property-name'>
                            Minimal ADA requirement
                        </Box>
                        <Box className='property-value'>
                            -
                        </Box>
                    </Box>



                </Box>
                <Box className='button-component'>

                <MintandburnButton onClick={null} name='Burn' variant='burn' disabled={false} />
                {/* <MintandburnButton onClick={null} name='Mint' variant='burn' disabled={true} /> */}

                </Box>
            </Box>
        // </section>
    )
}
export default BurnForm;