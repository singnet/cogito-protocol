import Box from '@mui/material/Box';
import { Typography } from '@mui/material';

import OrdersDummy from './OrdersDummy';

import {useStyles} from './styles';

const MintAndBurnLastOrders = () => {

const classes = useStyles();
return (
<Box className={classes.MintAndBurnLastOrders}>
    <Box className={classes.Header2}>Last orders</Box>
    <Box className={classes.LastOrdersContainer}>
        <OrdersDummy></OrdersDummy>
    </Box>

</Box>
)};
export default MintAndBurnLastOrders;