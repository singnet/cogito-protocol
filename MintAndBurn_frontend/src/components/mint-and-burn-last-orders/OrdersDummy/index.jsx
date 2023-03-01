import Box from '@mui/material/Box';
import { Typography } from '@mui/material';



import {useStyles} from './styles';

const OrdersDummy = () => {

const classes = useStyles();
return (
<Box className={classes.OrdersDummy}>
    <Box className={classes.Content}>
    <svg width="100%" height="100%" viewBox="0 0 30 25" fill="none" xmlns="http://www.w3.org/2000/svg">
        <path d="M13.75 11.1111V13.8889H23.75V11.1111H13.75ZM2.5 12.5C2.5 8.625 4.55 5.27778 7.5 3.72222V0.694444C3.125 2.44444 0 7.06944 0 12.5C0 17.9306 3.125 22.5556 7.5 24.3056V21.2778C4.55 19.7222 2.5 16.375 2.5 12.5ZM18.75 0C12.55 0 7.5 5.61111 7.5 12.5C7.5 19.3889 12.55 25 18.75 25C24.95 25 30 19.3889 30 12.5C30 5.61111 24.95 0 18.75 0ZM18.75 22.2222C13.925 22.2222 10 17.8611 10 12.5C10 7.13889 13.925 2.77778 18.75 2.77778C23.575 2.77778 27.5 7.13889 27.5 12.5C27.5 17.8611 23.575 22.2222 18.75 22.2222Z" fill="#FFD748"/>
    </svg>
        Connect to view your mint and burn requests
    </Box>
</Box>
)};
export default OrdersDummy;