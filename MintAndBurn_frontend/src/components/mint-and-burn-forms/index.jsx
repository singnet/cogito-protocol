import Box from '@mui/material/Box';
import { Typography } from '@mui/material';

import MintForm from "./mint-form";
import BurnForm from "./burn-form";

import {useStyles} from './styles';

const MintAndBurnForms = () => {

const classes = useStyles();
return (
<Box className={classes.MintAndBurnForms}>
    <Box className={classes.Header}>Mint or burn Gcoin</Box>
    <Box className={classes.FormsContainer}>
        <MintForm></MintForm>
        <BurnForm></BurnForm>
    </Box>

</Box>
)};
export default MintAndBurnForms;