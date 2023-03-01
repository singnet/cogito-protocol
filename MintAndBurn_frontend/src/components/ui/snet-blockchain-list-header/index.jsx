import { Grid, Avatar, ListItemText } from '@mui/material';
import style from './style';
import propTypes from 'prop-types';

const SnetBlockchainListHeader = ({ blockchain }) => {
    return (
        <Grid container sx={style.grid} minWidth="0">
            <Grid item sm={4} sx={style.flex}>
                <Avatar alt={blockchain.name} src={blockchain.logo} />
                <ListItemText primary={blockchain.name} sx={style.blockchain} />
            </Grid>
        </Grid>
    );
};

SnetBlockchainListHeader.propTypes = {
    blockchain: propTypes.object.isRequired
}

export default SnetBlockchainListHeader;