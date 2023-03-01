import { List, ListItem, Tooltip, } from "@mui/material";
import style from '../style';
import propTypes from 'prop-types';

const ExtensionsList = ({supportedExtensions, connectWallet}) => {

    return (
        <List sx={style.cardanoWalletList}>
            {supportedExtensions.map((extension) => {
                return (
                    <ListItem key={extension.identifier} onClick={() => connectWallet(extension)}>
                        <Tooltip title={extension.wallet}>
                            <img alt={extension.wallet} src={extension.logo} />
                        </Tooltip>
                    </ListItem>
                );
            })}
        </List>
    )
}
ExtensionsList.propTypes = {
    supportedExtensions: propTypes.array.isRequired,
    connectWallet: propTypes.func.isRequired
};

export default ExtensionsList;
