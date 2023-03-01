import { Box, Typography } from '@mui/material';
import walletMessages from '../../data/walletMessages/walletMessages';
import { useStyles } from './style';

const SnetTypography = () => {
    const classes = useStyles();

    const Messages = () => {
        const messagesLength = walletMessages.length;
        const isOneMessage = messagesLength === 1;
        return messagesLength ? (
            walletMessages.map((message, index) => {
                return (
                    <li key={index}>
                        <Typography>{isOneMessage ? '' : `${index + 1}.`} {message}</Typography>
                    </li>
                )
            })) : null;
    }

    return (
        <>
            <Box className={classes.snetTypography} >
                <ul className={classes.snetTypography}>
                    <Messages />
                </ul>
            </Box>
        </>
    )
}


export default SnetTypography;