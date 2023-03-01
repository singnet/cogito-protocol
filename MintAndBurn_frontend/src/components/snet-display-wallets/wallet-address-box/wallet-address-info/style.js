const style = {
    icon: {
        height: 20,
        color: '#212121'
    },
    btnsAfterConnectOrAdd: {
        '& button': { 
            padding: '15px 20px 13px 10px !important' 
        }
    },
    disconnectBtn: { 
        color: '#D0021B' 
    },
    addressInfo: {
        margin: '15px 0'
    },
    buttonWalletAddress: {
        height: '15px'
    },
    currentBold: {
        color: 'teal',
        fontWeight: '800',
    },
    flexBox: {
        display: 'flex',
        alignItems: 'center'
    },
    flexNoShrinkBox: {
        flexShrink: '0',
        width: '30px',
        height: '30px',
        marginRight: '10px',
        '& *' : {
            width: '100%',
            height: '100%',
        }
    }
};

export default style;