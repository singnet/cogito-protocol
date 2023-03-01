const style = {
    flex: {
        display: 'flex',
        alignItems: 'center'
    },
    grid: {
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'space-between',
        '& button': {
            padding: '15px 25px 13px',
            borderWidth: 2,
            fontSize: 14,
            fontWeight: 600,
            letterSpacing: 1.25,
            lineHeight: '16px',
            '&:hover': { borderWidth: 2 }
        },
        '& ul': {
            borderBottom: '1px solid rgba(0, 0, 0, 0.12)',
        },
        '& li': {
            width: 72,
            height: 72,
            padding: 0,
            border: '1px solid #D6D6D6',
            borderRadius: '6px',
            marginRight: '32px',
            display: 'flex',
            justifyContent: 'center',
            backgroundColor: '#FFF',
            boxShadow: '0 0 8px 0 rgba(173,180,180,0.24)',
            cursor: 'pointer',
            '&:last-of-type': {
                marginRight: 0
            },
            '& img': { width: '70%' },
            '&:hover': {
                border: '1px solid #4F13E0',
                backgroundColor: 'rgba(79,19,224,0.1)'
            }
        }
    },
    blockchain: {
        color: '#212121',
        fontSize: 20,
        letterSpacing: -0.25,
        lineHeight: '24px',
        '& span': {
            marginLeft: '7px',
            fontSize: 16
        }
    },
};

export default style;