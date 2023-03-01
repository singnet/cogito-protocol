const styles = {

base: {
    width: '100%',
    '& > button': {
        width: '100%',
        height: '4.375rem',
        borderRadius: '1.25rem',
        border: 'none',
        color: 'var(--base-font-color)',
        
        fontSize: 'var(--form-header-font-size)',

        fontFamily: 'var(--base-font-family)',
        fontWeight: 'var(--form-header-font-weight)',
    }
},
variants: {
    mint: {
        '& > button': {
            background: 'linear-gradient(178deg, #7B90DC 5.82%, rgba(206, 67, 255, 0.86) 95.02%)',
            boxShadow: '0px 8px 11px 1px rgba(206, 74, 218, 0.15)',
            '&:hover:not(:disabled), &:focus-visible':{
                boxShadow: '0px 11px 26px 3px rgba(251, 35, 255, 0.25)',
                outline: '2px solid #AE7DED',
                cursor: 'pointer',
            },
    
            '&:disabled': {
                background: 'linear-gradient(157.81deg, rgba(123, 144, 220, 0.6) 5.82%, rgba(206, 67, 255, 0.516) 95.02%)',
                outline: '2px solid #2D2D2D',
                cursor: 'not-allowed',
            }
        }
    },
    burn: {
        '& > button': {
            background: 'linear-gradient(129.57deg, #F9AD4E 31.82%, #FFDA7C 72.62%)',
            boxShadow: '0px 11px 11px 1px rgba(224, 255, 35, 0.15)',
            '&:hover:not(:disabled), &:focus-visible':{
                boxShadow: '0px 11px 15px 3px rgba(255, 167, 35, 0.25)',
                outline: '2px solid #E38846',
                cursor: 'pointer',
            },
    
            '&:disabled': {
                background: 'linear-gradient(129.57deg, rgba(249, 173, 78, 0.5) 31.82%, rgba(255, 218, 124, 0.5) 72.62%)',
                outline: '2px solid #2D2D2D',
                cursor: 'not-allowed',
            }
        }
    },
}

}
export default styles;