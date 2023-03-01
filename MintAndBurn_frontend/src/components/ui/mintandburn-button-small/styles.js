const styles = {

base: {
    
    '& > button': {
        width: '100%',
        padding: '0.5rem 2.5rem',
        borderRadius: '0.625rem',
        border: 'none',
        color: 'var(--base-font-color)',
        
        fontSize: 'var(--base-font-size)',

        fontFamily: 'var(--base-font-family)',
        fontWeight: 'var(--form-header-font-weight)',

        background: 'linear-gradient(175deg, #DCD87B 5.82%, rgba(255, 67, 67, 0.19) 95.02%)',
        boxShadow: '0px 2px 32px rgba(145, 135, 255, 0.25), 0px 4px 4px rgba(0, 0, 0, 0.25)',

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
variants: {
}

}
export default styles;