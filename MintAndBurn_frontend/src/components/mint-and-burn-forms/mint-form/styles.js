import { makeStyles } from '@mui/styles';

import mintFormBackground from '../../../assets/images/background/mintForm_background.png';

export const useStyles = makeStyles({
    testSection: {
        display: "flex",
        overflow: "scroll",
        resize: "both",

        width: "calc(620px + 16.8px)",
        height: "800px"

    },

    mintForm: {


        width: '100%',
        height: '46.625rem',
        flex: '0 1 49%',

        background: 'blue',
        borderRadius: '2.5rem',
        color: 'var(--base-font-color)',
        fontSize: 'var(--form-content-font-size)',
        padding: '3.125rem',
        display: 'flex',
        flexDirection: 'column',
        justifyContent: 'space-between',
        fontFamily:'var(--base-font-family)',
        fontWeight: 'var(--base-font-weight)',



        background: `url(${mintFormBackground}), linear-gradient(157.81deg, #7B90DC 5.82%, rgba(206, 67, 255, 0.19) 95.02%)`,
        boxShadow: '0px 11px 32px 1px rgba(145, 135, 255, 0.25)',
        backdropFilter: 'blur(6.5px)',
        

        '& .header': {
            // color: 'var(--color)',
            width: '100%',
            // marginBottom: '2.3125rem',

            display: 'flex',
            alignItems: 'center',
            

            '& .icon-holder': {
                width:'4.0625rem',
                height: '3.9375rem',

                // border: '1px solid red',
                flexShrink: '0',
            },
            '& .gap':{
                flex:'0 1 1.5rem',
            },
            '& .text-holder':{
                fontSize:'var(--form-header-font-size)',
                fontWeight: 'var(--form-header-font-weight)',
            }
        },

        '& .details-list':{
            // 'margin-bottom': '1.75rem',


            '& .details-item':{
                marginBottom:'2.8125rem',
                display: 'flex',
                justifyContent: 'space-between',

                '&:last-child':{
                    marginBottom: '0px',
                }
            },
        }
    }







});