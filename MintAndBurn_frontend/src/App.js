import Router from './router';
import { useSelector } from 'react-redux';
import { useEffect } from 'react';

import "./fonts.css";



const App = () => {

const body = document.querySelector('body');
const isLoading = useSelector(state => state.application.isLoading);



useEffect(() => {
    if(isLoading){
        body.style.pointerEvents = 'none';
        body.style.overflow = 'hidden';
    } else {
        body.style.pointerEvents = 'all';
        body.style.overflow = 'auto';
    }
}, [isLoading]);

return(<Router />);

}



export default App;
