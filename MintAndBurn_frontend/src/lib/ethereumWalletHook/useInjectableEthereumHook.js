import { ethers } from 'ethers';
import { useDispatch } from 'react-redux';
import { setError } from '../../store/slices/errors/errorSlice';
import { setIsLoading } from '../../store/slices/application/appSlice';

const useInjectableEthereumHook = () => {
  const dispatch = useDispatch();
  const ethereum = window?.ethereum;
  let provider = undefined;

  if(ethereum){
    provider = new ethers.providers.Web3Provider(ethereum);
  }

  const connectEthereumWallet = async (extension) => {
    try {
      if (ethereum.isMetaMask) { 
        dispatch(setIsLoading(true));
        const [accounts] = await provider.send("eth_requestAccounts", []);
        // signEthMessage(extension, '0x92c858b0219e7752ce586f16e4293669f779bca8', "test message");
        return accounts;
      }
    } catch (error) {
      console.error('Error on connectWallet: ', error);
      //throw error;
    }
  }
  
  const requetEthChainId = async (extension) => {
    try {
      
      if(ethereum.isMetaMask) {
        const chainID = await provider.send("eth_chainId", []);
        return Number(chainID);
      }
    }
    catch(error){
      console.error('Error on getChainID: ', error);
      //throw error;
    }
  }
  
  const signEthMessage = async (extension, address, message) => {
    try {
      if(ethereum.isMetaMask) {

        const [account] = await provider.send("eth_requestAccounts", []);
        if(account !== address){
          dispatch(setError({message: extension.wallet + ': Plese switch to the account that market as CURRENT!', showError: true}));
          return;
        }
        const signer = provider.getSigner();
        const signature = await signer.signMessage(message);
        console.log("signed message: ", message);
        console.log("from address: ", address);
        console.log("got signature: ", signature);
        return signature;
      }
    }
    catch(error){
      console.error('Error on sighn Eth message: ', error);
      //throw error;
    }
  }

  return {
    connectEthereumWallet,
    requetEthChainId,
    signEthMessage
  }
}

export default useInjectableEthereumHook;