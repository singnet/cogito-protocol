import { Address } from '@emurgo/cardano-serialization-lib-asmjs';
import { useDispatch } from "react-redux";
import { setError } from '../../store/slices/errors/errorSlice';
import { setIsLoading } from '../../store/slices/application/appSlice';
import {supportedWallets} from "../../data/constants/constants";

const useInjectableCardanoHook = () => {

  const cardano = window?.cardano;
  const dispatch = useDispatch();

  const requetCardanoChainId = async (extension) => {
    try {
      const connectingWallet = extension.identifier.toLowerCase();

      if (cardano[connectingWallet]) {
        const wallet = await cardano[connectingWallet].enable();
        const currentNetworkId = await wallet.getNetworkId();
        return Number(currentNetworkId);
      };
    } catch (error) {
      console.error('Error on getNetworkID: ', error);
      //throw error;
    }
  }

  const connectCardanoWallet = async (extension) => {
    try {
      const connectingWallet = extension.identifier.toLowerCase();

      if (cardano[connectingWallet]) {
        dispatch(setIsLoading(true));
        const wallet = await cardano[connectingWallet].enable();
        const account = await wallet.getUsedAddresses();
        const address = decodeAddress(account[0]);
        // signMessage(extension, 'addr_test1qrkzyfkrv2a76f86rfgv9yud8jg5kexjz3lndwgnlurlekptgf6qedqf28g82wk2ewppzjk9pnulqpvgdeuet88aexuqjxywnm', "testMessage");
        return address;
      };
    } catch (error) {
      console.error('Error on connectWallet: ', error);
      //throw error;
    }
  };

  const getWalletByIdentifier = (chain, identifier) => {
    return supportedWallets[chain].find(wallet => wallet.identifier === identifier);
  }

  const signMessage = async (extensionIdentifier, address, message) => {
    const extension = getWalletByIdentifier('CARDANO', extensionIdentifier);
    try {
      const encodedMessage = Buffer.from(message, 'ascii').toString('hex');
      const connectingWallet = extension.identifier.toLowerCase();
      const api = await window.cardano[connectingWallet].enable();
      const currentAddress = await api.getUsedAddresses();
      const currentDecodedAddress = decodeAddress(currentAddress[0]);

      if (address !== currentDecodedAddress) {
        dispatch(setError({ message: extension.wallet + ': Plese switch to the account that market as CURRENT!', showError: true }));
        return null;
      }

      const { signature, key } = await api.signData(currentAddress[0], encodedMessage);
      console.log("signed message: ", message);
      console.log("from address: ", address);
      console.log("got signature: ", signature);
      console.log("got key: ", key);

      return { signature, key }
    } catch (error) {
      console.error('Error on signMessage: ', error);
      //throw error;
    }
  }

  const decodeAddress = (address) => {
    return Address.from_bytes(Buffer.from(address, 'hex')).to_bech32();
  }

  return {
    requetCardanoChainId,
    connectCardanoWallet,
    signMessage,
    decodeAddress
  }
}

export default useInjectableCardanoHook;
