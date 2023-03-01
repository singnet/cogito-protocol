// import { availableBlockchains } from "../data/constants/constants";

// export const fetchCollection = async (params) => {
//     try {
//         const data = await axios.post('', params);
//         //updateCacheMethodHere from lib/cache like
//         //cacheData(availableBlockchains.CARDANO, data[availableBlockchains.CARDANO])

//     } catch (error) {
//         console.error(error);
//         throw error;
//     }
// };
import axios from "axios";

export const getProposalsByEventId = async (address) => {
  try {
    const payload = {
      address: '',
      // address: '0xba6be225a4cd7319696427246a8d62078de8c75a',
      event_id: process.env.REACT_APP_EVENT_ID
    };
    // console.log(payload);
    // console.log(process.env);
    const {data} = await axios.post(
      process.env.REACT_APP_BASE_API_URI + "/proposals/",
      payload
    );
    return data;
  } catch (error) {
    console.log(error);
    throw error;
  }
};
export const getPoolsByEventId = async () => {
  // try {
  //   const payload = {
  //     event_id: process.env.REACT_APP_EVENT_ID
  //   };
  //   // console.log(payload);
  //   // console.log(process.env);
  //   const {data} = await axios.post(
  //     process.env.REACT_APP_BASE_API_URI + "/pools/",
  //     payload
  //   );
  //   return data;
  // } catch (error) {
  //   console.log(error);
  //   throw error;
  // }
};
export const savePoolAnswers = async ({address, message, signature}) => {
  // try {
  //   console.log("const savePoolAnswers = async");
  //   console.log({
  //     event_id: process.env.REACT_APP_EVENT_ID,
  //     message: message,
  //     address: address,
  //     signature: signature,
  //   });
  //   const {data} = await axios.post(
  //     process.env.REACT_APP_BASE_API_URI + "/proposal/save",
  //     {
  //       event_id: process.env.REACT_APP_EVENT_ID,
  //       message,
  //       address,
  //       signature,
  //     }
  //   );
  //   console.log(data);
  //   return data;
  // } catch (error) {
  //   console.log(error);
  //   throw error;
  // }
};
export const getWalletsCollection = async (address) => {
  // try {
  //   console.log("getWalletsCollection");
  //   const {data} = await axios.post(
  //     process.env.REACT_APP_BASE_API_URI + "/wallet/collection",
  //     {
  //       event_id: process.env.REACT_APP_EVENT_ID,
  //       current_wallet: {
  //         address
  //       }
  //     }
  //   );
  //   console.log(data);
  //   return data;
  // } catch (error) {
  //   console.log(error);
  //   throw error;
  // }
};
export const updateWalletsCollection = async ({currentWallet, newWallet, removeWallet}) => {
  // try {
  //   console.log("updateWalletsCollection");

  //   const {data} = await axios.post(
  //     process.env.REACT_APP_BASE_API_URI + "/wallet/collection/update",
  //     {
  //       event_id: process.env.REACT_APP_EVENT_ID,
  //       current_wallet: {
  //         address: currentWallet?.address,
  //         extension: currentWallet?.extension
  //       },
  //       new_wallet: {
  //         address: newWallet?.address,
  //         extension: newWallet?.extension
  //       },
  //       remove_wallet: {
  //         address: removeWallet?.address,
  //         extension: removeWallet?.extension
  //       }
  //     }
  //   );
  //   console.log(data);
  //   return data;
  // } catch (error) {
  //   console.log(error);
  //   throw error;
  // }
};
