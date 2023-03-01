//import { useSelector } from "react-redux"
import store from "store";

export const useCacheCore = () => {
    //const isCacheDisabled = useSelector(state => state.application.isCacheDisabled);
    const isCacheDisabled = false;
    // console.log('isCacheDisabled', isCacheDisabled);

    const constructWarnMessage = (attemptedAction) => {
        return "Trying to call " + attemptedAction + "... Please enable cache with DISABLE_CACHE=false in your .env file to make cahing functions work.";
    }

    const cacheData = (id, data) => {

        try {
            if (isCacheDisabled) {
                console.warn(constructWarnMessage("cacheData()"));
            } else {
                store.set(id, data);
            }
        } catch (error) {
            console.error(error);
        }
    }

    const isDataInCache = (id) => {

        try {
            if (isCacheDisabled) {
                console.warn(constructWarnMessage("isDataInCache()"));
            } else {
                return Boolean(store.get(id));
            }
        } catch (error) {
            console.error(error);
        }
    }

    const getCachedData = (id) => {
        try {
            if (isCacheDisabled) {
                console.warn(constructWarnMessage("getCachedData()"));
            } else {
                return store.get(id);
            }
        } catch (error) {
            console.error(error);
        }
    }

    const deleteCachedData = (id) => {

        try {
            if (isCacheDisabled) {
                console.warn(constructWarnMessage("deleteCachedData()"));
            } else {
                store.remove(id);
            }
        } catch (error) {
            console.error(error);
        }
    }

    return {
        cacheData, 
        isDataInCache,
        getCachedData,
        deleteCachedData
    }
}