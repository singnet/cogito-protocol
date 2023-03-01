import { createSlice } from "@reduxjs/toolkit";

const applicationSlice = createSlice({
    name: 'application',
    initialState: {
        isLoading: false,
        isCacheDisabled:  process.env.REACT_APP_ENABLE_CACHE
    },
    reducers: {
        setIsLoading: (state, action) => {
            state.isLoading = action.payload;
        }
    }
});

export const { setIsLoading } = applicationSlice.actions
 
export default applicationSlice;