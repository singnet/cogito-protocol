import { createAsyncThunk } from '@reduxjs/toolkit';
import { getProposalsByEventId, getPoolsByEventId, savePoolAnswers } from "../../../utils/HTTPrequests";

export const getProposals = createAsyncThunk('event/proposals', async () => {
    const response = await getProposalsByEventId();
    return response.data;
});
export const getPools = createAsyncThunk('event/pools', async () => {
    const response = await getPoolsByEventId();
    return response.data;
});
export const savePool = createAsyncThunk('event/savePool', async (payload) => {
    console.log("export const savePool)");
    console.log(payload);
    const response = await savePoolAnswers(payload);
    console.log("export const savePool")
    console.log(response)
    return response.data;
});
