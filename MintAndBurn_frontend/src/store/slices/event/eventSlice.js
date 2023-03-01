import { createSlice } from '@reduxjs/toolkit';
import {progress } from '../../../data/constants/constants';
import { getProposals, getPools, savePool } from './eventActions';



const eventSlice = createSlice({
    name: 'event',
    initialState: {
        proposals: [],
        poolsOfQuestions: null,
        singleQuestions: [],
        poolsList: [],
        answers: {},
        selectedItem: null,
        loading: progress.PROCESSING
    },
    reducers: {
        setSelectedItem(state, action) {
            console.log(action);
            state.selectedItem = action.payload;
        },
        setAnswer(state, action) {
            console.log("setAnswer(state, action)")
            console.log(action);
            state.answers[action.payload.question_id] = action.payload.answer;
            console.log(state.answers);
        },
    },
    extraReducers: (builder) => {
        builder.addCase(getProposals.pending, (state) => {
            console.log("builder.addCase(getProposals.pending");
            state.loading = progress.PROCESSING;
        });
        builder.addCase(getProposals.fulfilled, (state, action) => {
            console.log("builder.addCase(getProposals.fulfilled");
            const proposalsList = action.payload.questions;
            let poolsObj = {};
            let questionsArr = [];
            proposalsList.map(question => {
                if (!question.pool_id) {
                    questionsArr.push(question);
                }
                else if (!poolsObj[question.pool_id]) {
                    poolsObj[question.pool_id] = [question];
                }
                else {
                    poolsObj[question.pool_id].push(question);
                }
            });
            state.proposals = proposalsList;
            state.poolsOfQuestions = poolsObj;
            state.singleQuestions = questionsArr;
            state.loading = progress.IDLE;
        });
        builder.addCase(getPools.pending, (state) => {
            console.log("builder.addCase(getPools.pending");
            console.log("state.loading = progress.PROCESSING");
            state.loading = progress.PROCESSING;
        });
        builder.addCase(getPools.fulfilled, (state, action) => {
            console.log("builder.addCase(getPools.fulfilled");
            console.log("action.payload.pools");
            console.log(action.payload.pools);
            state.poolsList = action.payload.pools;
            state.loading = progress.IDLE;
        });
        builder.addCase(savePool.pending, (state) => {
            console.log("builder.addCase(savePool.pending");
            console.log("state.loading = progress.PROCESSING");
            state.loading = progress.PROCESSING;
        });
        builder.addCase(savePool.fulfilled, (state, action) => {
            console.log("builder.addCase(savePool.fulfilled");
            console.log("action.payload");
            console.log(action.payload);
            state.loading = progress.IDLE;
        });
    }
});


export const {
    setSelectedItem,
    setAnswer,
} = eventSlice.actions;

export default eventSlice;
