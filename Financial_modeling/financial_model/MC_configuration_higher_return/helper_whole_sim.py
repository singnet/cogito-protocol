import warnings
from simulate_returns import * 
import numpy as np
import matplotlib.pyplot as plt
import math
from scipy.stats import *
import scipy.stats as stats    
import time
### Let us simulate the usage:
def usage_sim(sigma_usage):
    T = 24*365*5
    usage = np.zeros(T)
    vol_usage = np.random.standard_t(3, size=T)
    dt = 1/(24*365)
    t=1
    while t<T:
        usage[t] = usage[t-1] + sigma_usage * vol_usage[t] * dt
        t+=1
    return(usage)
    
### We simulate the indexes:
def simulate_index(our_T=10*4+1 ,N=10000): ### our_T is when we assume to have an index for 3 months in front.
    ### Also, we will predict the index growth for 10.25 years in front, because everyone knows the index value 3 months in front, so we can adjust the calculations.

    ### Now that we have that, we want to smooth the index over many time frames.
    t = 0
    T = 6*24*365
    current_return = 0
    index_returns = np.zeros((N,T))
    simulate = np.zeros((N,our_T))
    price = np.ones((N,T))
    i = 0
    while i<N:
        timestamp = 0
        t = 1
        current_return = stats.gennorm.rvs(1.3017688944349377, 0.00496293, 0.02069869963423673, size=1)
        simulate[i,timestamp] = 1 + current_return
        current_return = current_return/(31*24*3)
        price[i,0] = 1 + current_return
        while t<T:
            
            if t%(31*24*3)==0:
                timestamp += 1
                current_return = stats.gennorm.rvs(1.3017688944349377,0.00496293, 0.02069869963423673, size=1)[0] 
                simulate[i,timestamp] = simulate[i,timestamp-1] * (1 + current_return)
                current_return = current_return/(31*24*3)### We divide by 12 because we discount on a yearly basis. Actually, we divide by 3 because the returns are meant to happen on 3-month basis based on the design alone.
            if timestamp>41:
                print("Timestamp higher than 41",t)
            index_returns[i,t] = current_return
            price[i,t] = price[i,t-1] + current_return ### we linearly  increase value.
            t+=1
        i+=1
    
    ### We return the simulated results: 
    return(index_returns,simulate,price)

### We get an array of our investments as an input and returns of our investments as well as an index, which shows which period returns we are looking for.
def update_investment(myarray,returns,our_index,dt):
    if isinstance(myarray, np.float64): ### Then we have just one number return.
        myarray = myarray + myarray*returns[our_index] * dt
    else:
        i = 0
        while i<len(myarray):
            myarray[i] += myarray[i] * returns[our_index] * dt
            i+=1
    return(myarray)    

def update_all_investments(l_risk1day, l_risk30day, l_risk180day, l_risk1year, m_risk1day, m_risk30day, m_risk180day,m_risk1year, h_risk1day, h_risk30day, h_risk180day, h_risk1year,ret_lrisk,ret_mrisk,ret_hrisk,our_index,dt):
    l2_risk1day = update_investment(l_risk1day,ret_lrisk,our_index,dt)
    l2_risk30day = update_investment(l_risk30day,ret_lrisk,our_index,dt)
    l2_risk180day = update_investment(l_risk180day,ret_lrisk,our_index,dt)
    l2_risk1year = update_investment(l_risk1year,ret_lrisk,our_index,dt)
    m2_risk1day = update_investment(m_risk1day,ret_mrisk,our_index,dt)
    m2_risk30day = update_investment(m_risk30day,ret_mrisk,our_index,dt)
    m2_risk180day = update_investment(m_risk180day,ret_mrisk,our_index,dt)
    m2_risk1year = update_investment(m_risk1year,ret_mrisk,our_index,dt)
    h2_risk1day = update_investment(h_risk1day,ret_hrisk,our_index,dt)
    h2_risk30day = update_investment(h_risk30day,ret_hrisk,our_index,dt)
    h2_risk180day = update_investment(h_risk180day,ret_hrisk,our_index,dt)
    h2_risk1year = update_investment(h_risk1year,ret_hrisk,our_index,dt)
    return(l2_risk1day, l2_risk30day, l2_risk180day, l2_risk1year, m2_risk1day, m2_risk30day, m2_risk180day,m2_risk1year, h2_risk1day, h2_risk30day, h2_risk180day, h2_risk1year)
    
def rebalance_assets(available_liquidity,CAR,treasury,liquid,our_index,rule =[0.25,0.75,0.00]):
    day = 1/365
    ### This is just a larger asset class rebalancing.
    investment = 0
    if CAR[our_index]>=.95:
        ratio = liquid/treasury[our_index]
        change_liquid = rule[0] - ratio
        lacking_money = change_liquid * treasury[our_index]
        liquid = liquid + min(available_liquidity,lacking_money)
        available_liquidity = available_liquidity - min(available_liquidity,lacking_money)
        
        ### Desired deposit is:
        desired_deposit = treasury[our_index] * rule[2] * day
        if desired_deposit>available_liquidity:
            deposit_now = available_liquidity * 0.5
        else:
            deposit_now = max(available_liquidity * rule[2] * day,0,desired_deposit)
        
        #treasury[our_index] = treasury[our_index] - deposit_now
        available_liquidity = available_liquidity - deposit_now
        investment = investment + available_liquidity
    elif CAR[our_index]>0.75 and CAR[our_index]<.95:
        x = CAR[our_index]
        ratio = liquid/treasury[our_index]
        change_liquid = rule[0] - ratio
        lacking_money = change_liquid * treasury[our_index]
        liquid = liquid + min(available_liquidity,lacking_money)
        available_liquidity = available_liquidity - min(available_liquidity,lacking_money)

        dep_rate = np.sqrt((x-.75)/(.2/rule[2]**2))
        desired_deposit = treasury[our_index] * dep_rate * day
        if desired_deposit>available_liquidity:
            deposit_now = available_liquidity * 0.5
        else:
            deposit_now = max(available_liquidity * rule[2] * day,0,desired_deposit)
        #deposit_now = max(available_liquidity * dep_rate *day,0,desired_deposit)
        available_liquidity = available_liquidity - deposit_now
        
        investment = investment + available_liquidity
    ### For now we do not consider scenarios below .75 CAR.
    ### But now we do!
    elif CAR[our_index]<=0.75:
        
        x = CAR[our_index]
        ratio = liquid/treasury[our_index]
        change_liquid = rule[0] - ratio
        lacking_money = change_liquid * treasury[our_index]
        liquid = liquid + min(available_liquidity,lacking_money)
        available_liquidity = available_liquidity - min(available_liquidity,lacking_money)
        investment = investment + available_liquidity
        deposit_now = 0
    return(deposit_now,investment,liquid)

def shift_days(invested):
    invested = np.roll(abc,1)
    invested[0] = 0
    return(invested)



### Every day we see which assets are available at the end of the day and based on that we get them to daily_sumup and shift all other assets that are staked for one day.
def sumup_daily_assets(low_risk1day, low_risk30day, low_risk180day, low_risk1year, med_risk1day, med_risk30day, med_risk180day,med_risk1year, high_risk1day, high_risk30day, high_risk180day, high_risk1year):
    daily_sumup = 0
    daily_sumup += low_risk1day
    daily_sumup += low_risk30day[-1]
    daily_sumup += low_risk180day[-1]
    daily_sumup += low_risk1year[-1]
    daily_sumup += med_risk1day
    daily_sumup += med_risk30day[-1]
    daily_sumup += med_risk180day[-1]
    daily_sumup += med_risk1year[-1]
    daily_sumup += high_risk1day
    daily_sumup += high_risk30day[-1]
    daily_sumup += high_risk180day[-1]
    daily_sumup += high_risk1year[-1]
    low_risk1day = 0
    low_risk30day = np.roll(low_risk30day,1)
    low_risk30day[0] = 0
    low_risk180day = np.roll(low_risk180day,1)
    low_risk180day[0] = 0
    low_risk1year = np.roll(low_risk1year,1)
    low_risk1year[0] = 0
    med_risk1day = 0
    med_risk30day = np.roll(med_risk30day,1)
    med_risk30day[0] = 0
    med_risk180day = np.roll(med_risk180day,1)
    med_risk180day[0] = 0
    med_risk1year = np.roll(med_risk1year,1)
    med_risk1year[0] = 0
    high_risk1day = 0
    high_risk30day = np.roll(high_risk30day,1)
    high_risk30day[0] = 0
    high_risk180day = np.roll(high_risk180day,1)
    high_risk180day[0] = 0
    high_risk1year = np.roll(high_risk1year,1)
    high_risk1year[0] = 0
    return(daily_sumup, low_risk1day, low_risk30day, low_risk180day, low_risk1year, med_risk1day, med_risk30day, med_risk180day,med_risk1year, high_risk1day, high_risk30day, high_risk180day, high_risk1year)

### We split invested amount among the riskiness of assets. This is based on CAR.
def spread_risk(inv_amount,CAR,our_indx, lrisk_pool,mrisk_pool,hrisk_pol):
    if (CAR[our_indx]>0.95 and CAR[our_indx]<=1.1): ### We invest relatively balanced...
        lrisk_pool[our_indx] = inv_amount * 0.33
        mrisk_pool[our_indx] = inv_amount * 0.34
        hrisk_pol[our_indx] = inv_amount * 0.33
    if CAR[our_indx]>1.1: ### We invest relatively risky...
        lrisk_pool[our_indx] = inv_amount * .2
        mrisk_pool[our_indx] = inv_amount * .3
        hrisk_pol[our_indx] = inv_amount * .5    
    if CAR[our_indx]<=.95: ### We invest relatively balanced...
        lrisk_pool[our_indx] = inv_amount * 0.45
        mrisk_pool[our_indx] = inv_amount * 0.35
        hrisk_pol[our_indx] = inv_amount * 0.2
    return(lrisk_pool,mrisk_pool,hrisk_pol)
    
def add_assets_to_pool(amount, oneday,month,halfyear,year,indx):
    
    ### It is strange to add negative assets:
    if amount<0:
        raise ValueError("Arrays must have the same size")
    
    oneday = amount * .25
    month[0] = amount * .25
    halfyear[0] = amount * .25
    year[0] = amount * .25
    return(oneday,month,halfyear,year)
    
def uniswap_calc(x,y,a,b):
    
    exch1 = y/x
    if a==0:
        a = (x*b)/(y+b)
        exch = b/a
        slippage = abs(exch-exch1)  
        slip_amount = abs(b* exch1 - b)##

    if b==0:
        b = (a*y)/(x+a)
        exch = b/a
        slippage = abs(exch-exch1)  
        slip_amount = abs(a * exch1 - b)
    return(slippage,slip_amount,a,b)

def calc_pool(b,e):
    a = b/e
    return(a) ### This is how many our tokens we need to have in the pool to maintain the peg perfectly.

def catastrophic_event(probability):
    rand_nr = np.random.uniform(0,1)
    if rand_nr < probability:
        return(1)
    else:
        return(0)

    
### We define the network growth:
def larger_sim(low_ret,mid_ret,high_ret,incoming_price,market_sentiment,staking_rate_other,rt_low,rt_high,deposit_rate,usage_growth,stakers_share,index_appreciation_incoming,target_liquid, starting_network_lower = 2 * 10**7, starting_network_upper = 3 * 10**7,low_risk_hack = np.zeros(24*365*5),mid_risk_hack = np.zeros(24*365*5),high_risk_hack = np.zeros(24*365*5),dist_asset = [0.25,0.74,0.01]): ### Price is the index appreciation.
    
    
    
    T=24*365*5
    dt = 1/(365*24)
    deposit_sum = np.zeros(T)
    foundation_fee = np.zeros(5)
    index_appreciation = index_appreciation_incoming * 1 
    
    price = incoming_price * 1
    our_peg = np.zeros(T) ### Peg that we keep on DEX.
    our_peg[0] = 1

    prob_bankrun = 0.03 ### Probability of a bank run. We can change that parameter at will.
    bank_run = np.zeros(T)
    duration_brun = 0 ### How long does our bank run last.
    sigma_amountlost = 1
    u = 0
    while u<T:
        if catastrophic_event(prob_bankrun*dt):
            print("We have a bank run at time", u)
            #bank_run[t] = 1
            
            duration_brun = np.random.randint(low=20, high = 60)
            amount_lost = np.random.uniform(0.2,0.4)
            avg_amountlostperhour = 1 - (1-amount_lost)**(1/duration_brun)
            
        if duration_brun>0:
            bank_run[u] = avg_amountlostperhour + sigma_amountlost * np.random.standard_t(df = 5) *avg_amountlostperhour
            duration_brun = duration_brun - 1
        
        u+=1
    
    CAR = np.zeros(T)
    ### model treasuries:
    treasuries = np.zeros(T) ### Assets?
    liabilities_base = np.zeros(T)
    nr_tokens = np.zeros(T)
    collateral = np.zeros(T)
    investments = np.zeros(T) ### Here we will have our investments
    
    ### Investment in low risk, high risk, medium risk staking returns. We also have different staking periods;
    low_risk1day = 0
    low_risk30day = np.zeros(90)
    low_risk180day = np.zeros(180)
    low_risk1year = np.zeros(365)
    med_risk1day = 0
    med_risk30day = np.zeros(90)
    med_risk180day = np.zeros(180)
    med_risk1year = np.zeros(365)
    high_risk1day = 0
    high_risk30day = np.zeros(90)
    high_risk180day = np.zeros(180)
    high_risk1year = np.zeros(365)
    
    ### We also invest in deposit rewards:
    deposit_rewards = np.zeros(T)
    ### But we also have summed pools of different pools (high, med, low risk).
    lrisk_pool = np.zeros(T)
    mrisk_pool = np.zeros(T)
    hrisk_pool = np.zeros(T)
    liquid_reserve = np.zeros(T)
    

    
    network_growth = np.zeros(T)
    network_growth[0] = np.random.uniform(starting_network_lower,starting_network_upper) ### Random number
    treasuries[0] = network_growth[0] * 1 ### We are 100% collateralized at the beginning.
    liabilities_base[0] = network_growth[0] * 1 ### We are 100% collateralized at the beginning.
    nr_tokens[0] = network_growth[0] * 1
    collateral[0] = treasuries[0]/liabilities_base[0]
    investments[0] = treasuries[0] *  (dist_asset[1]/(dist_asset[1] + dist_asset[0])) #We assume 75% invested in at the very beginning.
    
    ### Set up the initial parameters;
    liquid_reserve[0] = (dist_asset[0]/(dist_asset[1] + dist_asset[0])) *   treasuries[0]  
    deposit_rewards[0] =  dist_asset[2]  * treasuries[0] * np.random.uniform(0,0.8,1)[0] ### Amount of money in deposits at time t-1. This is our money. We assume that some of the investors will start with the money deposited and that we will use some of our foundation tokens to ensure the beginning liquidity in this area. We assume that we will have only a small amount of funds dedicated to this though. Besides, it can be volatile; we do not know how much money the foundation will be able to guarantee because this in part depends on the market conditions. We target to get to the same amount as we want to have the long term deposits, but we account for the chance that this might be expensive for us and that we might not be able to do so. So, we assume that in the best case we will get to 80% of that and that we will stabilize the amount of depositors later on. We are again overly cautios in this case.
    ### After investing in deposit rewards, we do not count this as our assets anymore
    
    ### treasuries[0] = treasuries[0] - deposit_rewards[0] ### We start with a deposited amount, so we do not take it away from treasuries
    
    ### Pools distribution:
    lrisk_pool[0] = (1/3) * investments[0]
    mrisk_pool[0] = (1/3) * investments[0]
    hrisk_pool[0] = (1/3) * investments[0]
    
    ## Now all the investments:
    low_risk1day = 0.25 * lrisk_pool[0] 
    low_risk30day[0] = 0.25 * lrisk_pool[0] 
    low_risk180day[0] = 0.25 * lrisk_pool[0] 
    low_risk1year[0] = 0.25 * lrisk_pool[0] 
    med_risk1day = 0.25 * mrisk_pool[0] 
    med_risk30day[0] = 0.25 * mrisk_pool[0] 
    med_risk180day[0] = 0.25 * mrisk_pool[0] 
    med_risk1year[0] = 0.25 * mrisk_pool[0] 
    high_risk1day = 0.25 * hrisk_pool[0]  
    high_risk30day[0] = 0.25 * hrisk_pool[0] 
    high_risk180day[0] = 0.25 * hrisk_pool[0] 
    high_risk1year[0] = 0.25 * hrisk_pool[0] 
    
    
    #indx_growth = 0#0.01 ### How much the index will grow on a yearly basis.
    indx_growth = np.zeros(T)
    indx_growth[0] = sum(index_appreciation[0:31*24*3])### we calculate the next 3 months of index appreciation.
    indx_factor = 2
    CAR_ratio = 1.5
    Cmomentum = 2.6
    deposit_profit = 5
    lastmonth = np.zeros(T)
    lastmonth[0] = 0
    mkt_constant = 0.55 ### We change this one because we are more pessimistic about the market growth going forward.
    ### We believe users will care more about the stability of the project than overal market hype as regulations kick in.
    ### There will also always be a competition and we will have to differ from others. Of course in case of very bad market conditions we then expect also smaller downturn, but we believe this is also a positive outcome of more regulation.
    #deposit_rate = np.ones(N) * 0.04
    c_usage_growth = 5.2
    sigma_ngrowth = 1
    growth_vol = np.random.standard_t(5, size=T)
    
    ### Modeling deposit rate demand;
    risk_premium = 0.025 ### They will stack it for 1 year, they want to be reimbursed for the risk.
    dep_rate = np.zeros(T)
    dep_rate[0] = max(staking_rate_other[0] - sum(index_appreciation[0:(365*24*3+0)]),0.01) + risk_premium
    sigma_dep_rate = 1
    dW_dep_rate = np.random.standard_t(5, size=T) * dt
    ### How much of a deposit will the users take away.
    daily_reduction = 0.25
    deposits = np.zeros(T)
    sigma_red = 0.5
    dW_deps = np.random.standard_t(5, size=T) * dt
    deposits[0] = max(min(daily_reduction + daily_reduction * sigma_red * dW_deps[0],1),0)*(1/24)
    
    ### We will have 1 year deposits only;
    our_deposits = np.zeros(24*365) ### money (in USD) which we have deposited for this.
    value_locked_appreciated = np.zeros(24*365) ### how our assets have appreciated over time (in our coin's tokens)
    value_locked = np.zeros(24*365) ### Original money put in value locked (in our coin's tokens)
    int_rate_deposit = np.zeros(24*365) ### Interest rate we took on a specific deposit.
    tokens_currently_deposited = np.zeros(T)
    percentage_deposited = np.zeros(T)
    
    
    deposit_sum[0] = deposit_rewards[0] * 1 ### Amount of money in deposits at time t-1. This is our money. How much we dedicated to deposit rewards.
    
    dep_demand_t = deposit_sum[0] * deposits[0]
    our_deposits[0] = dep_demand_t * 1 ### Deposited in USD. This is our money. I
    deposit_sum[0] = deposit_sum[0] - dep_demand_t
    value_locked_appreciated[0] = (dep_demand_t/dep_rate[0]) / price[0]
    value_locked[0] = (dep_demand_t/dep_rate[0]) / price[0]
    int_rate_deposit[0] = value_locked[0]*dep_rate[0]*dt  ### dep_rate[0]*dt ### This will actually be the amount which we add every turn.
    
    ### Some statistics tracking:
    total_deposited_tokens = np.zeros(T)
    total_staked_USD = np.zeros(T)
    total_deposit_returns = np.zeros(T)
    
    total_deposited_tokens[0] = total_deposited_tokens[0] + value_locked[0]
    total_staked_USD[0] = total_staked_USD[0] + our_deposits[0]
    total_deposit_returns[0] = 0
    
    daily_assets = 0 ###  The assets we gain or lose through trading per hour. Resets over a day.
    
    ### Collect statistics:
    tokens_currently_deposited[0] = sum(value_locked)
    percentage_deposited[0] = tokens_currently_deposited[0]/nr_tokens[0]
    usd_currently_deposited = np.zeros(T)
    usd_currently_deposited[0] = sum(our_deposits)                                       
    
    was_below60 = 0 ### Have we every dropped below 60% CAR? 0 no, 1 yes.
    
    ### Our pool:
    our_tokens = np.zeros(T)
    our_tokens[0] = calc_pool(liquid_reserve[0],1)
    ### And we note slippage:
    slippage = np.zeros(T)
    slippage_amounts = np.zeros(T)
    foundation_slippage_income = np.zeros(T)
    our_slippage_income = np.zeros(T)
    
    slippage_against_us = np.zeros(T) ### If we stop updating the price because the bank run was too big and we lose substantial liquidity pools and the the price starts growing, we will have some slippage going against us. The foundation will have to cover that.
    
    CAR_below075 = False
    liq_problem = False ### Is our liquidity bellow 2%?
    
    t=1
    while t<T:
        
        ### CAR calculation:
        weighted_assets = liquid_reserve[t-1] + low_risk1day + sum(low_risk30day) + 0.95 * sum(low_risk180day) + 0.85 * sum(low_risk1year) + med_risk1day + sum(med_risk30day) + 0.95 * sum(med_risk180day) + 0.85 * sum(med_risk1year) + high_risk1day + sum(high_risk30day) + 0.95 * sum(high_risk180day) + 0.85 * sum(high_risk1year)
        weighted_liabilities = percentage_deposited[t-1] * liabilities_base[t-1] * 0.88 + (1-percentage_deposited[t-1]) * liabilities_base[t-1]
        CAR[t] = weighted_assets/weighted_liabilities
        
        if CAR[t]<0.5:
            
            print("our CAR has dropped below 50%","in time",t)
            #price[t:] = price[t:] * CAR[t]

        #### Check if our CAR is low and if yes, then we set index growth to 0:
        if CAR[t]<=0.6:
            was_below60 = 1
            if price[t]>price[t-1]:
                price[t] = price[t-1] * 1
                index_appreciation[t] = 0

        tic = time.time()
        indx_growth[t] = sum(index_appreciation[t:(31*24*3+t)])
        apprec_sum = sum(index_appreciation[t:(365*24*1+t)])
        if CAR[t]<=0.6:
            min_price = min(price[t:(365*24*1+t)])
            apprec_sum = min_price - price[t]
            indx_growth[t] = min(price[t:(31*24*3+t)]) - price[t]
        ### We move deposits by one place:
        if (our_deposits[-1]!=0) or (value_locked_appreciated[-1]!=0) or (value_locked[-1]!=0) or (int_rate_deposit[-1]!=0):
            print("t:",t,"our_dep",int_rate_deposit)
            warnings.warn("Rolling values for deposits did not work appropriately!")
            #raise ValueError("Rolling values for deposits did not work appropriately!")
        our_deposits = np.roll(our_deposits,1)
        value_locked_appreciated = np.roll(value_locked_appreciated,1)
        value_locked = np.roll(value_locked,1)
        int_rate_deposit = np.roll(int_rate_deposit,1)    
        if t>=(30*24+1):
            lastmonth[t] = (network_growth[t-1] - network_growth[t-30*24-1])/network_growth[t-30*24-1]

    
        if t%24==0: ### New day:
            daily_assets_ret, low_risk1day, low_risk30day, low_risk180day, low_risk1year, med_risk1day, med_risk30day, med_risk180day,med_risk1year, high_risk1day, high_risk30day, high_risk180day, high_risk1year = sumup_daily_assets(low_risk1day, low_risk30day, low_risk180day, low_risk1year, med_risk1day, med_risk30day, med_risk180day,med_risk1year, high_risk1day, high_risk30day, high_risk180day, high_risk1year)
            
            
            available = daily_assets + daily_assets_ret ### Perhaps we don't need that because daily_assets already go to liquidity reserve.
            deposit_rewards[t],investments_daily,liquid_reserve[t-1]  =  rebalance_assets(available_liquidity = daily_assets_ret,CAR = CAR,treasury = liabilities_base,liquid = liquid_reserve[t-1],our_index = t-1,rule =dist_asset)
            ### Great, now we know the exact amounts of investments that we make that day.
            
            ### Now that we have investments, we spread the risk:
            lrisk_pool, mrisk_pool, hrisk_pool = spread_risk(inv_amount = investments_daily,CAR = CAR,our_indx = t, lrisk_pool = lrisk_pool,mrisk_pool = mrisk_pool,hrisk_pol = hrisk_pool)
            
            ### And now specific investments:
            low_risk1day, low_risk30day, low_risk180day, low_risk1year = add_assets_to_pool(amount = lrisk_pool[t], oneday = low_risk1day,month = low_risk30day,halfyear = low_risk180day,year = low_risk1year,indx=t)
            med_risk1day, med_risk30day, med_risk180day, med_risk1year = add_assets_to_pool(amount = mrisk_pool[t], oneday = med_risk1day,month = med_risk30day,halfyear = med_risk180day,year = med_risk1year,indx=t)
            high_risk1day, high_risk30day, high_risk180day, high_risk1year = add_assets_to_pool(amount = hrisk_pool[t], oneday = high_risk1day,month = high_risk30day,halfyear = high_risk180day,year = high_risk1year,indx=t)
            
            
            
            daily_assets = 0   
   
        
        
        ### Calculate appreciation:
        if CAR[t]>0.75:
            value_locked_appreciated = value_locked_appreciated + int_rate_deposit 
            if CAR_below075:
                CAR_below075 = False
        else:
            value_locked_appreciated = value_locked_appreciated + 0 
            if not CAR_below075:
                print("CAR just dropped below .75")
                CAR_below075 = True
            

        market_growth = (market_sentiment[t])**(-(np.sign((market_sentiment[t] - market_sentiment[t-1]))-1)/2) * (abs(market_sentiment[t]+1))**((np.sign((market_sentiment[t] - market_sentiment[t-1]))+1)/2)  * mkt_constant  
        volatility = growth_vol[t] * sigma_ngrowth * dt
        dnetwork = indx_growth[t] * indx_factor * network_growth[t-1] * dt + ((collateral[t-1] - 1)/collateral[t-1]) * CAR_ratio * network_growth[t-1] * dt +lastmonth[t] * Cmomentum * network_growth[t-1] * dt + market_growth* network_growth[t-1] * dt + (deposit_rate[t] + indx_growth[t] - staking_rate_other[t] ) * deposit_profit * network_growth[t-1] * dt + usage_growth[t] * c_usage_growth * network_growth[t-1] * dt + volatility - bank_run[t] * network_growth[t-1] ### Bank run shows the amount of lost in our specific hour.
        
        stakers_share[t] = percentage_deposited[t-1] * 1### Updating the amount of stakers.
        
        network_growth[t] = network_growth[t-1] + dnetwork  * (1 - abs((np.sign(dnetwork)-1)/2) * stakers_share[t] ) 
        #if t%(24*365)==0:
        #    print("Third time measurement",time.time() - tic )
        
        ### We update our investments:
        low_risk1day, low_risk30day, low_risk180day, low_risk1year, med_risk1day, med_risk30day, med_risk180day,med_risk1year, high_risk1day, high_risk30day, high_risk180day, high_risk1year = update_all_investments(l_risk1day = low_risk1day, l_risk30day = low_risk30day, l_risk180day = low_risk180day, l_risk1year = low_risk1year, m_risk1day = med_risk1day, m_risk30day = med_risk30day, m_risk180day = med_risk180day,m_risk1year = med_risk1year, h_risk1day = high_risk1day, h_risk30day = high_risk30day, h_risk180day = high_risk180day, h_risk1year = high_risk1year,ret_lrisk = rt_low,ret_mrisk = staking_rate_other,ret_hrisk = rt_high,our_index = t,dt=dt)
        ### Now, what if we have a hack?
        low_risk1day = low_risk1day - low_risk1day *  low_risk_hack[t]
        low_risk30day = low_risk30day - low_risk30day *  low_risk_hack[t]
        low_risk180day = low_risk180day - low_risk180day *  low_risk_hack[t]
        low_risk1year = low_risk1year - low_risk1year *  low_risk_hack[t]
        med_risk1day = med_risk1day - med_risk1day *  mid_risk_hack[t]
        med_risk30day = med_risk30day - med_risk30day *  mid_risk_hack[t]
        med_risk180day = med_risk180day - med_risk180day *  mid_risk_hack[t]
        med_risk1year = med_risk1year - med_risk1year *  mid_risk_hack[t]
        high_risk1day = high_risk1day - high_risk1day *  high_risk_hack[t]
        high_risk30day = high_risk30day - high_risk30day *  high_risk_hack[t]
        high_risk180day = high_risk180day - high_risk180day *  high_risk_hack[t]
        high_risk1year = high_risk1year - high_risk1year *  high_risk_hack[t]
        
        
        ### Our pool setup:
        
        
        if liquid_reserve[t-1]/liabilities_base[t-1]<0.02:
            liq_problem = True
            #reserve_in_pools = liquid_reserve[t-1] * 0.5
            ### We do not update the reserve in our pools.
            warnings.warn("Our fractional reserve has dropped below 0.02")
            reserve_in_pools = liquid_reserve[t-1] * 1
            our_tokens[t] = our_tokens[t-1]
        else:
            liq_problem = False
            reserve_in_pools = liquid_reserve[t-1] * 1
            our_tokens[t] = calc_pool(liquid_reserve[t-1],price[t])
            
        if dnetwork>0:
            treasuries[t] = treasuries[t-1] + dnetwork
            
            slippage[t],slippage_amounts[t],tokens_bought,money_spent = uniswap_calc(x=our_tokens[t] ,y=reserve_in_pools,a=0,b=dnetwork)
            ### now we go through exchange. In this case the users are buying our tokens and we are selling!
            if not money_spent==dnetwork:
                warnings.warn("input of b should be the same as output???")
                #raise ValueError("input of b should be the same as output???")
            nr_tokens[t] = nr_tokens[t-1] + tokens_bought### (dnetwork  * (1 - abs((np.sign(dnetwork)-1)/2) * stakers_share[t] ) ) / price[t]
            daily_assets += dnetwork ### We might not need this.
            if tokens_bought * price[t] > dnetwork:
                #print("x:",our_tokens[t],"y:",liquid_reserve[t-1],"a",0,"b:",dnetwork)
                #print("time:",t)
                slippage_against_us[t] = tokens_bought * price[t] - dnetwork
                #warnings.warn("This is a strange behavior. Slippage works against us?")
                #raise Warning("This is a strange behavior. Slippage works against us?")
            
            
            foundation_slippage_income[t] = slippage_amounts[t] * 1
            liquid_reserve[t] = liquid_reserve[t-1] + tokens_bought * price[t] #### dnetwork ### well, this is the new money getting in in theor
            ### Let's fix the slippage calculation in case of bad liquidity:
            if liq_problem:
                liquid_reserve[t] = liquid_reserve[t-1] + tokens_bought * (reserve_in_pools/our_tokens[t])
                slippage[t] = slippage[t] + (price[t] - (reserve_in_pools/our_tokens[t]))
            
        else:# dnetwork<0:
            our_decrease = - dnetwork  * (1 - abs((np.sign(dnetwork)-1)/2) * stakers_share[t] )
            slippage[t],slippage_amounts[t],tokens_sold,money_spent = uniswap_calc(x=our_tokens[t] ,y=reserve_in_pools,a=our_decrease,b=0)
            if tokens_sold * price[t] < money_spent:
                print("time:",t)
                print("our_tokens[t]:",our_tokens[t],"liquid_reserve[t-1]",liquid_reserve[t-1],"our_decrease (a):",our_decrease,"b",0)
                warnings.warn("This is a strange behavior. Slippage works against us?")
                #raise Warning("This is a strange behavior. Slippage works against us?")
                        
            treasuries[t] = treasuries[t-1] - money_spent #+ tokens_sold * price[t]#+ dnetwork  * (1 - abs((np.sign(dnetwork)-1)/2) * stakers_share[t] ) * price[t]
            nr_tokens[t] = nr_tokens[t-1] - tokens_sold #(dnetwork  * (1 - abs((np.sign(dnetwork)-1)/2) * stakers_share[t] ) ) 
            daily_assets +=  dnetwork  * (1 - abs((np.sign(dnetwork)-1)/2) * stakers_share[t] ) * price[t]
            
            if money_spent>=liquid_reserve[t-1] :
                print("money_spent",money_spent,"and liq reserve:",liquid_reserve[t-1], "tokens",our_tokens[t]," and liq_problem:",liq_problem)
                warnings.warn("Money outflows due to oversell too high!")
            liquid_reserve[t] = liquid_reserve[t-1]  - money_spent### + dnetwork  * (1 - abs((np.sign(dnetwork)-1)/2) * stakers_share[t] ) * price[t]
            
            if liq_problem:
                slippage[t] = slippage[t] + (price[t] - (reserve_in_pools/our_tokens[t]))
            

            
            our_slippage_income[t] =  tokens_sold * price[t] - money_spent
        #liquid_reserve[t] = liquid_reserve[t -1] + daily_assets
        if t>=(365*24-1): ### After 1 year, we calculate deposit rates as well:
            nr_tokens[t] = nr_tokens[t] + value_locked_appreciated[-1] - value_locked[-1]
            liquid_reserve[t] = liquid_reserve[t] + our_deposits[-1]
            total_deposit_returns[t] = total_deposit_returns[t-1] +  value_locked_appreciated[-1] - value_locked[-1]
            value_locked_appreciated[-1] = 0
            value_locked[-1] = 0
            our_deposits[-1] = 0
            int_rate_deposit[-1] = 0
              
        
            
            
        ### Deposit rate:

        
        dep_rate[t] = max(staking_rate_other[t] - apprec_sum,0.005) + risk_premium + sigma_dep_rate * dW_dep_rate[t] * np.sqrt(dep_rate[t-1])
        deposits[t] = max(min(daily_reduction + daily_reduction * sigma_red * dW_deps[0],1),0)*(1/24)
    
        
        our_deposits[0] = deposit_sum[t-1] * deposits[t]
        deposit_sum[t] = deposit_sum[t-1] - our_deposits[0]
        value_locked_appreciated[0] = (our_deposits[0]/dep_rate[t]) / price[t] ### Diving by the price of the index, so we calculate the amount of tokens deposited appropriately!
        value_locked[0] = (our_deposits[0]/dep_rate[t]) / price[t]
        total_deposited_tokens[t] = total_deposited_tokens[t-1] + value_locked[0]  ### That is not necessary to track anymore, but we are leaving it for possible debugging purposes in the future.
        total_staked_USD[t] = total_staked_USD[t-1] + our_deposits[0]
        int_rate_deposit[0] = value_locked[0]*dep_rate[t]*dt  ###dep_rate[t] * (1/(365*24))
        
        
        ### Calculate all assets that we now have
        lrisk_pool[t] = low_risk1day +  sum(low_risk30day) + sum(low_risk180day) + sum(low_risk1year)
        mrisk_pool[t] = med_risk1day +  sum(med_risk30day) + sum(med_risk180day) + sum(med_risk1year)
        hrisk_pool[t] = high_risk1day +  sum(high_risk30day) + sum(high_risk180day) + sum(high_risk1year)

        investments[t] = lrisk_pool[t] + mrisk_pool[t] + hrisk_pool[t]
        
        treasuries[t] = investments[t] + liquid_reserve[t]
        
        liabilities_base[t] = nr_tokens[t] * price[t]
        collateral[t] = treasuries[t] /liabilities_base[t]
        deposit_sum[t] = deposit_sum[t] + deposit_rewards[t]
        
        tokens_currently_deposited[t] = sum(value_locked)
        percentage_deposited[t] = tokens_currently_deposited[t]/nr_tokens[t]
        usd_currently_deposited[t] = sum(our_deposits) 
        
        
        
        
        
        if t%(24*365)==0:
            if CAR[t]>1.1:
                print("here we will get money to the foundation")
                ### Get 1% of treasuries from liquidity pool. In case our liquidity pool is small, we ensure that we give max 50% of the liquidity pool to the foundation. We always want to have liquid money!
                foundation_fee[int((t*5)/T)] = 0.01 * treasuries[t] + sum(foundation_slippage_income[(t-365*24+1):t])
                liquid_reserve[t] = liquid_reserve[t] - 0.01 * min(0.01 * treasuries[t] ,0.5*liquid_reserve[t] )
            
            foundation_fee[int((t*5)/T)] = foundation_fee[int((t*5)/T)] + sum(foundation_slippage_income[(t-365*24+1):t])
        
        t+=1
    return(network_growth,treasuries,nr_tokens,liabilities_base,collateral, investments, liquid_reserve, lrisk_pool, mrisk_pool, hrisk_pool, low_risk1day, low_risk30day, low_risk180day, low_risk1year, med_risk1day, med_risk30day, med_risk180day,med_risk1year, high_risk1day, high_risk30day, high_risk180day, high_risk1year,dep_rate,deposit_sum,tokens_currently_deposited,percentage_deposited,usd_currently_deposited,foundation_fee,price,CAR,our_slippage_income,foundation_slippage_income,slippage)














