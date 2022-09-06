### All in one go:
import numpy as np
import matplotlib.pyplot as plt
import math

### Let's make such a function, using exponential distribution between 0 and 1:
def loss_share(max_prob):
    def_share = np.random.uniform(0,1)
    ### max probability:
    norm_param = 1 - def_share**2 ### We make the distribution as 1-x^2
    def_share = max_prob * norm_param
    return(def_share)
### Will catastrophic event happen:
def catastrophic_event(probability):
    rand_nr = np.random.uniform(0,1)
    if rand_nr < probability:
        return(1)
    else:
        return(0)
### Incoming parameters:
### starting_b_value: We assume that long term staking rate is changing. starting_b_value is the one we are having now.
### low_bound_b: What is the assumed lower bound for long term staking rate. It can potentially go lower due to random, but we assume that if that happens, it won't last long.
### upper_bound_b: What is the assumed upper bound for long term staking rate. It can potentially go higher due to random, but we assume that if that happens, it won't last long.
### ltr_vol_std: Volatility of the long term staking rate. This also affects random around the actual rate. Due to this random, it can get lower/higher.
### probability_of_default: Initial probability of default of our staking protocol. In case of a default, we will lose money!
### ltr_default_prob: Long term default probability of our staking protocol. In the long run, we expect the probability of default to decrease due to increased security and regulation.
### starting_staking_rate: what is the staking rate that we can get on the market currently (at the start of our stablecoin trading).
### r_t_sigma: volatility of current staking rate (that is besides the volatility of the long term rate).
### ltr_vol: volatility of the long term staking rate .
### sigma_market_time: we might have bull or bear market and that is determined by market_time. However, when that happens is in some way left to random. sigma_market_time is that random, the higher it is the more volatility we have in that.
### max_prob: this is the meximum amount we have staked in one protocol.



def sim_returns(starting_b_value = 0.015, low_bound_b = 0.02, upper_bound_b = 0.04, ltr_vol_std = 0.1, probability_of_default = 0.01, ltr_default_prob = 0.001 , starting_staking_rate = 0.015, r_t_sigma = 0.005 ,  sigma_market_time = 2 , max_prob = 0.6, sigma_b = 2 ):

    
    
    ### We make a function that will yield through the time and randomly generate sinus values.
    t = 0
    T = int(5*365*24)
    year = 365*24
    dt = 1/year
    market_time_t_vals = np.random.standard_t(1.5, size=T)
    
    ### Get the loss due to hacks/extreme events:
    extreme_events = np.zeros(T)
    
    ### Modeling the b value in Vasicek model, which in our case is a process itself. Similar to Chen model...
    difference_bounds = (upper_bound_b-low_bound_b)*0.5
    long_term_avg_stake = np.zeros(T)
    ltr_volatility_process = np.random.standard_t(30, size=T)
    b_value = np.zeros(T)
    long_term_avg_stake[0] = np.sin(0) + ltr_vol_std * ltr_volatility_process[0]
    b_value[t] = starting_b_value + (1+long_term_avg_stake[t] ) * 0.01 * dt
    
    
    
    ### Cox–Ingersoll–Ross model for staking.
    ### b_value is the b value, a will be constant and we will have a significant volatility that will also depend on
    ### interest rate.
    ### getting the whole number of returns, starting with 1.
    R_T = np.ones(T)
    a = 3
    a1 = 3
    r_t = np.zeros(T)
    r_t_vol = np.random.standard_t(5, size=T)
    
    r_t[0] = starting_staking_rate
    dwb = np.random.normal(size=T)
    
    
    
    ### Probability of default right now is cca 1%. It will decrease over the time:
    default_prob = np.zeros(T)
    default_prob_summed = np.zeros(T)
    #our_x = np.log(probability_of_default - ltr_default_prob) ## possibility of logarithmic decay
    our_x = (probability_of_default - ltr_default_prob)
    our_x = our_x**-0.5
    ### We try to converge towards 0.1% probability of default.
    default_probabilities = np.zeros(T)
    def_vol = np.random.standard_t(2, size=T) ### We add some short term randomness to default probabilities.
    
    year = 1/(365*24)
    default_prob[0] = (probability_of_default - ltr_default_prob)
    default_prob_summed[0] = default_prob[0] + ltr_default_prob
    
    returns = np.ones(T)
    year = 1/(365*24)
    starting_mkt_time = 1-((starting_b_value - low_bound_b)/difference_bounds)
    starting_mkt_time = np.sin(starting_mkt_time)**-1
    starting_mkt_time = starting_mkt_time * T
    starting_mkt_time = starting_mkt_time/(3*np.pi)
    ### How did we get to this number and equation?
    #b_value[t] = b_value[t-1] + a1 * ((low_bound_b + difference_bounds*(1+long_term_avg_stake[t] )) - b_value[t-1]) * dt+ sigma_b * dwb[t] * np.sqrt(b_value[t-1]) * dt
    ## This means that (low_bound_b + difference_bounds*(1+long_term_avg_stake[t] )) = x
    ## From that we can calculate that 
    
    mid_step = T*np.sin(starting_mkt_time)**-1
    substract_with = round(T*np.sin(starting_mkt_time)**-1/(2*np.pi),0)
    ### See https://www.wolframalpha.com/input?i=sin%28%28x%2FT%29*%28pi+*+3%29%29+%3D+k
    market_time = np.zeros(T)
    market_time[t] = starting_mkt_time### market_time[t] + 1*dt + sigma_market_time * market_time_t_vals[0]
    long_term_avg_stake[0] = - np.sin((market_time[0]/T)*(np.pi * 3)) + ltr_vol_std * ltr_volatility_process[0]
    t += 1
    while t<T:
        market_time[t] = market_time[t-1] + 1 + sigma_market_time * market_time_t_vals[t]
        long_term_avg_stake[t] = - np.sin((market_time[t-1]/T)*(np.pi * 3)) + ltr_vol_std * ltr_volatility_process[t]
        b_value[t] = b_value[t-1] + a1 * ((low_bound_b + difference_bounds*(1+long_term_avg_stake[t] )) - b_value[t-1]) * dt+ sigma_b * dwb[t] * np.sqrt(b_value[t-1]) * dt
        #b_value[t] = low_bound_b + 0.01*(1+long_term_avg_stake[t] ) * difference_bounds
        r_t[t] =max( r_t[t-1] + a * (b_value[t] - r_t[t-1]) * dt + r_t_sigma * np.sqrt(r_t[t-1]) * r_t_vol[t]* dt,0) 
        default_prob[t] =  default_prob[t-1] - 2 * (t*year+our_x)**-3 *year + default_prob[t-1] * def_vol[t] * year
        default_prob_summed[t] = default_prob[t] + ltr_default_prob
        uniform_prob = np.random.uniform(0,1)
        our_event = catastrophic_event((default_prob_summed[t])*year) 
        amount_lost = loss_share(max_prob)
        returns[t] = returns[t-1] + returns[t-1] * r_t[t] * year - our_event * amount_lost*returns[t-1]
        
        if our_event==1:
            print("weee, we got our event handled")
            extreme_events[t] = amount_lost
        t+=1
    return(market_time,long_term_avg_stake,b_value,r_t,default_prob_summed,returns,extreme_events)
   
        

        
        
              
        
def sim_returns_known_sent(market_time,long_term_avg_stake,starting_b_value = 0.02, low_bound_b = 0.02, upper_bound_b = 0.04, probability_of_default = np.ones(24*365*5), ltr_default_prob = 0.001 , starting_staking_rate = 0.02, r_t_sigma = 0.005 , max_prob = 0.6, sigma_b = 2 ):

    ### We make a function that will yield through the time and randomly generate sinus values.
    t = 0
    T = int(5*365*24)
    year = 365*24
    dt = 1/year
    market_time_t_vals = np.random.standard_t(1.33, size=T)
    
    ### Get the loss due to hacks/extreme events:
    extreme_events = np.zeros(T)
    
    ### Modeling the b value in Vasicek model, which in our case is a process itself. Similar to Chen model...
    difference_bounds = (upper_bound_b-low_bound_b)*0.5
    b_value = np.zeros(T)
    b_value[t] = starting_b_value + (1+long_term_avg_stake[t] ) * 0.01 * dt
    
    
    ### Cox–Ingersoll–Ross model for staking.
    ### b_value is the b value, a will be constant and we will have a significant volatility that will also depend on
    ### interest rate.
    ### getting the whole number of returns, starting with 1.
    R_T = np.ones(T)
    a = 3
    a1 = 3
    r_t = np.zeros(T)
    r_t_vol = np.random.standard_t(5, size=T)
    
    r_t[0] = starting_staking_rate
    dwb = np.random.normal(size=T)

    
    ### Probability of default right now is cca 1%. It will decrease over the time:
    default_prob = np.zeros(T)
    default_prob_summed = np.zeros(T)
    #our_x = np.log(probability_of_default - ltr_default_prob) ## possibility of logarithmic decay
    our_x = (probability_of_default - ltr_default_prob)
    our_x = our_x**-0.5
    ### We try to converge towards 0.1% probability of default.
    default_probabilities = np.zeros(T)
    def_vol = np.random.standard_t(2, size=T) ### We add some short term randomness to default probabilities.
    
    year = 1/(365*24)
    default_prob[0] = (probability_of_default - ltr_default_prob)
    default_prob_summed[0] = default_prob[0] + ltr_default_prob
    
    returns = np.ones(T)
    year = 1/(365*24)
    ### How did we get to this number and equation?
    #b_value[t] = b_value[t-1] + a1 * ((low_bound_b + difference_bounds*(1+long_term_avg_stake[t] )) - b_value[t-1]) * dt+ sigma_b * dwb[t] * np.sqrt(b_value[t-1]) * dt
    
    t += 1
    while t<T:
        b_value[t] = b_value[t-1] + a1 * ((low_bound_b + difference_bounds*(1+long_term_avg_stake[t] )) - b_value[t-1]) * dt+ sigma_b * dwb[t] * np.sqrt(b_value[t-1]) * dt
        #b_value[t] = low_bound_b + 0.01*(1+long_term_avg_stake[t] ) * difference_bounds
        r_t[t] =max( r_t[t-1] + a * (b_value[t] - r_t[t-1]) * dt + r_t_sigma * np.sqrt(r_t[t-1]) * r_t_vol[t]* dt,0) 
        default_prob[t] =  default_prob[t-1] - 2 * (t*year+our_x)**-3 *year + default_prob[t-1] * def_vol[t] * year
        default_prob_summed[t] = default_prob[t] + ltr_default_prob
        uniform_prob = np.random.uniform(0,1)
        our_event = catastrophic_event((default_prob_summed[t])*year) 
        amount_lost = loss_share(max_prob)
        returns[t] = returns[t-1] + returns[t-1] * r_t[t] * year - our_event * amount_lost*returns[t-1]
        if our_event==1:
            extreme_events[t] = amount_lost
        t+=1
    return(b_value,r_t,default_prob_summed,returns,extreme_events)
           
        
        
        
        
        
        
        
        
        
        
    