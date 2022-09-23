
from simulate_returns import * 
import numpy as np
import matplotlib.pyplot as plt
import math
from scipy.stats import *
import scipy.stats as stats    
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
        current_return = stats.gennorm.rvs(1.3017688944349377, 0.00298659, 0.02069869963423673, size=1)
        simulate[i,timestamp] = 1 + current_return
        current_return = current_return/(31*24*3)
        price[i,0] = 1 + current_return
        while t<T:
            
            if t%(31*24*3)==0:
                timestamp += 1
                current_return = stats.gennorm.rvs(1.3017688944349377,0.00298659, 0.02069869963423673, size=1)[0] 
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

### We define the network growth:
def network_growth(market_sentiment,staking_rate_other,deposit_rate,CAR,usage_growth,stakers_share,index_appreciation, starting_network_lower = 2 * 10**7, starting_network_upper = 3 * 10**7):
    T=24*365*5
    dt = 1/(365*24)
    
    network_growth = np.zeros(T)
    network_growth[0] = np.random.uniform(starting_network_lower,starting_network_upper) ### Random number
    #indx_growth = 0#0.01 ### How much the index will grow on a yearly basis.
    indx_growth = np.zeros(T)
    indx_growth[0] = sum(index_appreciation[0:31*24*3])### we calculate the next 3 months of index appreciation.
    indx_factor = 1.2
    CAR_ratio = 1.5
    Cmomentum = 2.6
    deposit_profit = 5
    lastmonth = np.zeros(T)
    lastmonth[0] = 0
    mkt_constant = .55 ### We change this one because we are more pessimistic about the market growth going forward.
    ### We believe users will care more about the stability of the project than overal market hype as regulations kick in.
    ### There will also always be a competition and we will have to differ from others. Of course in case of very bad market conditions we then expect also smaller downturn, but we believe this is also a positive outcome of more regulation.
    #deposit_rate = np.ones(N) * 0.04
    c_usage_growth = 5.2
    sigma_ngrowth = 1
    growth_vol = np.random.standard_t(5, size=T)
    
    
    t=1
    while t<T:
        indx_growth[t] = sum(index_appreciation[t:(31*24*3+t)])
        if t>=(30*24+1):
            lastmonth[t] = (network_growth[t-1] - network_growth[t-30*24-1])/network_growth[t-30*24-1]
        market_growth = (market_sentiment[t])**(-(np.sign((market_sentiment[t] - market_sentiment[t-1]))-1)/2) * (abs(market_sentiment[t]+1))**((np.sign((market_sentiment[t] - market_sentiment[t-1]))+1)/2)  * mkt_constant  
        growth_vol[t] * sigma_ngrowth * dt * network_growth[t-1] 
        dnetwork = indx_growth[t] * indx_factor * network_growth[t-1] * dt + ((CAR - 1)/CAR) * CAR_ratio * network_growth[t-1] * dt +lastmonth[t] * Cmomentum * network_growth[t-1] * dt + market_growth* network_growth[t-1] * dt + (deposit_rate[t] + indx_growth[t] - staking_rate_other[t] ) * deposit_profit * network_growth[t-1] * dt + usage_growth[t] * c_usage_growth * network_growth[t-1] * dt + volatility

        network_growth[t] = network_growth[t-1] + dnetwork  * (1 - abs((np.sign(dnetwork)-1)/2) * stakers_share[t] ) 

        t+=1
    return(network_growth)