

import numpy as np
from scipy.stats import *
import scipy.stats as stats    

def simulate_index(our_T=24*10*365,N=10000)
    ### Our simulated results:
    simulate = stats.dweibull.rvs(1.3222055880421635, 0.001986686906958109, 0.014177981934773431, size=[our_T,N])    
    simulate[0,:] = 1 + simulate[0,:]
    i = 1
    while i<len(simulate[:,0]):
        simulate[i,:] = simulate[i-1,:] * (1 + simulate[i,:])
        i+=1
    ### We return the simulated results:    
    return(simulate)


