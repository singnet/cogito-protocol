import json
import numpy as np

def put_numpy_seed_in_json_dic(results):
    (rnd0,rnd1,rnd2,rnd3,rnd4) = np.random.get_state()
    rnd1 = [int(number) for number in rnd1]
    rand_seed = (rnd0,rnd1,rnd2,rnd3,rnd4)
    results['rand_seed'] = rand_seed
    return results

def get_numpy_seed(results):
    (rnd0,rnd1,rnd2,rnd3,rnd4) = results['rand_seed']
    rnd1 = [np.uint32(number) for number in rnd1]
    rand_seed = (rnd0,rnd1,rnd2,rnd3,rnd4)
    return rand_seed