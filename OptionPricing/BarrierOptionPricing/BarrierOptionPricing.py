import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import scipy.stats as st

nmdt=pd.Series(np.random.standard_normal(10000))

nmdt.plot(kind='hist',bins=20)
plt.show()

def brownian_bridge(t,steps):
    dt = t/steps

    sigma=np.repeat(dt,steps+1)
    sigma