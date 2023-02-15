# -*- coding: utf-8 -*-

import warnings
import itertools
import os
import pandas as pd
import numpy as np
import statsmodels.api as sm
import matplotlib
matplotlib.use('agg')
import matplotlib.pyplot as plt
from statsmodels.tsa.statespace.sarimax import SARIMAX

from numba import jit, cuda 

warnings.filterwarnings("ignore") # specify to ignore warning messages
# use GPU
# os.environ["CUDA_VISIBLE_DEVICES"] = "1"

data = pd.read_csv('/content/data.csv', engine='python')

data = data[['temp',	'rain',	'sunshine',	'insolation',	'solar',	'div_solar',	'cloud_amount',	'cloud_level',	'cloud_ceiling',	'snow']]

data = data[data['div_solar']>0]

from sklearn.preprocessing import MinMaxScaler
scaler = MinMaxScaler()
data[:] = scaler.fit_transform(data[:])

# _data arimax
mod_data_sarimax = sm.tsa.statespace.SARIMAX(data['div_solar'],
                                order=(3,0,3),
                                seasonal_order=(2,0,2,24),
                                exog=data[['temp', 'insolation', 'sunshine']])

result_data_arimax = mod_data_sarimax.fit()

result_data_arimax.summary()

result_data_arimax.fittedvalues

import pandas as pd
import numpy as np

def smape(act,forc):
    return 100/len(act) * np.sum(2 * np.abs(forc - act) / (np.abs(act) + np.abs(forc)))

import matplotlib.pyplot as plt

all_pred_data = result_data_arimax.fittedvalues
data_day_data = data['div_solar'][int(len(data['div_solar'])*0.8):len(data['div_solar'])]
pred_day_data = all_pred_data[int(len(result_data_arimax.fittedvalues)*0.8):len(result_data_arimax.fittedvalues)]


plt.figure(figsize=(18,12))
plt.plot(pred_day_data, color = 'blue')
plt.plot(data_day_data, color = 'red')

plt.show()

fore = result_data_arimax.forecast(steps=24)
print(fore)

result_data_arimax.plot_predict()