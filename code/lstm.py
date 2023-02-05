# -*- coding: utf-8 -*-

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, LSTM, Dropout
import tensorflow as tf

gpus = tf.config.experimental.list_physical_devices('GPU')
tf.device("/gpu:0")

#open file
data = open("/content/data.csv", "r")
data = pd.read_csv(data, header=0)
seq_data = pd.DataFrame(data = data[['div_solar', 'temp', 'sunshine','insolation','cloud_amount','cloud_level','cloud_ceiling']])

#check data correlation (default = pearson)
data.corr()

#use daytime data
seq_data = seq_data[seq_data['div_solar']>0]

# Min-Max Normalization

from sklearn.preprocessing import MinMaxScaler
scaler = MinMaxScaler()
seq_data[:] = scaler.fit_transform(seq_data[:])

# make seq dataset function
def seq2dataset(seq, window, horizon):
    X = []; Y = []
    
    # repeat => total data length - (7+1) + 1 
    for i in range(len(seq)-(window + horizon)+1):
        x = seq[i:(i+window)]
        y = (seq[i+window+horizon-1])
        X.append(x)
        Y.append(y)
    return np.array(X), np.array(Y)

# set horizon & window
seq_data = seq_data.to_numpy()
X_data,Y_data = seq2dataset(seq_data, 12, 1)

# divide training set
split = int(len(X_data)*0.9)
x_train = X_data[0:split]
y_train = Y_data[0:split]
x_test = X_data[split:]
y_test = Y_data[split:]

# set epoch & batch
epo = 60
batchs = 10

# LSTM model training 
model = Sequential()
model.add(LSTM(units=10, input_shape=x_train[0].shape))
model.add(Dense(7))
model.compile(loss='mae', optimizer= 'adam', metrics=['mae'])
hist_spring = model.fit(x_train, y_train, epochs=epo,
                 batch_size=batchs, validation_data=(x_test, y_test), verbose=2)

# LSTM model evaluation
ev=model.evaluate(x_test, y_test, verbose=0)
print("loss function : ", ev[0], "MAE : ", ev[1])

#LSTM model prediction
pred = model.predict(x_test)
steady = x_test[:,-1]
trendy = x_test[:,-1]+(x_test[:,-1]-x_test[:,-2])

# visualizatoin
x_range = range(len(y_test))
plt.figure(figsize=(18,12))
pred_solar=pred[:,0]
y_test_solar=y_test[:,0]
plt.plot(pred_solar, color='blue', label='predict')
plt.plot(y_test_solar, color ='red' ,label='solar')

# plt.legend(['predict', 'solar'],loc='best')
plt.grid()
plt.show()