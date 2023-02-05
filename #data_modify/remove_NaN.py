
import pandas as pd

data = pd.read_csv('./all_data_modify.csv')

colums = data.columns

for i in colums : 
    data[i] = data[i].fillna(method='ffill')

f_data = pd.DataFrame(data)
f_data.to_csv('data_preprocess.csv', index=False)