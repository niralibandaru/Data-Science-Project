import numpy as np
import matplotlib.pyplot as plt
from scipy import stats
import pandas as pd
import seaborn as sns; sns.set()
from sklearn.datasets.samples_generator import make_blobs
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import MinMaxScaler
from sklearn.svm import SVR
from sklearn.model_selection import KFold



data = pd.read_csv("C:/DS/df.csv")
data.head()
mydata = pd.DataFrame(data)
kf = KFold(n_splits = 10)

kf.get_n_splits(mydata)

X = mydata.iloc[:, [0,8]]
y = mydata.iloc[:, 9]

#Noramlizing the dataset by applying minmax scaling

scaler = MinMaxScaler(feature_range=(0, 1))
X = scaler.fit_transform(X)

#Noramlizing the dataset by applying minmax scaling
scores = []
scaler = MinMaxScaler(feature_range=(0, 1))
X = scaler.fit_transform(X)

svr = SVR(kernel='rbf')
cv = KFold(n_splits=10, random_state=100, shuffle=False)
for train_index, test_index in cv.split(X):
    print("Train Index: ", train_index, "\n")
    print("Test Index: ", test_index)
    X_train, X_test, y_train, y_test = X[train_index], X[test_index], y[train_index], y[test_index]
    svr.fit(X_train, y_train)
    scores.append(svr.score(X_test, y_test))
    
    
