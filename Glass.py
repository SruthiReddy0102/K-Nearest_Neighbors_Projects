import pandas as pd
import numpy as np

Glass = pd.read_csv("C:/Users/personal/Desktop/glass.csv")

# Removing unwanted columns
Glass = Glass.iloc[:, 1:11] 

# Reodering the variable columns
Glass = Glass.iloc[:, [8,0,1, 2, 3,4,5,6,7]]
Glass.columns

# Normalization function 
def norm_func(i):
    x = (i-i.min())	/ (i.max()-i.min())
    return (x)

# Normalized data frame (considering the numerical part of data)
Glass_n = norm_func(Glass.iloc[:, 1:])
Glass_n.describe()

X = np.array(Glass_n.iloc[:,:]) # Predictors 
Y = np.array(Glass['Type']) # Target 

# Model Building

from sklearn.model_selection import train_test_split

X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size = 0.3)

from sklearn.neighbors import KNeighborsClassifier

knn = KNeighborsClassifier(n_neighbors = 5)
knn.fit(X_train, Y_train)

pred = knn.predict(X_test)
pred

# Evaluate the model
from sklearn.metrics import accuracy_score
print(accuracy_score(Y_test, pred))
pd.crosstab(Y_test, pred, rownames = ['Actual'], colnames= ['Predictions']) 
# 0.63 with k = 8
# 0.63 with k = 21
# 0.72 with k = 5

# error on train data
pred_train = knn.predict(X_train)
print(accuracy_score(Y_train, pred_train))
pd.crosstab(Y_train, pred_train, rownames=['Actual'], colnames = ['Predictions']) 
# 0.711 with k = 8
# 0.63 with k = 21
# 0.73 k = 5


# creating empty list variable 
acc = []

# running KNN algorithm for 3 to 50 nearest neighbours(odd numbers) and 
# storing the accuracy values

for i in range(1,30,2):
    neigh = KNeighborsClassifier(n_neighbors=i)
    neigh.fit(X_train, Y_train)
    train_acc = np.mean(neigh.predict(X_train) == Y_train)
    test_acc = np.mean(neigh.predict(X_test) == Y_test)
    acc.append([train_acc, test_acc])


import matplotlib.pyplot as plt # library to do visualizations 

# train accuracy plot 
plt.plot(np.arange(1,30,2),[i[0] for i in acc],"ro-")

# test accuracy plot
plt.plot(np.arange(1,30,2),[i[1] for i in acc],"bo-")

# Conclusion
# The k value at 5 gives the test and train accuarcy as 0.73 which is also a Right fit model
