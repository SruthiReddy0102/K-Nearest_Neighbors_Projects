import pandas as pd
import numpy as np

Zoo = pd.read_csv("C:/Users/personal/Desktop/Zoo.csv")

# Removing unwanted columns
Zoo = Zoo.iloc[:, 1:19] 

# Reodering the variable columns
Zoo = Zoo.iloc[:, [16,0,1, 2, 3,4,5,6,7,8,9,10,11,12,13,14,15]]
Zoo.columns

# Normalization function 
def norm_func(i):
    x = (i-i.min())	/ (i.max()-i.min())
    return (x)

# Normalized data frame (considering the numerical part of data)
Zoo_n = norm_func(Zoo.iloc[:, 1:])
Zoo_n.describe()

X = np.array(Zoo_n.iloc[:,:]) # Predictors 
Y = np.array(Zoo['type']) # Target 

# Model Building

from sklearn.model_selection import train_test_split

X_train, X_test, Y_train, Y_test = train_test_split(X, Y, test_size = 0.3)

from sklearn.neighbors import KNeighborsClassifier

knn = KNeighborsClassifier(n_neighbors = 9)
knn.fit(X_train, Y_train)

pred = knn.predict(X_test)
pred

# Evaluate the model
from sklearn.metrics import accuracy_score
print(accuracy_score(Y_test, pred))
pd.crosstab(Y_test, pred, rownames = ['Actual'], colnames= ['Predictions']) 
# 0.871 with k = 7
# 0.64 with k = 25
# 0.74 with k = 17
# 0.87 with k = 9

# error on train data
pred_train = knn.predict(X_train)
print(accuracy_score(Y_train, pred_train))
pd.crosstab(Y_train, pred_train, rownames=['Actual'], colnames = ['Predictions']) 
# 0.94 with k = 5
# 0.77 with k = 25
# 0.84 with k = 17
# 0.91 with k = 9


# creating empty list variable 
acc = []

# running KNN algorithm for 3 to 50 nearest neighbours(odd numbers) and 
# storing the accuracy values

for i in range(1,10,1):
    neigh = KNeighborsClassifier(n_neighbors=i)
    neigh.fit(X_train, Y_train)
    train_acc = np.mean(neigh.predict(X_train) == Y_train)
    test_acc = np.mean(neigh.predict(X_test) == Y_test)
    acc.append([train_acc, test_acc])


import matplotlib.pyplot as plt # library to do visualizations 

# train accuracy plot 
plt.plot(np.arange(1,10,1),[i[0] for i in acc],"ro-")

# test accuracy plot
plt.plot(np.arange(1,10,1),[i[1] for i in acc],"bo-")

# Conclusion
# The k value at 9 gives the test and train accuarcy as 0.87 and 0.94 which is also a Right fit model
