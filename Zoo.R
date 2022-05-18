# Output Variable = Classification/type of a animal
# Input Variable = Other Factors

# Importing Dataset
Zoo <- read.csv(file.choose())
View(Zoo)


# Removing Uncessary columns
Zoo  <- Zoo[ , 2:18]
View(Zoo)
attach(Zoo)

# Reorder of columns
Zoo  <- Zoo[,c(17,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16)]
View(Zoo)

head(Zoo) # Shows first 6 rows of the dataset
tail(Zoo) # Showa last 6 rows of the dataset

# Checking of NA values
sum(is.na(Zoo)) # No NA Values found


# Exploratory Data Analysis

# table of diagnosis
table(Zoo$type)

str(Zoo$type)

# table or proportions with more informative labels
round(prop.table(table(Zoo$type)) * 100, digits = 2)


install.packages("Hmisc")
library(Hmisc)
describe(Zoo)

install.packages("lattice") # Highly used for data visualization
library("lattice") # dotplot is part of lattice package


# Graphical exploration
dotplot(Zoo$hair, main = "Dot Plot of Hair")
dotplot(Zoo$eggs, main = "Dot Plot of Eggs")
dotplot(Zoo$legs, main = "Dot Plot of Legs")
dotplot(Zoo$feathers, main = "Dot Plot of Feathers")


#Boxplot Representation

boxplot(Zoo$hair, col = "dodgerblue4" , main = "Hair")
boxplot(Zoo$feathers, col = "dodgerblue4" , main = "Feathers")
boxplot(Zoo$legs, col = "dodgerblue4" , main = "Legs")

#Histogram Representation

hist(Zoo$hair, col = "dodgerblue4" , main = "Hair")
hist(Zoo$feathers, col = "dodgerblue4" , main = "Feathers")
hist(Zoo$legs, col = "dodgerblue4" , main = "Legs")

#Scatter plot for all pairs of variables
plot(Zoo)

# correlation matrix
cor(Zoo)

# confirm that normalization worked
summary(Zoo$area_mean)

# Data Partitioning
n <- nrow(Zoo)
n1 <- n * 0.7
n2 <- n - n1
train_index <- sample(1:n,n1)
train <- Zoo[train_index,]
test <- Zoo[-train_index,]

# create labels for training and test data

Zoo_train_labels <- Zoo[train_index, 1]

Zoo_test_labels <- Zoo[-train_index, 1]

#---- Training a model on the data ----

# load the "class" library
install.packages("class")
library(class)


Zoo_test_pred <- knn(train = train, test = test,
                       cl = Zoo_train_labels, k = 6)


## ---- Evaluating model performance ---- ##
confusion_test <- table(x = Zoo_test_labels, y = Zoo_test_pred)
confusion_test

Accuracy <- sum(diag(confusion_test))/sum(confusion_test)
Accuracy # 0.8387097

# Training Accuracy to compare against test accuracy
Zoo_train_pred <- knn(train = train, test = train, cl = Zoo_train_labels, k=6)

confusion_train <- table(x = Zoo_train_labels, y = Zoo_train_pred)

confusion_train

Accuracy_train <- sum(diag(confusion_train))/sum(confusion_train)
Accuracy_train


# Create the cross tabulation of predicted vs. actual

install.packages("gmodels")
library(gmodels)
CrossTable(x = Zoo_test_labels, y = Zoo_test_pred, prop.chisq=FALSE)


# try several different values of k
train <- Zoo[train_index,]
test <- Zoo[-train_index,]

Animals_test_pred <- knn(train = train, test = test, cl = Zoo_train_labels, k=1)
CrossTable(x = Zoo_test_labels, y = Zoo_test_pred, prop.chisq=FALSE) # 1

Zoo_test_pred <- knn(train = train, test = test, cl = Zoo_train_labels, k=3)
CrossTable(x = Zoo_test_labels, y = Zoo_test_pred, prop.chisq=FALSE) # 0.968

Zoo_test_pred <- knn(train = train, test = test, cl = Zoo_train_labels, k=9)
CrossTable(x = Zoo_test_labels, y = Zoo_test_pred, prop.chisq=FALSE)

Zoo_test_pred <- knn(train = train, test = test, cl = Zoo_train_labels, k=11)
CrossTable(x = Zoo_test_labels, y = Zoo_test_pred, prop.chisq=FALSE)

Zoo_test_pred <- knn(train = train, test = test, cl = Zoo_train_labels, k=15)
CrossTable(x = Zoo_test_labels, y = Zoo_test_pred, prop.chisq=FALSE)




########################################################
pred.train <- NULL
pred.val <- NULL
error_rate.train <- NULL
error_rate.val <- NULL
accu_rate.train <- NULL
accu_rate.val <- NULL
accu.diff <- NULL
error.diff <- NULL

for (i in 1:25) {
  pred.train <- knn(train = train, test = train, cl = Zoo_train_labels, k = i)
  pred.val <- knn(train = train, test = test, cl = Zoo_train_labels, k = i)
  error_rate.train[i] <- mean(pred.train!=Zoo_train_labels)
  error_rate.val[i] <- mean(pred.val != Zoo_test_labels)
  accu_rate.train[i] <- mean(pred.train == Zoo_train_labels)
  accu_rate.val[i] <- mean(pred.val == Zoo_test_labels)  
  accu.diff[i] = accu_rate.train[i] - accu_rate.val[i]
  error.diff[i] = error_rate.val[i] - error_rate.train[i]
}

knn.error <- as.data.frame(cbind(k = 1:25, error.train = error_rate.train, error.val = error_rate.val, error.diff = error.diff))
knn.accu <- as.data.frame(cbind(k = 1:25, accu.train = accu_rate.train, accu.val = accu_rate.val, accu.diff = accu.diff))

library(ggplot2)
errorPlot = ggplot() + 
  geom_line(data = knn.error[, -c(3,4)], aes(x = k, y = error.train), color = "blue") +
  geom_line(data = knn.error[, -c(2,4)], aes(x = k, y = error.val), color = "red") +
  geom_line(data = knn.error[, -c(2,3)], aes(x = k, y = error.diff), color = "black") +
  xlab('knn') +
  ylab('ErrorRate')
accuPlot = ggplot() + 
  geom_line(data = knn.accu[,-c(3,4)], aes(x = k, y = accu.train), color = "blue") +
  geom_line(data = knn.accu[,-c(2,4)], aes(x = k, y = accu.val), color = "red") +
  geom_line(data = knn.accu[,-c(2,3)], aes(x = k, y = accu.diff), color = "black") +
  xlab('knn') +
  ylab('AccuracyRate')

# Plot for Accuracy
plot(knn.accu[, c(4)], type = "b", xlab = "K-Value", ylab = "DifferenceInAccu") 

# Plot for Error
plot(knn.error[, c(4)], type = "b", xlab = "K-Value", ylab = "DifferenceInError") 



