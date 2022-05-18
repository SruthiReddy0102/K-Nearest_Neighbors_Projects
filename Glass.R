# Output Variable = Type of a glass
# Input Variable = Other Factors

# Importing Dataset
Glass <- read.csv(file.choose())
View(Glass)


# Removing Uncessary columns
Glass  <- Glass[ , 2:10]
View(Glass)
attach(Glass)

# Reorder of columns
Glass  <- Glass[,c(9,1,2,3,4,5,6,7,8)]
View(Glass)

head(Glass) # Shows first 6 rows of the dataset
tail(Glass) # Showa last 6 rows of the dataset

# Checking of NA values
sum(is.na(Glass)) # No NA Values found

# Standardize the input columns
standard.features <- scale(Glass[,2:9])
Glass1 <- cbind(standard.features,Glass[1]) # Combine function used to combine the rows and columns
View(Glass1)
?cbind

# Reorder of columns
Glass  <- Glass1[,c(9,1,2,3,4,5,6,7,8)]
View(Glass)

# Exploratory Data Analysis

# table of diagnosis
table(Glass$Type)

str(Glass$Type)

# table or proportions with more informative labels
round(prop.table(table(Glass$Type)) * 100, digits = 2)


install.packages("Hmisc")
library(Hmisc)
describe(Glass)

install.packages("lattice") # Highly used for data visualization
library("lattice") # dotplot is part of lattice package

# Graphical exploration
dotplot(Glass$Na, main = "Dot Plot of Sodium")
dotplot(Glass$Mg, main = "Dot Plot of Magnesium")
dotplot(Glass$Si, main = "Dot Plot of Silicon")
dotplot(Glass$K, main = "Dot Plot of Potassium")
dotplot(Glass$Ca, main = "Dot Plot of Calcium ")
dotplot(Glass$Ba, main = "Dot Plot of Barium")
dotplot(Glass$Fe, main = "Dot Plot of Iron")


#Boxplot Representation

boxplot(Glass$Na, col = "dodgerblue4" , main = "Sodium")
boxplot(Glass$Mg, col = "dodgerblue4" , main = "Magnesium")
boxplot(Glass$Si, col = "dodgerblue4" , main = "Silicon")
boxplot(Glass$K, col = "dodgerblue4", main = "Potassium")
boxplot(Glass$Ca, col = "dodgerblue4" , main = "Calcium")
boxplot(Glass$Ba, col = "dodgerblue4", main = "Barium")
boxplot(Glass$Fe, col = "dodgerblue4" , main = "Iron")


#Histogram Representation

hist(Glass$Na, col = "dodgerblue4" , main = "Sodium")
hist(Glass$Mg, col = "dodgerblue4" , main = "Magnesium")
hist(Glass$Si, col = "dodgerblue4" , main = "Silicon")
hist(Glass$K, col = "dodgerblue4", main = "Potassium")
hist(Glass$Ca, col = "dodgerblue4" , main = "Calcium")
hist(Glass$Ba, col = "dodgerblue4", main = "Barium")
hist(Glass$Fe, col = "dodgerblue4" , main = "Iron")

 
# Normal QQ plot
qqnorm(Glass$Na , main = "Sodium")
qqline(Glass$Na , main = "Sodium")

qqnorm(Glass$Mg , main = "Magnesium")
qqline(Glass$Mg , main = "Magnesium")

qqnorm(Glass$Si , main = "Silicon")
qqline(Glass$Si , main = "Silicon")

qqnorm(Glass$K , main = "Potassium")
qqline(Glass$K , main = "Potassium")

qqnorm(Glass$Ca , main = "Calcium")
qqline(Glass$Ca , main = "Calcium")

qqnorm(Glass$Ba , main = "Barium")
qqline(Glass$Ba , main = "Barium")

qqnorm(Glass$Fe , main = "Iron")
qqline(Glass$Fe , main = "Iron")


#Scatter plot for all pairs of variables
plot(Glass)

# correlation matrix
cor(Glass)

# Data Partitioning
n <- nrow(Glass)
n1 <- n * 0.7
n2 <- n - n1
train_index <- sample(1:n,n1)
train <- Glass[train_index,]
test <- Glass[-train_index,]

# create labels for training and test data

Glass_train_labels <- Glass[train_index, 1]
Glass_test_labels <- Glass[-train_index, 1]

#---- Training a model on the data ----

# load the "class" library
install.packages("class")
library(class)

Glass_test_pred <- knn(train = train, test = test,
                      cl = Glass_train_labels, k = 9)


## ---- Evaluating model performance ---- ##
confusion_test <- table(x = Glass_test_labels, y = Glass_test_pred)
confusion_test

Accuracy <- sum(diag(confusion_test))/sum(confusion_test)
Accuracy # 0.9692308

# Training Accuracy to compare against test accuracy
Glass_train_pred <- knn(train = train, test = train, cl = Glass_train_labels, k=9)

confusion_train <- table(x = Glass_train_labels, y = Glass_train_pred)

confusion_train

Accuracy_train <- sum(diag(confusion_train))/sum(confusion_train)
Accuracy_train # 0.8993289


## Improving model performance ----

# use the scale() function to z-score standardize a data frame
Glass_z <- as.data.frame(scale(Glass[-1]))

# confirm that the transformation was applied correctly
summary(Glass_z$area_mean)

# create training and test datasets

Glass_train <- Glass_z[train_index, 1]

Glass_test <- Glass_z[-train_index, 1]

# re-classify test cases
Glass_test_pred <- knn(train = train, test = test,
                       cl = Glass_train_labels, k=9)

confusion_train <- table(x = Glass_train_labels, y = Glass_train_pred)

confusion_train

Accuracy_train <- sum(diag(confusion_train))/sum(confusion_train)
Accuracy_train

# Create the cross tabulation of predicted vs. actual

install.packages("gmodels")
library(gmodels)
CrossTable(x = Glass_test_labels, y = Glass_test_pred, prop.chisq=FALSE)


# try several different values of k
train <- Glass[train_index,]
test <- Glass[-train_index,]

Glass_test_pred <- knn(train = train, test = test, cl = Glass_train_labels, k=1)
CrossTable(x = Glass_test_labels, y = Glass_test_pred, prop.chisq=FALSE)

Glass_test_pred <- knn(train = train, test = test, cl = Glass_train_labels, k=5)
CrossTable(x = Glass_test_labels, y = Glass_test_pred, prop.chisq=FALSE)

Glass_test_pred <- knn(train = train, test = test, cl = Glass_train_labels, k=7)
CrossTable(x = Glass_test_labels, y = Glass_test_pred, prop.chisq=FALSE)

Glass_test_pred <- knn(train = train, test = test, cl = Glass_train_labels, k=11)
CrossTable(x = Glass_test_labels, y = Glass_test_pred, prop.chisq=FALSE)

Glass_test_pred <- knn(train = train, test = test, cl = Glass_train_labels, k=15)
CrossTable(x = Glass_test_labels, y = Glass_test_pred, prop.chisq=FALSE)

Glass_test_pred <- knn(train = train, test = test, cl = Glass_train_labels, k=18)
CrossTable(x = Glass_test_labels, y = Glass_test_pred, prop.chisq=FALSE)



########################################################
pred.train <- NULL
pred.val <- NULL
error_rate.train <- NULL
error_rate.val <- NULL
accu_rate.train <- NULL
accu_rate.val <- NULL
accu.diff <- NULL
error.diff <- NULL

for (i in 1:20) {
  pred.train <- knn(train = train, test = train, cl = Glass_train_labels, k = i)
  pred.val <- knn(train = train, test = test, cl = Glass_train_labels, k = i)
  error_rate.train[i] <- mean(pred.train!=Glass_train_labels)
  error_rate.val[i] <- mean(pred.val != Glass_test_labels)
  accu_rate.train[i] <- mean(pred.train == Glass_train_labels)
  accu_rate.val[i] <- mean(pred.val == Glass_test_labels)  
  accu.diff[i] = accu_rate.train[i] - accu_rate.val[i]
  error.diff[i] = error_rate.val[i] - error_rate.train[i]
}

knn.error <- as.data.frame(cbind(k = 1:20, error.train = error_rate.train, error.val = error_rate.val, error.diff = error.diff))
knn.accu <- as.data.frame(cbind(k = 1:20, accu.train = accu_rate.train, accu.val = accu_rate.val, accu.diff = accu.diff))

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



