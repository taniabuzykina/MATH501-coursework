library(readr)
library(tidyverse)
library(class) #library for KNN
library(datasets)
data(iris3)

data_path <- "./data/churndata.txt"
churn_data <- read.csv(data_path, sep = " ")

attach(churn_data)
# table(churn)

set.seed(1) # to make the results reproducible
num_subset <- sample(length(churn), 350) # randomly choose 350 numbers out of 500

#* binding the attached variables of churn_data helps us get a matrix instead of dataframe
#* because KNN function doesn't like it when it's dataframe


X <- cbind(upload, webget, enqcount, callwait)
Y <- churn

train.X <- X[num_subset, ] # 350 records here
train.Y <- Y[num_subset]

test.X <- X[-num_subset, ] # 350 records here
test.Y <- Y[-num_subset]

# below is the normal KNN with a train set of train.X and a test set of test.X - no cross-validation
# k is selected as 3 just randomly honestly

normal.knn <- knn (train = train.X, test = test.X, cl = train.Y, k = 3)

# now moving on to error calculation:
# option 1: confusion table

table(normal.knn, test.Y)
confusion.table <- table(normal.knn, test.Y)
confusion.error <- (confusion.table[1,2] + confusion.table[2,1]) / sum(confusion.table)
confusion.error

# option 2: calculate it sligthly differently by finding the mean of wrongly recognized classifiers
# by actually comparing them as KNN returns the array of predicted classifiers

mean.error <- mean(normal.knn!= test.Y)
mean.error # the result is the same as confusion table error

leave.KNN <- function(K, train.X, train.Y){
  error <- 0
  n <- nrow(train.X)
  train.Y <- as.integer(train.Y == 'yes')
  # this function returns an error that is calculated as an average error of KNNs trained using leave-one-out
        for(i in 1:n){
          # subsetting the i-th row from the train predictors and classifiers and using them
          # as temporary training sets (without an i-th row)
          temp.train.X <- train.X[-i,]
          temp.train.Y <- train.Y[-i]

          # using an i-th row as a temporary test set
          temp.test.X <- train.X[i,]
          temp.test.Y <- train.Y[i]

          # the resulting KNN is tested on only 1 entry
          temp.knn <- knn(train = temp.train.X, test = temp.test.X, cl = temp.train.Y, k = K)

          # the error is being calculated on whether a test entry was classified wrongly or not
          # and accumulated as we'll need to find the mean error in the end
          error <- error + mean(temp.knn != temp.test.Y) #1 if the test entry was classified wrongly and 0 if correctly
        }

     return (error/n)
}

# leave.error <- leave.KNN(3, train.X, train.Y)
# leave.error
errors <- rep(0, 30) #trying with K from 1 to 30
for (j in 1:30) errors[j] <- leave.KNN(j, train.X, train.Y)

# plotting errors
plot(errors, xlab="K", ylab = "Test error")

