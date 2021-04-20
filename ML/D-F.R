# Title     : Questions d-f
# Created by: tania
# Created on: 20/04/2021

library(readr)
library(tidyverse)
library(tree)
library(randomForest)

#* Machine Learning Part (d)∗∗:
#* Using the training data set apply the random forest (bagging) method to
#* construct a classifier to predict churn based on the four available predictors.
data_path <- "./data/churndata.txt"
churn_data <- read.csv(data_path, sep = " ")
churn_data <- na.exclude(churn_data)

set.seed(4)
rf.tree <- randomForest(Y ~ ., data = X, subset = train.X, mtry = 2)

#* Using the obtained random forest, comment on the importance of the four
#* variables for predicting churn. Calculate the test error for the obtained
#* random forest. Compare it to the test error found for the KNN classifier and
#* provide an appropriate comment.









data_path <- "./data/churndata.txt"
churn_data <- read.csv(data_path, sep = " ")
churn_data <- na.exclude(churn_data)