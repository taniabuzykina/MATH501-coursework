# Machine Learning Part (a)∗∗: Present the data visually using box-and-whisker plots with
# a distinction for churn. Comment on the data in the context of the problem.
library(readr)

data_path <- "./data/churndata.txt"

churn_data <- read.csv(data_path, sep = " ")

#table(churn_data$churn)

###FANCY WAY####
#making variables (columns of the table) visible in the workspace so that we 
#can directly address them

attach(churn_data)

table(churn)


# Machine Learning Part (b)∗
# : Create a training set consisting of 350 randomly chosen
# data points and a test set consisting of the remaining 150 data points.
# Machine Learning Part (c)∗∗∗: Using the training data set apply the K nearest neighbours
# method to construct a classifier to predict churn based on the four available predictors. Find
# the optimal K using leave-one-out cross-validation for the training data set.
# Calculate the test error for the classification rule obtained for the optimal K.
