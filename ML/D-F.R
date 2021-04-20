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

# random sequence to subset from the predictors and classifiers dataframes

set.seed(1) # to make the results reproducible
num_subset <- sample(nrow(churn_data), 350) # randomly choose 350 numbers out of 500

testing.X <- churn_data[-num_subset, ] %>% subset(select = -churn)
testing.Y <- churn_data[-num_subset, ] %>% subset(select = churn)
testing.Y <- unlist(testing.Y)

random.tree <- randomForest(churn ~ ., data = churn_data, subset = num_subset, mtry = 2, keep.forest=FALSE,
                            importance = TRUE)

# ANOTHER OPTION - USING TRAINING SET FROM THE BEGINNING without subsetting
# rf.tree = randomForest(churn ~ ., data = training, mtry = 2)

# *****
#* Using the obtained random forest, comment on the importance of the four
#* variables for predicting churn. 
# *****

# getTree(random.tree, k = 500, labelVar = TRUE) - will not work now because we removed our forest LOL
varImpPlot(random.tree, main = "Random Forrest Variable Importance")

#* COMMENTARY

#* From both MeanDecreaseAccuracy and MeanDecreaseGini measures we conclude 
#* that 'webget' is one-sidedly the most important variable. The least important
#* variable turned out to be 'callwait' followed by 'upload'. 'enqcount' (the
#* number of customers enquiry calls to an operator) is the second most important
#* predictor.


# *****
#* Calculate the test error for the obtained
#* random forest. Compare it to the test error found for the KNN classifier and
#* provide an appropriate comment.
# *****

rf.predict <- predict(random.tree, testing.X, type = "class")
tab <- table(rf.predict, testing.Y)
tab

error <- (tab[1,2] + tab[2,1]) / sum(tab)
error
