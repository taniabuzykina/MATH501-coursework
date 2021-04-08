# Machine Learning Part (a)∗∗: Present the data visually using box-and-whisker plots with
# a distinction for churn. Comment on the data in the context of the problem.
library(readr)
library(ggplot2)
library(tidyverse)

#reading the data into a dataframe
data_path <- "./data/churndata.txt"
churn_data <- read.csv(data_path, sep = " ")

#table(churn_data$churn)

###FANCY WAY
#making variables (columns of the table) visible in the workspace so that we 
#can directly address them instead of 'churn_data$churn' we now can directly
#type 'churn'

attach(churn_data)
table(churn)

# 1 boxplot with average speed of upload against an indicator whether a customer 
# switched to a different provider
churn_data %>% ggplot() + 
 geom_boxplot(aes(x = churn, y = upload, color = churn)) +
 labs (y = "Speed of upload", 
       x = "Switched to a new provider",
       color = "Switched")

#COMMENTARY:

# As we can see the customers who have not yet switched to a different operator have 
# lower average speed of upload in the internet in comparison with the customers
# who have switched. In general, having higher uplink speed is a plus since it 
# improves the Internet phone/video call experience and it does no harm to the customers
# Hence increasing uplink speed could not affect the customers' decision to switch to 
# different operators

# 2 boxplot with the mean time to load a webpage against an indicator whether a customer 
# switched to a different provider

churn_data %>% ggplot() + 
 geom_boxplot(aes(x = churn, y = webget, color = churn)) +
 labs (y = "Average time to load a webpage", 
       x = "Switched to a new provider",
       color = "Switched")

# COMMENTARY
# We can observe a strong dependency between the time to load a webpage 
# (which directly corresponds to the downlink speed) and an indicator whether customers
# changed their operators. The average downlink speed was significantly lower for 
# the customers that have switched to a different provider than for those who haven't.
# We can conclude this as the average time to load a webpage for those clients who
# switched is nearly 200 units longer than of those who didn't.  

# 3 boxplot with how long a customer waited on the phone call for a customer 
# service operator against an indicator whether a customer 
# switched to a different provider

churn_data %>% ggplot() + 
 geom_boxplot(aes(x = churn, y = callwait, color = churn)) +
 labs (y = "Customer service waiting time", 
       x = "Switched to a new provider",
       color = "Switched")


# Machine Learning Part (b)∗
# : Create a training set consisting of 350 randomly chosen
# data points and a test set consisting of the remaining 150 data points.

# set.seed(1) # to make the results reproducible
# def.subset <- sample(10000, 5000) # we randomly choose 5000 numbers out of 10000
# train.X.sub <- train.X[-def.subset, ] # predictors for the training set
# cl.sub <- cl[-def.subset] # classes for the training set
# test.X <- train.X[def.subset, ] # predictors for the test set
# test.cl <- cl[def.subset]

# Machine Learning Part (c)∗∗∗: Using the training data set apply the K nearest neighbours
# method to construct a classifier to predict churn based on the four available predictors. Find
# the optimal K using leave-one-out cross-validation for the training data set.
# Calculate the test error for the classification rule obtained for the optimal K.
