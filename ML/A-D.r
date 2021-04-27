#---- 

# Machine Learning Part (a)∗∗: Present the data visually using box-and-whisker plots with
# a distinction for churn. Comment on the data in the context of the problem.
library(readr)
library(ggplot2)
library(tidyverse)
library(class) #library for KNN
library(GGally) # library for PCA
library(randomForest)


#reading the data into a dataframe
data_path <- "./data/churndata.txt"
churn_data <- read.csv(data_path, sep = " ")
churn_data <- na.exclude(churn_data)
churn_data$churn <- as.factor(churn_data$churn)
attach(churn_data)


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

# COMMENTARY

# Even though the average waiting time for a customer service operator is
# similar in both cases, overall the majority of of customers who switched to a
# different operator had to wait longer than the average and the customers who
# haven't changed their provider. We can assume that the time spent by a
# customer on a call while they're waiting for a customer service operator to
# attend may impact their decision to switch to another operator although the
# influence seems to be less significant compared to time to load a webpage.

# 4 boxplot with the number of times a customer contacted the company
# via a phone call against an indicator whether a customer
# switched to a different provider

churn_data %>% ggplot() + 
 geom_boxplot(aes(x = churn, y = enqcount, color = churn)) +
 labs (y = "The number of times a customer contacted the company", 
       x = "Switched to a new provider",
       color = "Switched")

# COMMENTARY

# we can observe that in average the customers that switched to a different
# operator contacted the company via a phone call 1 time more often than others
# The biggest number of contact attempts is 2 calls more than of the customers
# who haven't changed their providers. Needs to be mentioned that some of
# the customers who switched didn't contact the company even once. On the contrary
# minority of the customers who haven't changed their provider also have more
# than 5 and even 6 calls. Still it will be safe to assume that the number of calls
# impacts the customers' decision to choose a different operator but its importance
# is smaller the time to load a webpage.


#*CONCLUSION
#*
# *Out of all the 4 factors that can possibly influence the 'churn' variable
# *time to load the webpage (which subsequently leads to the downlink speed) is the
# *most important one. Average phone call customer service waiting time doesn't
# *differ drastically but still is higher for customers who chose
# *different providers hence we could conclude that this aspect also plays its part in
# *the customers' decision as well as the number of phone calls to the company.
# *The most suspicious variable is the upload speed - for those clients who changed
# *their providers the uplink speed was actually higher but the downlink speed was lower
# *(comparing to the customers who didn't change their provider) while normaly
# *the opposite should be the case (unless we're talking about 5G).
# *Unfortunately, we don't have access to any other data hence we can only speculate that
# *perhaps there are some issues with the provider's network.

#----

# Machine Learning Part (b)∗:

#********
# Create a training set consisting of 350 randomly chosen
# data points and a test set consisting of the remaining 150 data points.
#********


set.seed(1) # to make the results reproducible
num_subset <- sample(nrow(churn_data), 350) # randomly choose 350 numbers out of 500

#* binding the attached variables of churn_data helps us get a matrix instead of dataframe
#* because KNN function doesn't like it when it's dataframe


X <- cbind(upload, webget, enqcount, callwait) 
Y <- churn
Y <- as.integer(Y == 'yes')
Y <- as.factor(Y)

detach(churn_data)
# dividing our data into training and testing matrices

train.X <- X[num_subset, ] # 350 records here
train.Y <- Y[num_subset]

test.X <- X[-num_subset, ] # 350 records here
test.Y <- Y[-num_subset]

#----

# Machine Learning Part (c)∗∗∗:


#* Using the training data set apply the K nearest neighbours
#* method to construct a classifier to predict churn based on the four available 
#* predictors. 

#********
#* Find the optimal K using leave-one-out cross-validation for the training data set.
#********


normalise <- function (inList){
  m <- mean(inList)
  s <- sd(inList)
  inList <- (inList - m)/s
  return(inList)
}

train.X <- apply(train.X, 2, normalise)
test.X <- apply(test.X, 2, normalise)

leave.KNN <- function(K, train.X, train.Y){
  error <- 0
  n <- nrow(train.X)
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


#finding optimal K as an index of the first smallest error value
optim.K <- which.min(errors)
optim.K

#********
#* Calculate the test error for the classification rule obtained for the optimal K.
#********

# below is our KNN
def.knn <- knn(train = train.X, test = test.X, cl = train.Y, k = optim.K) # yep that's it

table(def.knn, test.Y)
tab <- table(def.knn, test.Y)

# calculating the error using falsely predicted churn values
error.KNN <- (tab[1,2] + tab[2,1]) / sum(tab) 
sprintf("Expected error: %f Test error: %f", min(errors), error.KNN)


#* Machine Learning Part (d)∗∗:
#* Using the training data set apply the random forest (bagging) method to
#* construct a classifier to predict churn based on the four available predictors.

testing.X <- churn_data[-num_subset, ] %>% subset(select = -churn)
testing.Y <- churn_data[-num_subset, ] %>% subset(select = churn)
testing.Y <- unlist(testing.Y)

random.tree <- randomForest(churn ~ ., data = churn_data, subset = num_subset, mtry = 2, 
                            importance = TRUE)

# ANOTHER OPTION - USING TRAINING SET FROM THE BEGINNING without subsetting
# rf.tree = randomForest(churn ~ ., data = training, mtry = 2)

# *****
#* Using the obtained random forest, comment on the importance of the four
#* variables for predicting churn. 
# *****

varImpPlot(random.tree, main = "Random Forest Variable Importance")

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

error.RandomForest <- (tab[1,2] + tab[2,1]) / sum(tab)
error.RandomForest

sprintf("KNN Error: %f RandomForrest Error: %f", error.KNN, error.RandomForest)

# COMMENTARY
#* Random Forest method is more accurate than KNN with optimal K set to 1 with 
#* prediction error almost 10% lower than KNN. 
# However, FUN FACT: keep.forest=FALSE - this parameter removes the forest of trees
# from our random forest model.if this parameter is kept and we have only 
# one tree left, then the error rate grows to 0.12 = the same as KNN's error

#****
# Machine Learning Part (e)∗∗: Using the entire data set (training set and 
# test set combined), perform Principal Component Analysis for the four 
# variables: upload, webget,enqcount and callwait. Comment on the results.
#****

churn_predictors <- churn_data[ , c("upload", "webget","enqcount", "callwait")]

ggpairs(churn_predictors)

#* COMMENTARY
#* The 'upload' and 'webget' variables have the biggest correlation coefficient
#* 0.661. Since the is closer to 1 we can assume that although weak but there
#* might be dependency between those 2 variables. The 'upload' data has the 
#* majority of records with speed between 7.5 and 12.5 units, while most of 'webget'
#* entries' values rise up to 600 units. 'callwait' and 'enqcount' have second biggest
#* but still a very weak correlation of 0.278. Most of the entries in 'enqcount'
#* variable have at least one call and spread up until 7 calls with the highest 
#* number of entries in 2 calls while 'callwait' shows that most of the customers 
#* had to wait from 5 to 12.5 units (mins presumably) or on the contrary, we can
#* observe that some of the customers waited for less than 2.5 units of time.
#* Overall the correlation between predictors is rather weak, 'upload', 'enqcount' 
#* and 'callwait' variables have high variation. Correlation between 'upload' and 
#* 'webget' can be considered as strong in the current dataset since it's closer to 1. 

churn_pca <- princomp(churn_predictors, cor = TRUE)
# the first column contains the names of cities so we exclude it.
summary(churn_pca)

# COMMENTARY
#* New variable Comp.1 holds 41.5% of variance in the data and has the deviation 
#* of 1.289, which is the biggest spread of the data. Comp. 2 accounts for 31.9%
#* of the information variance giving a cumulative percentage of 73.5%.
#* The 2 components contain different information and created by using data from 
#* variables with certain weights.  

new_churn <- churn_pca$scores # creating a table of new predictors
rm(churn_predictors)


#****
# Using principal components, create the “best” two dimensional view of the data set. 
# In this visualisation, use colour coding to indiciate the churn. 
#****

# for the 2-dim view will use comp.1 and comp.2
new_churn <- data.frame(new_churn)
new_churn <- new_churn %>% mutate(churn = churn_data$churn)


# plotting
new_churn %>% ggplot(aes(x = Comp.1, y = Comp.2, color = churn)) +
  geom_point() +
  labs(x = "First Principal Component",
       y = "Second Principal Component") +
  coord_fixed(ratio = 1) 

#****
#*How much of the variation or information in the data is preserved in this plot?
#*Provide an interpretation of the first two principal components.
#****

biplot(churn_pca, cex = c(0.5, 1),
       xlab = "Contribution to First Principal Component",
       ylab = "Contribution to Second Principal Component")

#*COMMENTARY
#*Together, the first two principal components explain 73.5% of the variability.
#*ADD SOME INFO ON THE PLOT AND VARIABLES IN COMPONENTS

#****
# Machine Learning Part (f)∗∗∗: Apply the random forest (bagging) method to construct
# a classifier to predict churn based on the two first principal components 
# as predictors. In doing so, use the split of the data into a training and 
# test set (you may use the same indices as in part (b)).
#****

new_churn <- new_churn %>% subset(select = c(Comp.1, Comp.2, churn))
pca.test.X <- new_churn[-num_subset, ] %>% subset(select = -churn)
pca.test.Y <- churn_data[-num_subset, ] %>% subset(select = churn)
pca.test.Y <- unlist(pca.test.Y)

pca.random.tree <- randomForest(churn ~ ., data = new_churn, subset = num_subset, mtry = 1, 
                            importance = TRUE)
#****
# Calculate the test error for the obtained random forest and comment on it.
#****

pca.rf.predict <- predict(pca.random.tree, pca.test.X, type = "class")
tab <- table(pca.rf.predict, pca.test.Y)
tab

error.PCA.RandomForest <- (tab[1,2] + tab[2,1]) / sum(tab)
error.PCA.RandomForest

# COMMENTARY
# TO BE ADDED

#****
# Visualise the resulting classification rule on the scatter plot of the two 
# first principal components.
#****

pca.test.X %>% ggplot(aes(x = Comp.1, y = Comp.2, color = pca.rf.predict)) +
  geom_point() +
  coord_fixed(ratio = 1) + 
  labs(x = "Principal Component 1", y = "Principal Component 2",
       color = "Churn")


