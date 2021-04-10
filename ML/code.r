#---- 

# Machine Learning Part (a)∗∗: Present the data visually using box-and-whisker plots with
# a distinction for churn. Comment on the data in the context of the problem.
library(readr)
library(ggplot2)
library(tidyverse)
library(class) #library for KNN

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
#*Out of all the 4 factors that can possibly influence the 'churn' variable 
#*time to load the webpage (which subsequently leads to the downlink speed) is the 
#*most important one. Average phone call customer service waiting time doesn't 
#*differ drastically but still is higher for customers who chose 
#*different providers hence we could conclude that this aspect also plays its part in 
#*the customers' decision as well as the number of phone calls to the company.
#*The most suspicious variable is the upload speed - for those clients who changed
#*their providers the uplink speed was actually higher but the downlink speed was lower
#*(comparing to the customers who didn't change their provider) while normaly 
#*the opposite should be the case (unless we're talking about 5G). 
#*Unfortunately, we don't have access to any other data hence we can only speculate that 
#*perhaps there are some issues with the provider's network. 

#----

# Machine Learning Part (b)∗:


# Create a training set consisting of 350 randomly chosen
# data points and a test set consisting of the remaining 150 data points.

set.seed(1) # to make the results reproducible
num_subset <- sample(length(churn), 350) # randomly choose 350 numbers out of 500

# useless chunks of code
#----

# subsetting our dataset, predictors and classes together in dataframes

# train_churn <- churn_data[num_subset, ] # training set = 350 random records
# test_churn <- churn_data[-num_subset, ] # test set = remaining 150 records
# 
# # separating predictors and classifiers for test and train sets
# 
# train.X <- train_churn[-5]
# train.Y <- train_churn[5]
# 
# test.X <- test_churn[-5]
# test.Y <- test_churn[5]
# 
# rm(train_churn, test_churn) #cleaning up

# train_churn <- churn_data[num_subset, ] # training set = 350 random records
# test_churn <- churn_data[-num_subset, ] # test set = remaining 150 records

#----

# actually working code:

#* binding the attached variables of churn_data helps us get a matrix instead of dataframe
#* because KNN function doesn't like it when it's dataframe


X <- cbind(upload, webget, enqcount, callwait) 
Y <- churn

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

k <- 3 #setting K=3 just to try it out at first

# below is our KNN
def.knn <- knn(train = train.X, test = test.X, cl = train.Y, k = k) # yep that's it

table(def.knn, test.Y)
tab <- table(def.knn, test.Y)

# calculating the error using falsely predicted churn values

error <- (tab[1,2] + tab[2,1]) / sum(tab) 
error

#* Find the optimal K using leave-one-out cross-validation for the training data set.
#* Calculate the test error for the classification rule obtained for the optimal K.
# 
# n <- nrow(Carseats) # the number of data points in the data set Carseats
# cv.predictions <- rep('Yes', n)
# for(i in 1:n) { # start a loop over all data points
#         # Fit a classification tree using all data except one data point.
#         tree.fit <- tree(High ~ . - Sales, data = Carseats[-i, ])
#         # Make a prediction for the excluded data point.
#         cv.predictions[i] <- predict(tree.fit, newdata = Carseats[i,], type = "class")
# }

n <- nrow(train.X)

error <- 0


#* option 1: forming a 1-record test set along the way in the loop and then 
#* finding an average error from all the test errors produced by KNN in the loop
#* do the same thing for like n Ks and then choose K with the smallest average error


leave.KNN <- function(k){
        
        for(i in 1:n){
                temp.train.X <- train.X[-i,]
                temp.train.Y <- train.Y[-i]
                temp.test.X <- train.X[i,]
                temp.test.Y <- train.Y[i]
                temp.knn <- knn(train = temp.train.X, test = temp.test.X, cl = temp.train.Y, k = k)
                temp.tab <- tab <- table(temp.knn, temp.test.Y)
                error <- error + (tab[1,2] + tab[2,1]) / sum(tab)
        }
        error <- error/n
     return(error)   
}

errors <- rep(0, 30)
for (j in 1:30) errors[j] <- leave.KNN(j)
plot(errors, xlab="K", ylab = "Test error")

#* option 2: let's try just normal test set LOL
#* 

normal.KNN <- function(k){
        def.knn <- knn(train = train.X, test = test.X, cl = train.Y, k = k) # yep that's it
        
        tab <- table(def.knn, test.Y)
        
        
        error <- (tab[1,2] + tab[2,1]) / sum(tab) 
        
}

errors2 <- rep(0, 30)
for (j in 1:30) errors2[j] <- normal.KNN(j)
plot(errors2, xlab="K", ylab = "Test error")


