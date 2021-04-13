# STATISTICAL MODELLING


# Add Libraries -----------------------------------------------------------
library(dplyr)
library(ggplot2)
library(MASS)


# Data --------------------------------------------------------------------
Patient_group <- c(1,2,3,4) #i
Dose <- c(422,744,948,2069) #di
Number_treated <- c(50,50,50,50) #ni
Number_better <- c(2,13,39,48) #yi

#
# Put the data in a data frame
#
experiment_df <- data.frame(Patient_group, Dose, Number_treated, Number_better)
experiment_df


# Statistical Modelling Part (a)* -----------------------------------------
#Calculate the proportion of patients who gets better with the new medicine and
#use ggplot2 to visualize these data. What can you conclude from the plot?

#
# Work out proportions with reduced blood pressure 
#
experiment_df <- experiment_df %>% mutate(Proportion_reduced = Number_better / Number_treated)
experiment_df

# 
# Plot these proportions
#
ggplot(experiment_df, aes(x = Dose, y = Proportion_reduced)) +
 geom_point() +
 labs(x = "Dose (mg/ml)",
      y = "Proportion of patients who get better") +
 scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                    minor_breaks = NULL,
                    limits = c(0, 1))

#
# Conclusion from the plot
#
# A dose of 500mg/ml or less has no real impact on the proportion of patients
# who get better (less than 4%). There is a reasonable increase at around the
# 750mg/ml as it reaches over 20% of the patients. However, the best increase is
# when the dose is increased to 1000mg/ml with nearly 80% of patients getting
# better. To increase the proportion of patients to just under 100%, it appear
# that the dose needs to be more than double to above 2000mg/ml. This is
# comparable with the current COVID vacinations which see a large increase from
# the first dose and almost complete immunity with a second dose.


# Statistical Modelling Part (b)* -----------------------------------------
# Fit the model in the frequentist framework and report hat beta_0 (intercept)
# and hat beta_1 (Dose (x))

#
# Fitting the binary logistic regression model
# glm needs the number of successes and the number of failures
#
m <- glm(cbind(Number_better, 
               Number_treated - Number_better) ~ Dose, 
         family = binomial, 
         data = experiment_df)

#
# Maximum likelihood estimates - hat beta_0 and hat beta_1 of 
# the parameters beta_0 and beta_1
#
beta_0_hat <- coef(m)[1]
beta_0_hat

beta_1_hat <- coef(m)[2]
beta_1_hat

#
# Confidence intervals for beta_0 and beta_1
#
confint(m)

# *** Visualization of the fitted model ***

# Number of Dose points at which to predict
N <- 20

# Sequence of Dose values
Dose_seq <- seq(from = min(Dose), to = max(Dose), length = N)
#Dose_seq

# Predict beta_0 + beta_1 x = beta_0 + beta_1 Dose at these values
# The predicted values are given by hat beta_0 + hat beta_1 Dose
eta_seq <- predict(m, # Model, and new data:
                   newdata = data.frame(Dose = Dose_seq))
#eta_seq

# Transform to probabilities
p_seq <- exp(eta_seq) / (1 + exp(eta_seq))
#p_seq

# Put these values into a data frame
predictions_df <- data.frame(Dose_seq, eta_seq, p_seq)

# Add to the ggplot
ggplot(experiment_df, aes(x = Dose, y = Proportion_reduced)) +
 geom_point() +
 # Add the estimated values of p (predictions)
 geom_line(aes(x = Dose_seq, 
               y = p_seq), 
           data = predictions_df, # Stored in this data frame
           colour = "red") +
 labs(x = "Dose (mg/ml)",
      y = "Proportion of patients who get better") +
 scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                    minor_breaks = NULL,
                    limits = c(0, 1))



# Statistical Modelling Part (c)*** ---------------------------------------
# Logarithmic Model

# Create a similar plot to that produced in part (a) to visualise log(di)
# against the proportion of Covid-19 patients who gets better and compare the
# two plots. What do you conclude?


#
# Work out the log of the dose and add to data frame
#
experiment_df <- experiment_df %>% mutate(Log_Dose = log(Dose))








# Fit the model in the frequentist framework and report hat beta_0 (intercept)
# and hat beta_1 (Dose (x))




#
# Fitting the binary logistic regression model
# glm needs the number of successes and the number of failures
#
m_log <- glm(cbind(Number_better, 
               Number_treated - Number_better) ~ Log_Dose, 
         family = binomial, 
         data = experiment_df)

#
# Maximum likelihood estimates - hat beta_0 and hat beta_1 of 
# the parameters beta_0 and beta_1
#
beta_0_hat <- coef(m_log)[1]
beta_0_hat

beta_1_hat <- coef(m_log)[2]
beta_1_hat

Dose_new <- 600

beta_0_hat + beta_1_hat * log(Dose_new)

#
# Confidence intervals for beta_0 and beta_1
#
confint(m_log)

# *** Visualization of the fitted model ***

# Number of Dose points at which to predict
N <- 30 

# Sequence of Dose values
Dose_seq <- seq(from = min(Dose), to = max(Dose), length = N)
#Dose_seq

# Predict beta_0 + beta_1 x = beta_0 + beta_1 Dose at these values
# The predicted values are given by hat beta_0 + hat beta_1 Dose
eta_seq <- predict(m, # Model, and new data:
                   newdata = data.frame(Dose = Dose_seq))
#eta_seq

# Transform to probabilities
p_seq <- exp(eta_seq) / (1 + exp(eta_seq))
#p_seq

# Put these values into a data frame
predictions_df <- data.frame(Dose_seq, eta_seq, p_seq)

# Add to the ggplot
ggplot(experiment_df, aes(x = Dose, y = Proportion_reduced)) +
 geom_point() +
 # Add the estimated values of p (predictions)
 geom_line(aes(x = Dose_seq, 
               y = p_seq), 
           data = predictions_df, # Stored in this data frame
           colour = "red") +
 labs(x = "Dose (mg/ml)",
      y = "Proportion of patients who get better") +
 scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                    minor_breaks = NULL,
                    limits = c(0, 1))






# Statistical Modelling Part (d)** ----------------------------------------








# Statistical Modelling Part (e)** ----------------------------------------




