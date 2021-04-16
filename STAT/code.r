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
## Calculate the proportion of patients who gets better with the new medicine and
## use ggplot2 to visualize these data. 

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
 labs(x = "Dose (mg/mL)",
      y = "Proportion of patients who get better") +
 scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                    minor_breaks = NULL,
                    limits = c(0, 1))

## What can you conclude from the plot?

# A dose of 500mg/ml or less has no real impact on the proportion of patients
# who get better (less than 4%). There is a reasonable increase at around the
# 750mg/ml as it reaches over 20% of the patients. However, the best increase is
# when the dose is increased to 1000mg/ml with nearly 80% of patients getting
# better. To increase the proportion of patients to just under 100%, it appear
# that the dose needs to be more than double to above 2000mg/ml. This is
# comparable with the current COVID vacinations which see a large increase from
# the first dose and almost complete immunity with a second dose.


# Statistical Modelling Part (b)* -----------------------------------------

##QUESTION
## Fit the model in the frequentist framework and report hat beta_0 (intercept)
## and hat beta_1 (Dose (x))

# Fitting the binary logistic regression model
m <- glm(cbind(Number_better, 
               Number_treated - Number_better) ~ Dose, 
         family = binomial, 
         data = experiment_df)

# Maximum likelihood estimates - hat beta_0 and hat beta_1 of the parameters beta_0 and beta_1
beta_0_hat <- coef(m)[1]
beta_0_hat

beta_1_hat <- coef(m)[2]
beta_1_hat

# Confidence intervals for beta_0 and beta_1
confint(m)


# Visualization of the fitted model

# Number of Dose points at which to predict
N <- 20

# Sequence of Dose values
Dose_seq <- seq(from = min(Dose), to = max(Dose), length = N)

# Predict beta_0 + beta_1 x = beta_0 + beta_1 Dose at these values
# The predicted values are given by hat beta_0 + hat beta_1 Dose
eta_seq <- predict(m, # Model, and new data:
                   newdata = data.frame(Dose = Dose_seq))

# Transform to probabilities
p_seq <- exp(eta_seq) / (1 + exp(eta_seq))

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
 labs(x = "Dose (mg/mL)",
      y = "Proportion of patients who get better") +
 scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                    minor_breaks = NULL,
                    limits = c(0, 1))



# Statistical Modelling Part (c)*** ---------------------------------------
# Logarithmic Model

## QUESTION
## Create a similar plot to that produced in part (a) to visualise log(di)
## against the proportion of Covid-19 patients who gets better and compare the
## two plots.

# Work out the log of the dose and add to data frame
experiment_df <- experiment_df %>% mutate(Log_Dose = log(Dose))

# Plot these proportions
ggplot(experiment_df, aes(x = Log_Dose, y = Proportion_reduced)) +
 geom_point() +
 labs(x = "Log of Dose (mg/mL)",
      y = "Proportion of patients who get better") +
 scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                    minor_breaks = NULL,
                    limits = c(0, 1))


## QUESTION
## What do you conclude?

print("############################## INSERT ANSWER HERE ##############################")


# Fitting the binary logistic regression model
# glm needs the number of successes and the number of failures
m_log <- glm(cbind(Number_better, 
               Number_treated - Number_better) ~ Log_Dose, 
         family = binomial, 
         data = experiment_df)

## QUESTION
## Fit the logarithmic model in the frequentist framework, report hat beta_0 and hat beta_1
beta_0_hat <- coef(m_log)[1]
beta_0_hat

beta_1_hat <- coef(m_log)[2]
beta_1_hat


## QUESTION
## Calculate the 95% confidence intervals for beta_0 and beta_1.
confint(m_log)


## QUESTION
## Which of the two frequentist models considered in parts (b) and (c) (the standard or the logarithmic model) do you prefer? Why?

print("############################## INSERT ANSWER HERE ##############################")



# Statistical Modelling Part (d)** ----------------------------------------

## QUESTION
## Use the logarithmic model implemented in part (c) to predict the probabilities that Covid-19 patients who receive doses of 600, 800 and 1500 mg/mL of the medicine get better. In addition, calculate the 95% confidence intervals for each prediction.

## Indirect Method
Dose_new <- c(log(600),log(800),log(1500))

# Estimated value of eta and associated standard error at Dose_new
eta_hat <- predict(m_log, 
               newdata = data.frame(Log_Dose = Dose_new),
               se.fit = TRUE) # Extract the standard errors

# Approximate 95% confidence interval for eta
eta_estimate_ci <- c(estimate =eta_hat$fit, 
                      lower = eta_hat$fit - 2 * eta_hat$se.fit, # Lower limit
                      upper = eta_hat$fit + 2 * eta_hat$se.fit) # Upper limit

# Convert to a confidence interval for p
p_estimate_ci <- exp(eta_estimate_ci) / (1 + exp(eta_estimate_ci))

data.frame(exp(Dose_new), p_estimate_ci)


## QUESTION
## What can you conclude from these results?

print("As the dose increases the probability of getting better increases.")


## Direct Method
p_hat_direct <- predict(m_log, 
               newdata = data.frame(Log_Dose = Dose_new),
               type = "response", # Prediction of p, not eta
               se.fit = TRUE) # Extract the standard errors

# Confidence interval for p
p_estimate_ci_direct <- c(estimate = p_hat_direct$fit,
                            lower = p_hat_direct$fit - 2 * p_hat_direct$se.fit, # Lower limit
                            upper = p_hat_direct$fit + 2 * p_hat_direct$se.fit) # Upper limit

data.frame(exp(Dose_new), p_estimate_ci_direct)

## QUESTION
## Which one of the two methods is generally recommended? Why?

print("It is generally recommended to use the indirect method as it provides more accurate results more regularly than the direct method.")

# Statistical Modelling Part (e)** ----------------------------------------

## QUESTION
## Use the logarithmic model implemented in part (c) to produce the plot below, with 95% confidence intervals obtained using the indirect method, and comment on it.


Dose_seq_log <- seq(from = min(log(Dose)), to = max(log(Dose)), length = N)

# Obtain the estimated values of eta and associated standard errors at a sequence of Dose values
eta_seq_log <- predict(m_log, 
                  newdata = data.frame(Log_Dose = Dose_seq_log),
                  se.fit = TRUE) # Extract the standard errors

# Transform estimated values of eta to probabilities 
p_seq_log <- exp(eta_seq_log$fit) / (1 + exp(eta_seq_log$fit))

#  Confidence intervals for eta, saved in a matrix 
eta_seq_log_ci <- cbind(eta_seq_log$fit - 2 * eta_seq_log$se.fit, # Lower limits
                        eta_seq_log$fit + 2 * eta_seq_log$se.fit) # Upper limits

# Convert to ** confidence intervals for p **
p_seq_log_ci <- 
  exp(eta_seq_log_ci) / (1 + exp(eta_seq_log_ci))

# Put all these values into a data frame
predictions_df_log <- data.frame(Dose_seq_log,  
                             p_seq_log = p_seq_log, # Estimated values of p
                              # Lower confidence limits
                             p_lower_log = p_seq_log_ci[,1],
                              # Upper confidence limits
                             p_upper_log = p_seq_log_ci[,2])

# Add to the ggplot 
ggplot(experiment_df, aes(x = Log_Dose, y = Proportion_reduced)) +
  geom_point() +
   # Add the estimated values of p (predictions)
  geom_line(aes(x = Dose_seq_log, 
                y = p_seq_log), 
            data = predictions_df_log, # Stored in this data frame
            colour = "red") +
   # Add the lower confidence limits
  geom_line(aes(x = Dose_seq_log, 
                y = p_lower_log), 
            data = predictions_df_log, # Stored in this data frame
            colour = "red") +
     # Add the upper confidence limits
  geom_line(aes(x = Dose_seq_log, 
                y = p_upper_log), 
            data = predictions_df_log, # Stored in this data frame
            colour = "red") +
  labs(x = "Log of Dose (mg/mL)",
       y = "portion of patients who get better") +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     minor_breaks = NULL,
                     limits = c(0, 1))


## QUESTION
## In addition, perform a suitable statistical test to check whether the logarithmic model is adequate. 

# The hypotheses are:
# Null hypothesis H0: Model (2) provides an adequate fit to the data
# Alternative hypothesis H1: Model (2) does not provide an adequate fit to the data.

p_value <- pchisq(deviance(m_log),# Specify the model under consideration
            df.residual(m_log),
            lower = FALSE)

p_value

## QUESTION
## State your hypotheses and conclusions carefully.
print("p-value is less than 0.05 and thus the null hypothesis is rejected signifiying that this model is not an adequete fit to the data.") 
