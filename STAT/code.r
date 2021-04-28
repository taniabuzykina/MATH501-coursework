# STATISTICAL MODELLING


# Add Libraries -----------------------------------------------------------
lib <- c('dplyr', 'ggplot2', 'MASS')
lapply(lib, library, character.only = T)
rm(lib)


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
experiment_df <- experiment_df %>% mutate(Proportion_better = Number_better / Number_treated)
experiment_df

# 
# Plot these proportions
#
ggplot(experiment_df, aes(x = Dose, y = Proportion_better)) +
 geom_point() +
 geom_hline(yintercept=.25,linetype='dotted',col='blue')+
 geom_hline(yintercept=.75,linetype='dotted',col='blue')+
 labs(x = "Dose (mg/mL)",
      y = "Proportion of patients who get better") +
 scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                    minor_breaks = NULL,
                    limits = c(0, 1))

## What can you conclude from the plot?

print("A dose of 500mg/ml or less has no real impact on the proportion of patients who get better (less than 4%). There is a reasonable increase at around the 750mg/ml as it reaches over 20% of the patients. However, the best increase is when the dose is increased to 1000mg/ml with nearly 80% of patients getting better. To increase the proportion of patients to just under 100%, it appears that the dose needs to be more than double to above 2000mg/ml. This is comparable with the current COVID vacinations which see a large increase from the first dose and almost complete immunity with a second dose.")


# Statistical Modelling Part (b)* -----------------------------------------

##QUESTION
## Fit the model in the frequentist framework and report hat beta_0 (intercept)
## and hat beta_1 (Dose (x))

# Fitting the binary logistic regression model
m <- glm(cbind(Number_better, 
               Number_treated - Number_better) ~ Dose, 
         family = binomial, 
         data = experiment_df)

# Maximum likelihood estimates - hat_beta_0 and hat_beta_1 of the parameters beta_0 and beta_1
beta_0_hat <- coef(m)[1]
beta_0_hat

beta_1_hat <- coef(m)[2]
beta_1_hat

# Confidence intervals for beta_0 and beta_1
confint(m)

#-----------------------------------------------------------------------------------------------------

# Visualization of the fitted model

# Number of Dose points at which to predict

# Sequence of Dose values
Dose_seq <- seq(from = min(Dose), to = max(Dose), length = 20)

# Predict beta_0 + beta_1 x = beta_0 + beta_1 Dose at these values
# The predicted values are given by hat beta_0 + hat beta_1 Dose
eta_seq <- predict(m, # Model, and new data:
                   newdata = data.frame(Dose = Dose_seq))

# Transform to probabilities
p_seq <- exp(eta_seq) / (1 + exp(eta_seq))

# Put these values into a data frame
predictions_df <- data.frame(Dose_seq, eta_seq, p_seq)

# Add to the ggplot
ggplot(experiment_df, aes(x = Dose, y = Proportion_better)) +
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
experiment_df

# Plot these proportions
ggplot(experiment_df, aes(x = Log_Dose, y = Proportion_better)) +
 geom_point() +
 geom_vline(xintercept=log(500),linetype='dotted',col='blue')+
 geom_vline(xintercept=log(1000),linetype='dotted',col='blue')+
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
## Fit the logarithmic model in the frequentist framework, report hat_beta_0 and hat_beta_1
beta_0_hat <- coef(m_log)[1]
beta_0_hat

beta_1_hat <- coef(m_log)[2]
beta_1_hat


## QUESTION
## Calculate the 95% confidence intervals for beta_0 and beta_1.
confint(m_log)


## QUESTION
## Which of the two frequentist models considered in parts (b) and (c) (the standard or the logarithmic model) do you prefer? Why?

print("Due to the size of the data set it is difficult to see any difference in the graph as using a log in this case would help with data being compacted and needing to be scaled out in magnatude. As such there is no real benefit to this model. This model does however produce a more confusng graph as the x axis is in units of Log of Dose which is mre confusing that just the standard in graph a of dose as mg/ml. Graph a will be easier to read by many medical professionals as well as general people due to the accessability of this label. In a time of over whelming data accessability is key to helping people understand data and provide information.")


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

print("It is generally recommended to use the indirect method as it provides more accurate results more regularly than the direct method. This is not evident in the currnet data set, but this is most likely due to its small size.")

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
ggplot(experiment_df, aes(x = Log_Dose, y = Proportion_better)) +
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

# Null hypothesis H0: The model as it stands is useful.
# Alternative hypothesis H1: The model as it stands is NOT useful.

p_value <- pchisq(deviance(m_log),# Specify the model under consideration
            df.residual(m_log),
            lower = FALSE)
p_value

## QUESTION
## State your hypotheses and conclusions carefully.
print("P-value is less than 0.05 and thus the null hypothesis is rejected signifiying that this model is not an adequete fit to the data or in other words is not useful.") 


# Statistical Modelling Part (f)** ----------------------------------------

## QUESTION
## Fit the quadratic model in the frequentist framework and report hat_beta_0, hat_beta_1 and hat_beta_2.
m_log_log2 <- glm(cbind(Number_better, 
                        Number_treated - Number_better) ~ Log_Dose + I(Log_Dose^2), 
                family = binomial, 
                data = experiment_df)

## QUESTION
## Fit the logarithmic model in the frequentist framework, report hat_beta_0, hat_beta_1 and hat_beta_2
beta_0_hat <- coef(m_log_log2)[1]
  beta_0_hat
beta_1_hat <- coef(m_log_log2)[2]
  beta_1_hat
beta_2_hat <- coef(m_log_log2)[3]
  beta_2_hat

## QUESTION
## Perform a frequentist hypothesis test of size 0.05 of whether beta_2 is statistically significant and report your conclusion with justification. 

# The hypotheses are:
# Null hypothesis H0: Model (2) provides an adequate fit to the data
# Alternative hypothesis H1: Model (2) does not provide an adequate fit to the data.

# Null hypothesis H0: The model as it stands is useful.
# Alternative hypothesis H1: The model as it stands is NOT useful.

p_value <- pchisq(deviance(m_log_log2),# Specify the model under consideration
            df.residual(m_log_log2),
            lower = FALSE)
p_value

## QUESTION
## State your hypotheses and conclusions carefully.
print("P-value is less than 0.05 and thus the null hypothesis is rejected signifiying that this model is not an adequete fit to the data or in other words is not useful.") 


## QUESTION
## In addition, report the 95% confidence interval for beta_2. Does this result confirm the conclusion of the hypothesis test?
# confint(m_log_log2)
tail(confint(m_log_log2),1)



# Statistical Modelling Part (g)* ----------------------------------------

## QUESTION
## Use the analysis of Deviance method to compare the logarithmic model fitted in part (c) with the quadratic model fitted in part (f). State your hypotheses and conclusions carefully.
Deviance_c_log <- deviance(m_log)
Deviance_c_log
Deviance_f_log_log2 <- deviance(m_log_log2)
Deviance_f_log_log2

#  Difference in deviances 
Deviance_c_log - Deviance_f_log_log2

# Compare the models
#
# H_0: Model C is adequate compared to Model F
# H_1: Model C is not adequate compared to Model F
#
AnovaTest <- anova(m_log, m_log_log2, test = "Chisq")
AnovaTest[2,5]

print("Here, there p-value is 0.23. As this is greater than 0.05, we do not reject the null hypothesis and so Model C is adequate compared to Model F. In other words beta_2 = 0. Thus Model C is preferred over Model F")
