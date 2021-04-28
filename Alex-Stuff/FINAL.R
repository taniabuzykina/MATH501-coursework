
# Import Dependencies -------------------------------------------------------------------------------------------------------

libs <- c('tidyverse', 'R2jags', 'ggmcmc')
invisible(lapply(libs, library, character.only = TRUE))
rm(libs)

# Construct Dataset ---------------------------------------------------------------------------------------------------------

# A medical study involves four groups of fifty patients affected by COVID-19. Each group is subjected to a different dose of
# a new medicine (expressed in mg/mL), and the number of patients in each group who get better is recorded.

p_group <- 1:4 # patient group
dose <- c(422, 744, 948, 2069) # medicine dosage
n_patients <- c(50, 50, 50, 50) # number of patients in the group
n_improved <- c(2, 13, 39, 48) # number of patients who showed signs of improvement

covid_data <- data.frame(p_group, dose, n_patients, n_improved)
rm(p_group, dose, n_patients, n_improved)

# Calculate the proportion of patients who showed signs of improvement, and the log of dose
covid_data <- covid_data %>% mutate(
  percent_improved = n_improved / n_patients,
  log_dose = log(dose)
)

# Task H --------------------------------------------------------------------------------------------------------------------

# Write jags/BUGS code to perform inference about the following related bayesian binary logistic regression model (that we 
# name the bayesian logarithmic model)
#
# Run your code. Include a graphical representation of the traceplots and posterior densities of beta_0 and beta_1 in your
# report and discuss your results

# Preparing data
n_obs <- length(covid_data$dose)
n <- covid_data$n_patients
y <- covid_data$n_improved
x <- covid_data$dose

x_new <- 1500

task_h_data <- list('n_obs', 'n', 'y', 'x', 'x_new')

# Creating the model
bayesian_binary_logistic_model <- function() {
  for (i in 1:n_obs) {
    y[i] ~ dbin(p[i], n[i]) # binomial distribution
    
    logit(p[i]) <- eta[i] # link function
    
    eta[i] <- beta_0 + beta_1 * log(x[i])
  }
  # Specify prior distributions on the unknown parameters
  beta_0 ~ dnorm(0.0, 1.0E-4)
  beta_1 ~ dnorm(0.0, 1.0E-4)
  
  # Evaluate the linear predictor at the new value of dose (x_new)
  eta_new <- beta_0 + beta_1 * log(x_new)
  
  # Convert to the probability scale
  p_new <- exp(eta_new) / (1 + exp(eta_new))
}

# Performing bayesian inference
bayesian_binary_logistic <- jags(data = task_h_data,
                                 parameters.to.save = c('beta_0', 'beta_1', 'p', 'p_new'),
                                 n.iter = 100000,
                                 n.chains = 3,
                                 model.file = bayesian_binary_logistic_model)

# Print numerical results of the model
print(bayesian_binary_logistic, intervals = c(0.025, 0.5, 0.975))
#
# The estimates for beta_0 and beta_1 are -33.94 and 5.04, respectively. We can also see that zero does not appear within
# the credible intervals for beta_0 and beta_1 which indicates no posterior support at zero, which means that both values
# are useful in our model.
#

# Visualising model results - Has our simulated markov chain converged to its stationary distribution?

# Convert jags output to an MCMC object, then to a ggs object
bayesian_binary_logistic.mcmc <- as.mcmc(bayesian_binary_logistic)
bayesian_binary_logistic.ggs <- ggs(bayesian_binary_logistic.mcmc)

# Traceplot
ggs_traceplot(bayesian_binary_logistic.ggs)
#
# Traceplots show the history of parameters values across iterations of the chain. The plot above shows that the average
# for all values appear to have converged as they follow a horizontal pattern - this suggests that they do not depend on
# their initial values. Those that seem more volatile, such as p[2] and p[3], are following a normal distribution.
#

# Autocorrelation plot
ggs_autocorrelation(bayesian_binary_logistic.ggs)
#
# Autocorrelation is a value between -1 and 1, which measures how linearly dependent the current value of the chain is to
# past values (known as lags).
#
# At the zeroth lag, a value in the chain has perfect autocorrelation with itself. As we progress along the chain, the
# values become less correlated. This is further indication that the values have converged.
#

# Density plots for beta_0 and beta_1
ggs_density(bayesian_binary_logistic.ggs, family = '^beta')
#
# We can see that for both beta_0 and beta_1, all three values have converged with one another as the distributions mostly
# overlap with one another.
#

# Caterpillar plots for beta_0 and beta_1
ggs_caterpillar(bayesian_binary_logistic.ggs, family = '^beta_0') +
  geom_vline(xintercept = 0)
ggs_caterpillar(bayesian_binary_logistic.ggs, family = '^beta_1') +
  geom_vline(xintercept = 0)
#
# Caterpillar plots provide an insight into the distribution of parameter values. The dots represent median values, and the
# thick and thin lines represent the 90% and 95% of the highest posterior density regions, respectively. We can see that in
# both plots, there is no posterior support at zero.
#

# Density and caterpillar plots for p values
ggs_density(bayesian_binary_logistic.ggs, family = '^p') +
  geom_vline(xintercept = 0)
ggs_caterpillar(bayesian_binary_logistic.ggs, family = '^p') +
  geom_vline(xintercept = 0)
#
# As we saw in the plots for beta_0 and beta_1, all values shown here have converged and show no indication of any posterior
# support at zero. Furthermore, we can see that there is a positive correlation between the dosage and the likelihood of a
# patient showing signs of improvement.
#

rm(bayesian_binary_logistic.mcmc, task_h_data, n, n_obs, x, x_new, y) # clean up

# Task I --------------------------------------------------------------------------------------------------------------------

# Compare the 95% confidence intervals of beta_0 and beta_1 obtained in part C using the frequentist logarithmic model, with
# the corresponding 95% credible intervals obtained in part H using the bayesian logarithmic model

# Creating the frequentist model
m_log <- glm(cbind(n_improved,
                   n_patients - n_improved) ~ covid_data$log_dose,
             family = binomial,
             data = covid_data)

# Print the 95% confidence intervals for beta_0 and beta_1
confint(m_log)

# Print the 95% credible intervals
bayesian_binary_logistic$BUGSoutput$summary[1:2, c('mean', '50%', '2.5%', '97.5%')]

#
# Looking at the intervals for both models, we can see that they share extremely similar results. This is because the prior
# distributions for the unknown values (beta_0 and beta_1) were vague. This is due to the fact that when flat or vague
# priors are used, the bayesian approach is based on the likelihood function, which is incredibly similar to the frequentist
# approach.
#

rm(m_log, bayesian_binary_logistic_model) # clean up

# Task J --------------------------------------------------------------------------------------------------------------------

# Using the bayesian logarithmic model implemented in part H, estimate approximate 95% credible intervals for the probability
# that COVID-19 patients who receive doses of 550, 780, and 1900 mg/mL of the medicine show signs of improvement.
#
# Include a graphical representation, together with the numerical values of the 95% credible intervals, and comment on your
# results.

# Preparing data
n_obs <- length(covid_data$dose)
n <- covid_data$n_patients
y <- covid_data$n_improved
x <- covid_data$dose

x_new <- c(550, 780, 1900)

task_j_data <- list('n_obs', 'n', 'y', 'x', 'x_new')

# Creating the model for prediction
bayesian_binary_logistic_model_prediction <- function() {
  for (i in 1:n_obs) {
    y[i] ~ dbin(p[i], n[i]) # binomial distribution
    
    logit(p[i]) <- eta[i] # link function
    
    eta[i] <- beta_0 + beta_1 * log(x[i])
  }
  # Specify prior distributions on the unknown parameters
  beta_0 ~ dnorm(0.0, 1.0E-4)
  beta_1 ~ dnorm(0.0, 1.0E-4)
  
  # Prediction
  for (j in 1:length(x_new)) {
    eta_new[j] <- beta_0 + beta_1 * log(j[i])
    
    # Probabilities
    p_new[j] <- exp(eta_new[j]) / (1 + exp(eta_new[j]))
  }
}

# Performing predictions
bayesian_binary_logistic_prediction <- jags(data = task_j_data,
                                            parameters.to.save = c('p_new'),
                                            n.iter = 10000,
                                            n.chains = 3,
                                            model.file = bayesian_binary_logistic_model_prediction)

# Print numerical results of the prediction
print(bayesian_binary_logistic_prediction, intervals = c(0.025, 0.5, 0.975))

# Visualising model results

# Convert jags output to an MCMC object, then to a ggs object
bayesian_binary_logistic_prediction.mcmc <- as.mcmc(bayesian_binary_logistic_prediction)
bayesian_binary_logistic_prediction.ggs <- ggs(bayesian_binary_logistic_prediction.mcmc)

#
#
#
# TODO
#
#
#

# Clean up
rm(n, n_obs, x, x_new, y, task_j_data, bayesian_binary_logistic_prediction.ggs, bayesian_binary_logistic_prediction.mcmc,
   bayesian_binary_logistic.ggs, bayesian_binary_logistic_model_prediction)

# Task K --------------------------------------------------------------------------------------------------------------------

# Consider the following quadratic bayesian model for the COVID-19 data and perform inference about its parameters, writing
# appropriate jags/BUGS code.
#
# Use numerical and graphical representations of the posterior probabilities to discuss whether beta_2 is an appropriate term
# in the model.

# Preparing data
n_obs <- length(covid_data$dose)
n <- covid_data$n_patients
y <- covid_data$n_improved
x <- covid_data$dose
x_new <- 1500

task_k_data <- list('n_obs', 'n', 'y', 'x', 'x_new')

# Creating the quadratic model
bayesian_binary_logistic_quadratic_model <- function() {
  for (i in 1:n_obs) {
    y[i] ~ dbin(p[i], n[i]) # binomial distribution
    
    logit(p[i]) <- eta[i] # link function
    
    eta[i] <- beta_0 + beta_1 * log(x[i]) + beta_2 * log(x[i])^2
  }
  
  # Specify prior distributions on unknown parameters
  beta_0 ~ dnorm(0.0, 1.0E-4)
  beta_1 ~ dnorm(0.0, 1.0E-4)
  beta_2 ~ dnorm(0.0, 1.0E-4)
  
  # Evaluate the linear predictor at the new value of dose (x_new)
  eta_new <- beta_0 + beta_1 * log(x_new) + beta_2 * log(x_new)^2
  
  # Convert to the probability scale
  p_new <- exp(eta_new) / (1 + exp(eta_new))
}

# Performing bayesian inference
bayesian_binary_logistic_quadratic <- jags(data = task_k_data,
                                           parameters.to.save = c('beta_0', 'beta_1', 'beta_2',
                                                                  'p', 'p_new'),
                                           n.iter = 100000,
                                           n.chains = 3,
                                           model.file = bayesian_binary_logistic_quadratic_model)

# Print numerical results of the quadratic model
print(bayesian_binary_logistic_quadratic, intervals = c(0.025, 0.5, 0.975))
#
# Looking at the credible intervals for beta_2, we can see that zero IS found within. This indicates that beta_2 is NOT
# useful in our model.
#

# Print numerical results of the non-quadratic model, created in task H
print(bayesian_binary_logistic, intervals = c(0.025, 0.5, 0.975))
#
# We can see that in the quadratic model, the credible intervals for beta_0 and beta_1 include zero whereas they do not
# in the non-quadratic model.
#

# Comparing the DIC (deviance information criterion) values of each model
bayesian_binary_logistic$BUGSoutput$DIC
bayesian_binary_logistic_quadratic$BUGSoutput$DIC
#
# Based on the numerical results above, we should expect to see that the simple model produces a lower DIC value than
# the quadratic model. This appears to not be the case: while both DIC values are incredibly similar, the quadratic
# model produces a slightly lower DIC value which is more favourable.
#
#
# TODO - Variance in results for each execution. Majority of results show that the logarithmic model has a lower DIC
# value than the quadratic
#
#

# Visualising quadratic model results

# Convert jags output to an MCMC object, then to a ggs object
bayesian_binary_logistic_quadratic.mcmc <- as.mcmc(bayesian_binary_logistic_quadratic)
bayesian_binary_logistic_quadratic.ggs <- ggs(bayesian_binary_logistic_quadratic.mcmc)

# Density plots
ggs_density(bayesian_binary_logistic_quadratic.ggs, family = '^beta') +
  geom_vline(xintercept = 0)
#
# The density plots illustrate the inclusion of zero within the credible intervals for all three unknown parameters in
# the quadratic model. Here we can more clearly see the posterior support for all unknown parameters, with significant
# posterior support for beta_2
#

# Task L --------------------------------------------------------------------------------------------------------------------

# Which of the two bayesian models considered in parts H (the logarithmic) and K (the quadratic) do you prefer? Why?

#
# Based on the results of parts H and K, the simple (logarithmic) model appears to be the best suited for the task of
# predicting the effectiveness of different dosages of medicine, given the provided data.
#
# The simple model resulted in no signs of posterior support for all unknown values while the quadratic model, with the
# inclusion of beta_2, resulted in posterior support showing for ALL unknown values (not just beta_2).
#
# The quadratic model did produce a slightly lower DIC value (29.226) than the simple model (29.837). This difference is
# negligible however, and given the appearance of posterior support for unknown values in the quadratic model, the simple
# model is still likely the more favourable option of the two.
#
