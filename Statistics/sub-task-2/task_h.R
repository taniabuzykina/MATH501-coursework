#
# Task H
# ------
# 
# Write jags/BUGS code to perform inference about the following related Bayesian Binary Logistic Regression model
#
# Run your code. Include a graphical representation of the traceplots and posterior densities of beta_0 and beta_1 
# in your report and discuss your results
#
# Relevant lecture: Session 8 (2021-03-18)
#

# Import Dependencies -------------------------------------------------------------------------------------------------------

libs <- c('tidyverse', 'R2jags', 'ggmcmc')
invisible(lapply(libs, library, character.only = TRUE))
rm(libs)

# Construct Dataset ---------------------------------------------------------------------------------------------------------

# Using values provided in the assignment brief
p_group <- 1:4
dose <- c(422, 744, 948, 2069)
n_patients <- c(50, 50, 50, 50)
n_improved <- c(2, 13, 39, 48)

data <- data.frame(p_group, dose, n_patients, n_improved)
rm(p_group, dose, n_patients, n_improved)


# Calculating the proportion of patients who improved
data <- data %>%
  mutate(proportion_improved = n_improved / n_patients)


# Data visualisation
ggplot(data, aes(x = dose, y = proportion_improved)) +
  geom_point() + 
  labs(x = "Dosage (mg/mL)", y = "Proportion of Patients w/ Improvement",
       title = "Dosage vs. Improvement") +
  scale_y_continuous(breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1),
                     minor_breaks = NULL,
                     limits = c(0, 1))

# Implementing the Model ----------------------------------------------------------------------------------------------------

# Likelihood
# ----------
# y_i ~ Bi(n_i, p_i), i = 1, ..., 6, independently
# p_i = Pr(success) = Pr(patient improvement)
# log(p_i / (1 - p_i)) = eta_1                <--- Link function
# eta_i = beta_0 + beta_1 log(x_i)            <--- Linear predictor, x = dose

# Prior
# -----
# beta_0 ~ N(0.0, precision = 0.0001)
# beta_1 ~ N(0.0, precision = 0.0001)

bayesian_binary_logistic_model <- function(){
  # Likelihood
  for (i in 1:n_obs){
    y[i] ~ dbin(p[i], n[i])
    
    # Link function
    logit(p[i]) <- eta[i]
    
    eta[i] <- beta_0 + beta_1 * log(x[i])
  }
  
  # Specify prior distributions on the unknown parameters
  beta_0 ~ dnorm(0.0, 1.0E-4)
  beta_1 ~ dnorm(0.0, 1.0E-4)
  
  # Evaluate the linear predictor at the new value of dose, named 'x_new'
  eta_new <- beta_0 + beta_1 * log(x_new)
  
  # Convert to the probability scale
  p_new <- exp(eta_new) / (1 + exp(eta_new))
}

# Data Preparation ----------------------------------------------------------------------------------------------------------

n_obs <- length(data$dose)
n <- data$n_patients
y <- data$n_improved
x <- data$dose

x_new <- 1500 # for the prediction task

data_binary_logistic <- list('n_obs', 'n', 'y', 'x', 'x_new')

# Perform Bayesian Inference ------------------------------------------------------------------------------------------------

bayesian_binary_logistic <- jags(data = data_binary_logistic,
                                 parameters.to.save = c('beta_0',
                                                        'beta_1',
                                                        'p', 'p_new'),
                                 n.iter = 100000,
                                 n.chains = 3,
                                 model.file = bayesian_binary_logistic_model)

print(bayesian_binary_logistic, intervals = c(0.025, 0.5, 0.975))

# Visualizing Jags Output ---------------------------------------------------------------------------------------------------

# Convert jags output to an MCMC object
bayesian_binary_logistic.mcmc <- as.mcmc(bayesian_binary_logistic)

# Convert MCMC object to ggs object
bayesian_binary_logistic.ggs <- ggs(bayesian_binary_logistic.mcmc)


# Traceplots can be used to assess convergence, that is, no dependence on initial chain values
ggs_traceplot(bayesian_binary_logistic.ggs)
#
# We can see that all parameters have converged, following a horizontal pattern
#


# Density and Caterpillar plots for beta_0 and beta_1, respectively
ggs_density(bayesian_binary_logistic.ggs, family = '^beta_0')
ggs_caterpillar(bayesian_binary_logistic.ggs, family = '^beta_0')

ggs_density(bayesian_binary_logistic.ggs, family = '^beta_1')
ggs_caterpillar(bayesian_binary_logistic.ggs, family = '^beta_1')
#
# We can see that there is no posterior support at zero
#


#
ggs_density(bayesian_binary_logistic.ggs, family = '^p')
ggs_caterpillar(bayesian_binary_logistic.ggs, family = '^p')
#
# We can see that as the dosage increases, the likelihood of patient improvement increases
# Note that the final density subplot is 'p_new', which explains the apparent decrease in likelihood
#