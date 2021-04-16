
# Import Dependencies -------------------------------------------------------------------------------------------------------

libs <- c('tidyverse', 'R2jags')
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
