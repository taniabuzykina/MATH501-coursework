#######################################################################
# STATISTICAL MODELLING
#######################################################################


# Add Libraries -----------------------------------------------------------



# Statistical Modelling Part (a)* -----------------------------------------
#Calculate the proportion of patients who gets better with the new medicine and
#use ggplot2 to visualize these data. What can you conclude from the plot?

patientGroup <- c(1,2,3,4) #i
dose <- c(422,744,948,2069) #di
patientNumbers <- c(50,50,50,50) #ni
patientBetter <- c(2,13,39,48) #yi


df <- data.frame(patientGroup, dose, patientNumbers, patientBetter)

