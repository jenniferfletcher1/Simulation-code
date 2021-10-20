#######################################################################################################
# generating standard error and 95% CI using functions for difference in means between two variables
#######################################################################################################

sd_difference_means <- function(x1, x2){ 
  sd1 <- sqrt(var(x1))
  sd2 <- sqrt(var(x2))
  
  sqrt(
    ((length(x1)-1)*(sd1)^2 + (length(x2)-1)*(sd2)^2) / (length(x1) + length(x2) - 2)
  )
}

se_difference_means <- function(x1, x2){# standard deviation for each group
  sd1 <- sqrt(var(x1))
  sd2 <- sqrt(var(x2))
  # standard deviation for difference in means  
  sd_difference_means <-  sqrt(
    ((length(x1)-1)*(sd1)^2 + (length(x2)-1)*(sd2)^2) / (length(x1) + length(x2) - 2)
  )
  # standard error for difference in means
  sd_difference_means*(sqrt((1/length(x1))+(1/length(x2))))
}

se_95_CI_lower_difference_means <- function(mean_difference, se_difference_means){
  mean_difference - 1.96*se_difference_means
}

se_95_CI_upper_difference_means <- function(mean_difference, se_difference_means){
  mean_difference + 1.96*se_difference_means
}
