##############################################################################################
# generating standard error and 95% CI using functions for one variable
##############################################################################################

standard_error <- function(x){ 
  sqrt(var(x) / length(x))}

se_95_CI_lower <- function(mean, se){
  mean - 1.96*se
}

se_95_CI_upper <- function(mean, se){
  mean + 1.96*se
}

