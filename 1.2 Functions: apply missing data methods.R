##############################################################################################
# apply CCA & imputation methods
##############################################################################################

######################################
# CCA
######################################

generating_CCA <- function(dataset){
  filter(dataset, X1.I == 1 & X4.I == 1)
}

##############################################
# mean / median & mode imputation: 
# mode: X1 is a categorical variable
# mean / median: X4 is a categorical variable
##############################################

mode_imputation <- function(dataset){
  # introduce a new variable for X1 to impute
  dataset$X1_mode_imp <- dataset$X1
  # create function for mode
  mode <- function(var){which.max(table(var))} # create function for mode
  # mode for X1
  mode_X1 <- mode(var = dataset$X1)
  # impute "missing values" with mode
  dataset$X1_mode_imp <- as.factor(ifelse(dataset$X1.I == 0, as.numeric(mode_X1), as.numeric(as.character(dataset$X1))))
  dataset
}

mean_imputation <- function(dataset){
  # introduce a new variable for X4 to impute
  dataset$X4_mean_imp <- dataset$X4
  # mean
  mean_X4 <- mean(dataset$X4_mean_imp)
  # impute "missing values" with mean
  dataset$X4_mean_imp <- as.numeric(ifelse(dataset$X4.I == 0, as.numeric(mean_X4), as.numeric(dataset$X4_mean_imp)))
  dataset
}
