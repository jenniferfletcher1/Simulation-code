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
# mean: X4 is a continuous variable
##############################################

dataset <- generated_dataset_MCAR

mode_imputation <- function(dataset){
  
  # mode for observed X1
  data_CCA_X1 <- filter(dataset, X1.I == 1)
  mode_X1 <- mfv(data_CCA_X1$X1)
  
  # impute "missing values" with mode
  dataset$X1_mode_imp <- as.factor(ifelse(dataset$X1.I == 0, as.numeric(as.character(mode_X1)), as.numeric(as.character(dataset$X1_miss))))
  dataset
}


mean_imputation <- function(dataset){

  # mean for observed X4
  data_CCA_X4 <- filter(dataset, X4.I == 1)
  mean_X4 <- mean(data_CCA_X4$X4)
  
  # impute "missing values" with mean
  dataset$X4_mean_imp <- as.numeric(ifelse(dataset$X4.I == 0, as.numeric(mean_X4), as.numeric(dataset$X4_miss)))
  dataset
}
