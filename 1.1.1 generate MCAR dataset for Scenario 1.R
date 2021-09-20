##############################################################################################
# generate data set in a function with missing data MCAR
##############################################################################################

generating_dataset_MCAR <- function(n, 
                                    prob_X1, 
                                    prob_X2, 
                                    mean_X3, sd_X3, 
                                    mean_X4, sd_X4, 
                                    beta0, beta1, beta2, beta3, beta4,
                                    MCAR_prob_X1, MCAR_prob_X4){
  
  ##############################
  # generate complete data
  ##############################
  
  # generating independent variables
  X1 <- rbinom(n = n, size = 1, prob = prob_X1)
  X2 <- rbinom(n = n, size = 1, prob = prob_X2)
  X3 <- rnorm(n = n, mean = mean_X3, sd = sd_X3)
  X4 <- rnorm(n = n, mean = mean_X4, sd = sd_X4)
  
  # generating dependent variable
  linear_predictor <- beta0 + beta1*X1 + beta2*X2 + beta3*X3 + beta4*X4
  probabilites <- exp(linear_predictor) / (1 + exp(linear_predictor))
  Y <- rbinom(n = n, size = 1, prob = probabilites)
  
  # create dataframe for all variables
  df <- as.data.frame(cbind(X1, X2, X3, X4, Y))
  
  # convert variables to the correct class
  df$X1 <- as.factor(df$X1)
  df$X2 <- as.factor(df$X2)
  df$X3 <- as.numeric(df$X3)
  df$X4 <- as.numeric(df$X4)
  df$Y <- as.factor(df$Y)
  
  #####################################
  # introduce missing indicator for X1
  # missingness mechanism is MCAR
  #####################################
  
  ### generating indicator variable
  
  # generating indicator variable
  X1.I <- rbinom(n = n, size = 1, prob = MCAR_prob_X1)
  
  # binding to dataset
  generated_dataset <- cbind(df, X1.I)
  generated_dataset$X1.I <- as.factor(generated_dataset$X1.I)
  
  # generating X1 variable containing missingness
  generated_dataset$X1_miss <- as.factor(as.numeric(ifelse(generated_dataset$X1.I == 0, "NA", as.numeric(as.character(generated_dataset$X1)))))
  
  #####################################
  # introduce missing indicator for X4
  # missingness mechanism is MCAR
  #####################################
  
  ### generating indicator variable
  
  # generating indicator variable
  X4.I <- rbinom(n = n, size = 1, prob = MCAR_prob_X4)
  
  # binding to dataset
  generated_dataset <- cbind(generated_dataset, X4.I)
  generated_dataset$X4.I <- as.factor(generated_dataset$X4.I)

  # generating X4 variable containing missingness
  generated_dataset$X4_miss <- as.numeric(ifelse(generated_dataset$X4.I == 0, "NA", generated_dataset$X4))
  
  #####################################
  # add proportion of outcome = 1
  #####################################
  
  generated_dataset$proportion_case <- round(sum(generated_dataset$Y == 1) / 
                                               (sum(generated_dataset$Y == 1) + sum(generated_dataset$Y == 0)), 2)
  
  #####################################
  # add proportion of missingness
  #####################################
  
  # add proportion of missing values in X1 & X4
  generated_dataset$proportion_X1_missing <- round(sum(generated_dataset$X1.I == 0) / 
                                                                            (sum(generated_dataset$X1.I == 1) + sum(generated_dataset$X1.I == 0)), 2)
  generated_dataset$proportion_X4_missing <- round(sum(generated_dataset$X4.I == 0) / 
                                                                            (sum(generated_dataset$X4.I == 1) + sum(generated_dataset$X4.I == 0)), 2)
  
  #####################################
  # add auc of missingness mechanism
  #####################################
  
  # add auc of missingness mechanism of X1 & X4
  generated_dataset$auc_X1 <- 0
  generated_dataset$auc_X4 <- 0
  
  #####################################
  # output final dataset
  #####################################
  
  generated_dataset
  
}
