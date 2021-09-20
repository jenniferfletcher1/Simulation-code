##############################################################################################
# generate data set in a function with missing data MAR
##############################################################################################

generating_dataset_MAR <- function(n, 
                                   prob_X1, 
                                   prob_X2, 
                                   mean_X3, sd_X3, 
                                   mean_X4, sd_X4, 
                                   beta0, beta1, beta2, beta3, beta4,
                                   beta0_m_X1, beta1_m_X1,
                                   beta0_m_X4, beta1_m_X4, beta2_m_X4){
  
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
  # missingness mechanism is MAR
  # dependent on X3
  #####################################
  
  ### generating indicator variable
  
  # generating indicator variable
  linear_predictor_m_X1 <- beta0_m_X1 + beta1_m_X1*df$X3
  probabilites_m_X1 <- exp(linear_predictor_m_X1) / (1 + exp(linear_predictor_m_X1))
  X1.I <- rbinom(n = n, size = 1, prob = probabilites_m_X1)
  
  # binding to dataset
  generated_dataset <- cbind(df, X1.I)
  generated_dataset$X1.I <- as.factor(generated_dataset$X1.I)
  
  ### checking relationship between X1.I & X3
  
  # fit model to determine if relationship is appropriate between X1.I & X3 by finding auc
  model_X1 <- glm(formula = X1.I ~ X3, data = generated_dataset, family = binomial(link = "logit")) 
  
  # make predictions on test data
  generated_dataset$predicted_probabilities_m_X1 <- predict(model_X1, generated_dataset, type = "response")
  # find optimal threshold 
  roc_J_m_X1 <- roc(response = generated_dataset$Y, 
                    predictor = generated_dataset$predicted_probabilities_m_X1,
                    levels=c("0", "1"), quiet = TRUE) 
  J_m_X1 <- max(roc_J_m_X1$sensitivities + roc_J_m_X1$specificities - 1)
  
  # predict classes
  generated_dataset$predicted_classes_m_X1 <- ifelse(generated_dataset$predicted_probabilities_m_X1 > J_m_X1, "1", "0")
  
  # find auc
  roc_X1 <- roc(response = generated_dataset$Y, 
                predictor = generated_dataset$predicted_probabilities_m_X1,
                levels=c("0", "1"), quiet = TRUE) # used to calculate auc
  auc_X1 <- roc_X1$auc 
  
  # add auc_X1 to dataset
  generated_dataset$auc_X1 <- auc_X1
  
  # generating X1 variable containing missingness
  generated_dataset$X1_miss <- as.factor(as.numeric(ifelse(generated_dataset$X1.I == 0, "NA", as.numeric(as.character(generated_dataset$X1)))))
  
  #####################################
  # introduce missing indicator for X4
  # missingness mechanism is MAR
  # dependent on X2 & X3
  #####################################
  
  ### generating indicator variable
  
  # generating indicator variable
  df$X2 <- as.numeric(as.character(df$X2)) # factor so convert to numeric for multiplication
  linear_predictor_m_X4 <- beta0_m_X4 + beta1_m_X4*df$X2 + beta2_m_X4*df$X3
  df$X2 <- as.factor(df$X2) # convert back to factor
  probabilites_m_X4 <- exp(linear_predictor_m_X4) / (1 + exp(linear_predictor_m_X4))
  X4.I <- rbinom(n = n, size = 1, prob = probabilites_m_X4)
  
  # binding to dataset
  generated_dataset <- cbind(generated_dataset, X4.I)
  generated_dataset$X4.I <- as.factor(generated_dataset$X4.I)
  
  ### checking relationship between X4 & X2
  
  # fit model to determine if relationship is appropriate between X4 and X2, X3 by finding auc
  model_X4 <- glm(formula = X4.I ~ X2 + X3, data = generated_dataset, family = binomial(link = "logit")) 
  
  # make predictions on test data
  generated_dataset$predicted_probabilities_m_X4 <- predict(model_X4, generated_dataset, type = "response")
  # find optimal threshold 
  roc_J_m_X4 <- roc(response = generated_dataset$Y, 
                    predictor = generated_dataset$predicted_probabilities_m_X4,
                    levels=c("0", "1"), quiet = TRUE) 
  J_m_X4 <- max(roc_J_m_X4$sensitivities + roc_J_m_X4$specificities - 1)
  
  # predict classes
  generated_dataset$predicted_classes_m_X4 <- ifelse(generated_dataset$predicted_probabilities_m_X4 > J_m_X4, "1", "0")
  
  # find auc
  roc_X4 <- roc(response = generated_dataset$Y, 
                predictor = generated_dataset$predicted_probabilities_m_X4,
                levels=c("0", "1"), quiet = TRUE) # used to calculate auc
  auc_X4 <- roc_X4$auc

  # add auc_X1 to dataset
  generated_dataset$auc_X4 <- auc_X4
  
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
  # output dataset 
  #####################################
  
  # final dataset
  generated_dataset <- subset(generated_dataset, select = c(X1, X2, X3, X4, Y, X1.I, X4.I, X1_miss, X4_miss, 
                                                            proportion_case, proportion_X1_missing, proportion_X4_missing, 
                                                            auc_X1, auc_X4))

  generated_dataset
  
}
