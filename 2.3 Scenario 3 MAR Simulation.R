##################################################################################################################
# Simulation Scenario 3: MAR
# approximately X hours to run
##################################################################################################################

# start time
start_time <- Sys.time()

# dataframes for simulation results to output to
predictions_df_full = data.frame()
predictions_df_CCA = data.frame()
predictions_df_mode_mean = data.frame() 
predictions_df_mice_1 = data.frame()
predictions_df_mice_10 = data.frame()
predictions_df_mice_40 = data.frame()

# generate n_sim=1000 simulations
set.seed(5)
for (i in 1:1000){
  
  # print i to know the current simulation 
  print(i) 
  
  ######################
  # generate dataset 
  ######################
  
  # approximately 50% cases
  generated_dataset_MAR <- generating_dataset_MAR(n = 5500,
                                                  prob_X1 = 0.17, prob_X2 = 0.016,
                                                  mean_X3 = 130, sd_X3 = 20.2,
                                                  mean_X4 = 3, sd_X4 = 0.98,
                                                  beta0 = -11.2, beta1 = 1.9, beta2 = 2, beta3 = 0.1, beta4 = -0.7,
                                                  beta0_m_X1 = -2.2, beta1_m_X1 = 0.02,
                                                  beta0_m_X4 = 2.7, beta1_m_X4 = -1.1, beta2_m_X4 = -0.01)
  
  ##################################################################################################################
  # full dataset
  ##################################################################################################################
  
  ###############################################################
  # generate predictions & coefficient estimates of each dataset
  ###############################################################
  
  # generate predictions: fully observed data (prediction scores are on test data)
  predictions_full <- generating_predictions(dataset = generated_dataset_MAR, nfolds = 10)
  # add sample size
  predictions_full$n <- as.numeric(NROW(generated_dataset_MAR))
  # add name of dataset generated
  predictions_full$name_dataset <- "full"
  # bind predictions to dataframe: each row are predictions of the ith simulation
  predictions_df_full = rbind(predictions_df_full, predictions_full)
  
  ##################################################################################################################
  # CCA
  ##################################################################################################################
  
  ###############################################################
  # generate predictions & coefficient estimates of each dataset
  ###############################################################
  
  # apply CCA
  dataset_CCA <- generating_CCA(dataset = generated_dataset_MAR) # function to generate CCA data
  # generate predictions (prediction scores are on test data)
  predictions_CCA <- generating_predictions(dataset = dataset_CCA, nfolds = 10)
  # add sample size
  predictions_CCA$n <- as.numeric(NROW(dataset_CCA))
  # add name of dataset generated
  predictions_CCA$name_dataset <- "CCA"
  # bind predictions to dataframe: each row are predictions of the ith simulation
  predictions_df_CCA = rbind(predictions_df_CCA, predictions_CCA)
  
  ##################################################################################################################
  # mode/mean
  ##################################################################################################################
  
  ###############################################################
  # generate predictions & coefficient estimates of each dataset
  ###############################################################
  
  # apply mean & mode imputation
  
  # X1: use mode imputation to impute missing values in X1
  data_mode <- mode_imputation(dataset = generated_dataset_MAR)
  # X4: use mean imputation to impute missing values in X4
  data_mode_mean <- mean_imputation(dataset = data_mode)
  
  # create complete datset
  data_mode_mean <- subset(data_mode_mean, select = -c(X1, X4))
  # update names for analysis
  names(data_mode_mean)[names(data_mode_mean) == "X1_mode_imp"] <- "X1"
  names(data_mode_mean)[names(data_mode_mean) == "X4_mean_imp"] <- "X4"
  
  # generate predictions (prediction scores are on test data)
  predictons_mode_mean <- generating_predictions(dataset = data_mode_mean, nfolds = 10)
  # add sample size
  predictons_mode_mean$n <- as.numeric(NROW(data_mode_mean))
  # add name of dataset generated
  predictons_mode_mean$name_dataset <- "mode/mean"
  # bind predictions to dataframe: each row are predictions of the ith simulation
  predictions_df_mode_mean = rbind(predictions_df_mode_mean, predictons_mode_mean)
  
  ##################################################################################################################
  # MICE: m=1
  ##################################################################################################################
  
  ##############################################
  # create dataset for all mice imputations
  ##############################################
  
  # create dataset for mice
  data_mice <- generated_dataset_MAR
  # subset data
  dataset_mice <- subset(data_mice, select = c(X1_miss, X2, X3, X4_miss, Y))
  # change names of X1_miss & X4_miss
  names(dataset_mice)[names(dataset_mice) == "X1_miss"] <- "X1"
  names(dataset_mice)[names(dataset_mice) == "X4_miss"] <- "X4"
  
  # matrix for imputation
  predictor_matrix <- matrix(c(0,1,1,1,1, 0,0,0,0,0, 0,0,0,0,0, 1,1,1,0,1, 0,0,0,0,0), nrow = 5, ncol = 5, byrow = TRUE)
  
  
  ###############################################################
  # generate predictions & coefficient estimates of each dataset
  ############################################################### 
  
  ### imputations: m=1
  dataset_mice_1 <- mice(data = dataset_mice, 
                         m = 1, maxit = 20, 
                         predictorMatrix = predictor_matrix,
                         method = c("logreg", "", "", "norm.nob", ""),
                         print=FALSE)
  
  # complete dataset
  dataset_mice_1_complete_1 <- complete(dataset_mice_1, 1) 
  # bind with extra variables
  mice_subset <- subset(data_mice, select = c(X1.I, X4.I, X1_miss, X4_miss,
                                              proportion_case, proportion_X1_missing, proportion_X4_missing,
                                              auc_X1, auc_X4))
  dataset_mice_1_complete_1 <- cbind(dataset_mice_1_complete_1, mice_subset)
  
  ### generate predictions
  
  # generate predictions (prediction scores are on test data)
  predictons_mice_1 <- generating_predictions(dataset = dataset_mice_1_complete_1, nfolds = 10)
  
  # add sample size
  predictons_mice_1$n <- as.numeric(NROW(dataset_mice_1_complete_1))
  # add name of dataset generated
  predictons_mice_1$name_dataset <- "mice 1"
  
  # bind predictions to dataframe: each row are predictions of the ith simulation
  predictions_df_mice_1 = rbind(predictions_df_mice_1, predictons_mice_1)
  
  ##################################################################################################################
  # MICE: m=10
  ##################################################################################################################
  
  ###############################################################
  # generate predictions & coefficient estimates of each dataset
  ############################################################### 
  
  ### imputations: m=10
  dataset_mice_10 <- mice(data = dataset_mice, 
                          m = 10, maxit = 20, 
                          predictorMatrix = predictor_matrix,
                          method = c("logreg", "", "", "norm.nob", ""),
                          print=FALSE)
  
  ### generate predictions
  
  # generate complete dataset & predictions for each imputation separately
  predictions_mice_10_all = data.frame()
  for (i in 1:10){
    
    # complete dataset for each imputation
    dataset_mice_10_complete <- complete(dataset_mice_10, i) 
    # bind with extra variables
    dataset_mice_10_complete <- cbind(dataset_mice_10_complete, mice_subset)
    
    # predictions for each imputation
    generated_predictions_mice_10 <- generating_predictions(dataset = dataset_mice_10_complete, nfolds = 10)
    
    # add sample size
    generated_predictions_mice_10$n <- as.numeric(NROW(dataset_mice_10_complete))
    # add name of dataset generated
    generated_predictions_mice_10$name_dataset <- "mice 10"
    
    # bind predictions of each imputation together 
    predictions_mice_10_all = rbind(predictions_mice_10_all, generated_predictions_mice_10)
    
  }
  
  # pool (i.e., average) all performance measures & parameter estimates into one set of results
  predictions_mice_10 <- averageing_predictions_mice(predictions_df = predictions_mice_10_all)
  
  # bind predictions to dataframe: each row are predictions of the ith simulation
  predictions_df_mice_10 = rbind(predictions_df_mice_10, predictions_mice_10)
  
  
  ##################################################################################################################
  # MICE: m=40
  ##################################################################################################################
  
  ###############################################################
  # generate predictions & coefficient estimates of each dataset
  ############################################################### 
  
  ### imputations: m=40
  dataset_mice_40 <- mice(data = dataset_mice, 
                          m = 40, maxit = 20, 
                          predictorMatrix = predictor_matrix,
                          method = c("logreg", "", "", "norm.nob", ""),
                          print=FALSE)
  
  ### generate predictions
  
  # generate complete dataset & predictions for each imputation separately
  predictions_mice_40_all = data.frame()
  for (i in 1:40){
    
    # complete dataset for each imputation
    dataset_mice_40_complete <- complete(dataset_mice_40, i) 
    # bind with extra variables
    dataset_mice_40_complete <- cbind(dataset_mice_40_complete, mice_subset)
    
    # predictions for each imputation
    generated_predictions_mice_40 <- generating_predictions(dataset = dataset_mice_40_complete, nfolds = 10)
    
    # add sample size
    generated_predictions_mice_40$n <- as.numeric(NROW(dataset_mice_40_complete))
    # add name of dataset generated
    generated_predictions_mice_40$name_dataset <- "mice 40"
    
    # bind predictions of each imputation together 
    predictions_mice_40_all = rbind(predictions_mice_40_all, generated_predictions_mice_40)
    
  }
  
  # pool (i.e., average) all performance measures & parameter estimates into one set of results
  predictions_mice_40 <- averageing_predictions_mice(predictions_df = predictions_mice_40_all)
  
  # bind predictions to dataframe: each row are predictions of the ith simulation
  predictions_df_mice_40 = rbind(predictions_df_mice_40, predictions_mice_40)
  
}

# end time
end_time <- Sys.time()

# duration (in minutes) for simulation to run
duration <- end_time - start_time
duration

# m=30: 11.5 hours
# m=40: 13.5 hours
# m=50: 16.5 hours
# m=100: 27.6 hours

########################################################################################################
# export complete datasets
########################################################################################################

write.csv(x = predictions_df_full, file = "predictions_df_full_MAR3.csv")
write.csv(x = predictions_df_CCA, file = "predictions_df_CCA_MAR3.csv")
write.csv(x = predictions_df_mode_mean, file = "predictions_df_mode_mean_MAR3.csv")
write.csv(x = predictions_df_mice_1, file = "predictions_df_mice_1_MAR3.csv")
write.csv(x = predictions_df_mice_10, file = "predictions_df_mice_10_MAR3.csv")
write.csv(x = predictions_df_mice_40 , file = "predictions_df_mice_40_MAR3.csv")
