##############################################################################################
# fitting prediction model: generating predictions & parameter estimates
##############################################################################################

generating_predictions <- function(dataset, nfolds){
  
  ### cross validation
  
  # split data into 10 equal folds stratified by Y
  folds <- createFolds(y = dataset$Y, k = nfolds, list = TRUE, returnTrain = FALSE)
  
  prediction_measures_df = data.frame()
  
  for (i in 1:nfolds) {
    
    # create test set out of first fold and train set out of remaining 9 folds
    test_data <- dataset[folds[[i]], ]
    train_data <- dataset[-folds[[i]], ]
    
    # fit logistic regression model on train data
    model <- glm(formula = Y ~ X1 + X2 + X3 + X4, data = train_data, family = binomial(link = "logit")) 
    # output parameter estimates
    coefficients <- model$coefficients
    coefficients_df <- as.data.frame(coefficients)
    coefficients_df <- transpose(coefficients_df) 
    colnames(coefficients_df) <- c("estimate_intercept", "estimate_X1", "estimate_X2", "estimate_X3", "estimate_X4")

    # make predictions on train data to find optimal threshold for classification
    train_data$train_predicted_probabilities <- predict(model, train_data, type = "response")
    # find optimal threshold
    roc_J <- roc(response = train_data$Y, 
                 predictor = train_data$train_predicted_probabilities,
                 levels=c("0", "1"), quiet = TRUE) 
    J <- max(roc_J$sensitivities + roc_J$specificities - 1)
    
    # make predictions on test data
    test_data$test_predicted_probabilities <- predict(model, test_data, type = "response")
    # make classifications using optimal threshold
    test_data$test_predicted_classes <- ifelse(test_data$test_predicted_probabilities > J, "1", "0")
    
    ### prediction measures on test data
    
    # sensitivity = number of true positives / (number of true positives + number of false negatives)
    num_true_positive <- sum(test_data$Y == 1 & test_data$test_predicted_classes == 1)
    num_false_negative <- sum(test_data$Y == 1 & test_data$test_predicted_classes == 0)
    sensitivity <- num_true_positive / (num_true_positive + num_false_negative) 
    
    # specificity = number of true negatives / (number of true negatives + number of false positives)
    num_true_negative <- sum(test_data$Y == 0 & test_data$test_predicted_classes == 0)
    num_false_positive <- sum(test_data$Y == 0 & test_data$test_predicted_classes == 1)
    specificity <- num_true_negative / (num_true_negative + num_false_positive) 
    
    accuracy <- sum(test_data$Y == test_data$test_predicted_classes) / length(test_data$Y)
    roc <- roc(response = test_data$Y, 
               predictor = test_data$test_predicted_probabilities,
               levels=c("0", "1"), quiet = TRUE) # used to calculate auc
    auc <- roc$auc
    brier_score <- BrierScore(model)
    
    # combine prediction measures together for test data
    row_data = data.frame(sensitivity = sensitivity,
                          specificity = specificity,
                          accuracy = accuracy,
                          auc = auc,
                          brier_score = brier_score,
                          coefficients_df,
                          J)
    
    prediction_measures_df = rbind(prediction_measures_df, row_data)
    
  }
  
  # generate average of each prediction measure
  df <- transpose(as.data.frame(colMeans(prediction_measures_df[sapply(prediction_measures_df, is.numeric)])))
  colnames(df) <- colnames(prediction_measures_df)
  as.data.frame(df)
  
  # add proportion of cases & missingness information
  df$proportion_case <- dataset$proportion_case[1]
  df$proportion_X1_missing <- dataset$proportion_X1_missing[1]
  df$proportion_X4_missing <- dataset$proportion_X4_missing[1]
  df$auc_X1 <- dataset$auc_X1[1]
  df$auc_X4 <- dataset$auc_X4[1]
  
  df
  
}
