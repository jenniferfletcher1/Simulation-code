##############################################################################################
# generating dataframe of prediction measures & parameter estimates
# with corresponding standard errors & 95% CI's
##############################################################################################

averageing_predictions <- function(predictions_df){

  # take average of each numeric column
  average_predictions_df <- as.data.frame(t(as.data.frame(colMeans(predictions_df[sapply(predictions_df, is.numeric)]))))
  # add name of dataset
  average_predictions_df <- cbind(average_predictions_df, predictions_df$name_dataset[1]) 
  # update column names
  colnames(average_predictions_df) <- colnames(predictions_df)
  # remove row names
  rownames(average_predictions_df) <- NULL
  
  #################################
  # generate prediction measures
  # with standard errors & 95% CI
  #################################
  
  # sensitivity
  average_predictions_df$se_sensitivity <- standard_error(predictions_df$sensitivity)
  average_predictions_df$lower_CI_sensitivity <- se_95_CI_lower(mean = average_predictions_df$sensitivity, se = average_predictions_df$se_sensitivity)
  average_predictions_df$upper_CI_sensitivity <- se_95_CI_upper(mean = average_predictions_df$sensitivity, se = average_predictions_df$se_sensitivity)
 
  # specificity
  average_predictions_df$se_specificity <- standard_error(predictions_df$specificity)
  average_predictions_df$lower_CI_specificity <- se_95_CI_lower(mean = average_predictions_df$specificity, se = average_predictions_df$se_specificity)
  average_predictions_df$upper_CI_specificity <- se_95_CI_upper(mean = average_predictions_df$specificity, se = average_predictions_df$se_specificity)
  
  # accuracy
  average_predictions_df$se_accuracy <- standard_error(predictions_df$accuracy)
  average_predictions_df$lower_CI_accuracy <- se_95_CI_lower(mean = average_predictions_df$accuracy, se = average_predictions_df$se_accuracy)
  average_predictions_df$upper_CI_accuracy <- se_95_CI_upper(mean = average_predictions_df$accuracy, se = average_predictions_df$se_accuracy)
  
  # auc
  average_predictions_df$se_auc <- standard_error(predictions_df$auc)
  average_predictions_df$lower_CI_auc <- se_95_CI_lower(mean = average_predictions_df$auc, se = average_predictions_df$se_auc)
  average_predictions_df$upper_CI_auc <- se_95_CI_upper(mean = average_predictions_df$auc, se = average_predictions_df$se_auc)
  
  # brier score
  average_predictions_df$se_brier_score <- standard_error(predictions_df$brier_score)
  average_predictions_df$lower_CI_brier_score <- se_95_CI_lower(mean = average_predictions_df$brier_score, se = average_predictions_df$se_brier_score)
  average_predictions_df$upper_CI_brier_score <- se_95_CI_upper(mean = average_predictions_df$brier_score, se = average_predictions_df$se_brier_score)
  
  #################################
  # generating parameter estimates
  # with standard errors & 95% CI
  #################################
  
  # intercept
  average_predictions_df$se_estimate_intercept <- standard_error(predictions_df$estimate_intercept)
  average_predictions_df$lower_CI_estimate_intercept <- se_95_CI_lower(mean = average_predictions_df$estimate_intercept, se = average_predictions_df$se_estimate_intercept)
  average_predictions_df$upper_CI_estimate_intercept <- se_95_CI_upper(mean = average_predictions_df$estimate_intercept, se = average_predictions_df$se_estimate_intercept)
  
  # X1
  average_predictions_df$se_estimate_X1 <- standard_error(predictions_df$estimate_X1)
  average_predictions_df$lower_CI_estimate_X1 <- se_95_CI_lower(mean = average_predictions_df$estimate_X1, se = average_predictions_df$se_estimate_X1)
  average_predictions_df$upper_CI_estimate_X1 <- se_95_CI_upper(mean = average_predictions_df$estimate_X1, se = average_predictions_df$se_estimate_X1)
  
  # X2
  average_predictions_df$se_estimate_X2 <- standard_error(predictions_df$estimate_X2)
  average_predictions_df$lower_CI_estimate_X2 <- se_95_CI_lower(mean = average_predictions_df$estimate_X2, se = average_predictions_df$se_estimate_X2)
  average_predictions_df$upper_CI_estimate_X2 <- se_95_CI_upper(mean = average_predictions_df$estimate_X2, se = average_predictions_df$se_estimate_X2)
  
  # X3
  average_predictions_df$se_estimate_X3 <- standard_error(predictions_df$estimate_X3)
  average_predictions_df$lower_CI_estimate_X3 <- se_95_CI_lower(mean = average_predictions_df$estimate_X3, se = average_predictions_df$se_estimate_X3)
  average_predictions_df$upper_CI_estimate_X3 <- se_95_CI_upper(mean = average_predictions_df$estimate_X3, se = average_predictions_df$se_estimate_X3)
  
  # X4
  average_predictions_df$se_estimate_X4 <- standard_error(predictions_df$estimate_X4)
  average_predictions_df$lower_CI_estimate_X4 <- se_95_CI_lower(mean = average_predictions_df$estimate_X4, se = average_predictions_df$se_estimate_X4)
  average_predictions_df$upper_CI_estimate_X4 <- se_95_CI_upper(mean = average_predictions_df$estimate_X4, se = average_predictions_df$se_estimate_X4)
  
  ############################
  # final dataset
  ############################
  
  # set to data frame
  as.data.frame(average_predictions_df)  
  
  # update order of columns
  average_predictions_df <- average_predictions_df[,c("name_dataset", 
                                                      "n", 
                                                      "proportion_case", 
                                                      "J",
                                                      "proportion_X1_missing", "auc_X1", 
                                                      "proportion_X4_missing", "auc_X4",
                                                      "sensitivity", "se_sensitivity", "lower_CI_sensitivity", "upper_CI_sensitivity",
                                                      "specificity", "se_specificity", "lower_CI_specificity", "upper_CI_specificity",
                                                      "accuracy", "se_accuracy", "lower_CI_accuracy", "upper_CI_accuracy",
                                                      "auc", "se_auc", "lower_CI_auc", "upper_CI_auc",
                                                      "brier_score", "se_brier_score", "lower_CI_brier_score", "upper_CI_brier_score",
                                                      "estimate_intercept", "se_estimate_intercept", "lower_CI_estimate_intercept", "upper_CI_estimate_intercept",
                                                      "estimate_X1", "se_estimate_X1", "lower_CI_estimate_X1", "upper_CI_estimate_X1", 
                                                      "estimate_X2", "se_estimate_X2", "lower_CI_estimate_X2", "upper_CI_estimate_X2", 
                                                      "estimate_X3", "se_estimate_X3", "lower_CI_estimate_X3", "upper_CI_estimate_X3", 
                                                      "estimate_X4", "se_estimate_X4", "lower_CI_estimate_X4", "upper_CI_estimate_X4")]
  
  average_predictions_df
  
}

##############################################################################################
# function to round predictions 
##############################################################################################

rounding_average_predictions <- function(dataset){
  
  ### variables about the dataset
  
  dataset$n <- round(dataset$n, 0)
  dataset$proportion_case <- round(dataset$proportion_case, 2)
  dataset$proportion_X1_missing <- round(dataset$proportion_X1_missing, 2)
  dataset$auc_X1 <- round(dataset$auc_X1, 4)
  dataset$proportion_X4_missing <- round(dataset$proportion_X4_missing, 2)
  dataset$auc_X4 <- round(dataset$auc_X4, 4)
  dataset$J <- round(dataset$J, 2)
  
  ### prediction measures
  
  # sensitivity
  dataset$sensitivity <- round(dataset$sensitivity, 4)
  dataset$se_sensitivity <- signif(dataset$se_sensitivity, 3)
  dataset$lower_CI_sensitivity <- round(dataset$lower_CI_sensitivity, 4)
  dataset$upper_CI_sensitivity <- round(dataset$upper_CI_sensitivity, 4)
  # specificity
  dataset$specificity <- round(dataset$specificity, 4)
  dataset$se_specificity <- signif(dataset$se_specificity, 3)
  dataset$lower_CI_specificity <- round(dataset$lower_CI_specificity, 4)
  dataset$upper_CI_specificity <- round(dataset$upper_CI_specificity, 4)
  # accuracy
  dataset$accuracy <- round(dataset$accuracy, 4)
  dataset$se_accuracy <- signif(dataset$se_accuracy, 3)
  dataset$lower_CI_accuracy <- round(dataset$lower_CI_accuracy, 4)
  dataset$upper_CI_accuracy <- round(dataset$upper_CI_accuracy, 4)
  # auc
  dataset$auc <- round(dataset$auc, 4)
  dataset$se_auc <- signif(dataset$se_auc, 3)
  dataset$lower_CI_auc <- round(dataset$lower_CI_auc, 4)
  dataset$upper_CI_auc <- round(dataset$upper_CI_auc, 4)
  # brier_score
  dataset$brier_score <- round(dataset$brier_score, 4)
  dataset$se_brier_score <- signif(dataset$se_brier_score, 3)
  dataset$lower_CI_brier_score <- round(dataset$lower_CI_brier_score, 4)
  dataset$upper_CI_brier_score <- round(dataset$upper_CI_brier_score, 4)
  
  ### parameter estimates
  
  # intercept
  dataset$estimate_intercept <- round(dataset$estimate_intercept, 4)
  dataset$se_estimate_intercept <- signif(dataset$se_estimate_intercept, 3)
  dataset$lower_CI_estimate_intercept <- round(dataset$lower_CI_estimate_intercept, 4)
  dataset$upper_CI_estimate_intercept <- round(dataset$upper_CI_estimate_intercept, 4)
  # X1
  dataset$estimate_X1 <- round(dataset$estimate_X1, 4)
  dataset$se_estimate_X1 <- signif(dataset$se_estimate_X1, 3)
  dataset$lower_CI_estimate_X1 <- round(dataset$lower_CI_estimate_X1, 4)
  dataset$upper_CI_estimate_X1 <- round(dataset$upper_CI_estimate_X1, 4)
  # X2
  dataset$estimate_X2 <- round(dataset$estimate_X2, 4)
  dataset$se_estimate_X2 <- signif(dataset$se_estimate_X2, 3)
  dataset$lower_CI_estimate_X2 <- round(dataset$lower_CI_estimate_X2, 4)
  dataset$upper_CI_estimate_X2 <- round(dataset$upper_CI_estimate_X2, 4)
  # X3
  dataset$estimate_X3 <- round(dataset$estimate_X3, 4)
  dataset$se_estimate_X3 <- signif(dataset$se_estimate_X3, 3)
  dataset$lower_CI_estimate_X3 <- round(dataset$lower_CI_estimate_X3, 4)
  dataset$upper_CI_estimate_X3 <- round(dataset$upper_CI_estimate_X3, 4)
  # X4
  dataset$estimate_X4 <- round(dataset$estimate_X4, 4)
  dataset$se_estimate_X4 <- signif(dataset$se_estimate_X4, 3)
  dataset$lower_CI_estimate_X4 <- round(dataset$lower_CI_estimate_X4, 4)
  dataset$upper_CI_estimate_X4 <- round(dataset$upper_CI_estimate_X4, 4)
  
  dataset
  
}
  
  