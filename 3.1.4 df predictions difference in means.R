################################################################################################################
# generate difference in means for performance measures between each dataset and full dataset 
################################################################################################################

predictions_difference_in_means <- function(dataset_means_all, 
                                            dataset_predictions_full, 
                                            dataset_predictions_CCA,
                                            dataset_predictions_mode_mean,
                                            dataset_predictions_mice1, 
                                            dataset_predictions_mice10,
                                            dataset_predictions_mice40){
  
  # label rows
  rownames(dataset_means_all) <- c("full", "CCA", "mode/mean", "mice 1", "mice 10", "mice 40")
  
  #####################################################
  ## generate difference in mean prediction measures
  #####################################################
  
  ############
  # full  
  ############
  
  ### sensitivity
  
  # difference in means
  difference_sensitivity_full <-  NA
  # se
  difference_sensitivity_se_full <- NA
  # 95% CI's
  difference_sensitivity_95CI_lower_full <- NA
  difference_sensitivity_95CI_upper_full <- NA
  
  ### specificity
  
  # difference in means
  difference_specificity_full <- NA
  # se
  difference_specificity_se_full <- NA
  # 95% CI's
  difference_specificity_95CI_lower_full <- NA
  difference_specificity_95CI_upper_full <- NA
  
  
  ### accuracy
  
  # difference in means
  difference_accuracy_full <-  NA
  # se
  difference_accuracy_se_full <- NA
  # 95% CI's
  difference_accuracy_95CI_lower_full <- NA
  difference_accuracy_95CI_upper_full <- NA
  
  ### auc
  
  # difference in means
  difference_auc_full <-  NA
  # se
  difference_auc_se_full <- NA
  # 95% CI's
  difference_auc_95CI_lower_full <- NA
  difference_auc_95CI_upper_full <- NA
  
  ### brier_score
  
  # difference in means
  difference_brier_score_full <-  NA
  # se
  difference_brier_score_se_full <- NA
  # 95% CI's
  difference_brier_score_95CI_lower_full <- NA
  difference_brier_score_95CI_upper_full <- NA
  
  # bind all results together for CCA
  differences_prediction_measures_full <- as.data.frame(cbind(difference_sensitivity = difference_sensitivity_full, 
                                                             difference_sensitivity_se = difference_sensitivity_se_full, 
                                                             difference_sensitivity_95CI_lower  = difference_sensitivity_95CI_lower_full, 
                                                             difference_sensitivity_95CI_upper = difference_sensitivity_95CI_upper_full,
                                                             difference_specificity = difference_specificity_full, 
                                                             difference_specificity_se = difference_specificity_se_full, 
                                                             difference_specificity_95CI_lower = difference_specificity_95CI_lower_full, 
                                                             difference_specificity_95CI_upper = difference_specificity_95CI_upper_full,
                                                             difference_accuracy = difference_accuracy_full, 
                                                             difference_accuracy_se = difference_accuracy_se_full, 
                                                             difference_accuracy_95CI_lower  = difference_accuracy_95CI_lower_full, 
                                                             difference_accuracy_95CI_upper = difference_accuracy_95CI_upper_full,
                                                             difference_auc = difference_auc_full, 
                                                             difference_auc_se = difference_auc_se_full, 
                                                             difference_auc_95CI_lower = difference_auc_95CI_lower_full, 
                                                             difference_auc_95CI_upper = difference_auc_95CI_upper_full,
                                                             difference_brier_score = difference_brier_score_full, 
                                                             difference_brier_score_se = difference_brier_score_se_full, 
                                                             difference_brier_score_95CI_lower = difference_brier_score_95CI_lower_full, 
                                                             difference_brier_score_95CI_upper = difference_brier_score_95CI_upper_full))
  
  rownames(differences_prediction_measures_full) <- c("full")
  
  ############
  # CCA 
  ############
  
  ### sensitivity
  
  # difference in means
  difference_sensitivity_CCA <-  dataset_means_all["CCA",]$sensitivity - dataset_means_all["full",]$sensitivity
  # se
  difference_sensitivity_se_CCA <- se_difference_means(x1 = dataset_predictions_CCA$sensitivity, x2 = dataset_predictions_full$sensitivity)
  # 95% CI's
  difference_sensitivity_95CI_lower_CCA <- se_95_CI_lower_difference_means(mean_difference = difference_sensitivity_CCA, se_difference_means = difference_sensitivity_se_CCA)
  difference_sensitivity_95CI_upper_CCA <- se_95_CI_upper_difference_means(mean_difference = difference_sensitivity_CCA, se_difference_means = difference_sensitivity_se_CCA)
  
  ### specificity
  
  # difference in means
  difference_specificity_CCA <-  dataset_means_all["CCA",]$specificity - dataset_means_all["full",]$specificity
  # se
  difference_specificity_se_CCA <- se_difference_means(x1 = dataset_predictions_CCA$specificity, x2 = dataset_predictions_full$specificity)
  # 95% CI's
  difference_specificity_95CI_lower_CCA <- se_95_CI_lower_difference_means(mean_difference = difference_specificity_CCA, se_difference_means = difference_specificity_se_CCA)
  difference_specificity_95CI_upper_CCA <- se_95_CI_upper_difference_means(mean_difference = difference_specificity_CCA, se_difference_means = difference_specificity_se_CCA)
  
  
  ### accuracy

  # difference in means
  difference_accuracy_CCA <-  dataset_means_all["CCA",]$accuracy - dataset_means_all["full",]$accuracy
  # se
  difference_accuracy_se_CCA <- se_difference_means(x1 = dataset_predictions_CCA$accuracy, x2 = dataset_predictions_full$accuracy)
  # 95% CI's
  difference_accuracy_95CI_lower_CCA <- se_95_CI_lower_difference_means(mean_difference = difference_accuracy_CCA, se_difference_means = difference_accuracy_se_CCA)
  difference_accuracy_95CI_upper_CCA <- se_95_CI_upper_difference_means(mean_difference = difference_accuracy_CCA, se_difference_means = difference_accuracy_se_CCA)
  
  ### auc

  # difference in means
  difference_auc_CCA <-  dataset_means_all["CCA",]$auc - dataset_means_all["full",]$auc
  # se
  difference_auc_se_CCA <- se_difference_means(x1 = dataset_predictions_CCA$auc, x2 = dataset_predictions_full$auc)
  # 95% CI's
  difference_auc_95CI_lower_CCA <- se_95_CI_lower_difference_means(mean_difference = difference_auc_CCA, se_difference_means = difference_auc_se_CCA)
  difference_auc_95CI_upper_CCA <- se_95_CI_upper_difference_means(mean_difference = difference_auc_CCA, se_difference_means = difference_auc_se_CCA)
  
  ### brier_score

  # difference in means
  difference_brier_score_CCA <-  dataset_means_all["CCA",]$brier_score - dataset_means_all["full",]$brier_score
  # se
  difference_brier_score_se_CCA <- se_difference_means(x1 = dataset_predictions_CCA$brier_score, x2 = dataset_predictions_full$brier_score)
  # 95% CI's
  difference_brier_score_95CI_lower_CCA <- se_95_CI_lower_difference_means(mean_difference = difference_brier_score_CCA, se_difference_means = difference_brier_score_se_CCA)
  difference_brier_score_95CI_upper_CCA <- se_95_CI_upper_difference_means(mean_difference = difference_brier_score_CCA, se_difference_means = difference_brier_score_se_CCA)
  
  # bind all results together for CCA
  differences_prediction_measures_CCA <- as.data.frame(cbind(difference_sensitivity = difference_sensitivity_CCA, 
                                                             difference_sensitivity_se = difference_sensitivity_se_CCA, 
                                                             difference_sensitivity_95CI_lower  = difference_sensitivity_95CI_lower_CCA, 
                                                             difference_sensitivity_95CI_upper = difference_sensitivity_95CI_upper_CCA,
                                                             difference_specificity = difference_specificity_CCA, 
                                                             difference_specificity_se = difference_specificity_se_CCA, 
                                                             difference_specificity_95CI_lower = difference_specificity_95CI_lower_CCA, 
                                                             difference_specificity_95CI_upper = difference_specificity_95CI_upper_CCA,
                                                             difference_accuracy = difference_accuracy_CCA, 
                                                             difference_accuracy_se = difference_accuracy_se_CCA, 
                                                             difference_accuracy_95CI_lower  = difference_accuracy_95CI_lower_CCA, 
                                                             difference_accuracy_95CI_upper = difference_accuracy_95CI_upper_CCA,
                                                             difference_auc = difference_auc_CCA, 
                                                             difference_auc_se = difference_auc_se_CCA, 
                                                             difference_auc_95CI_lower = difference_auc_95CI_lower_CCA, 
                                                             difference_auc_95CI_upper = difference_auc_95CI_upper_CCA,
                                                             difference_brier_score = difference_brier_score_CCA, 
                                                             difference_brier_score_se = difference_brier_score_se_CCA, 
                                                             difference_brier_score_95CI_lower = difference_brier_score_95CI_lower_CCA, 
                                                             difference_brier_score_95CI_upper = difference_brier_score_95CI_upper_CCA))
  
  rownames(differences_prediction_measures_CCA) <- c("CCA")
  
  ############
  # mode/mean 
  ############
  
  ### sensitivity
  
  # difference in means
  difference_sensitivity_mode_mean <-  dataset_means_all["mode/mean",]$sensitivity - dataset_means_all["full",]$sensitivity
  # se
  difference_sensitivity_se_mode_mean <- se_difference_means(x1 = dataset_predictions_mode_mean$sensitivity, x2 = dataset_predictions_full$sensitivity)
  # 95% CI's
  difference_sensitivity_95CI_lower_mode_mean <- se_95_CI_lower_difference_means(mean_difference = difference_sensitivity_mode_mean, se_difference_means = difference_sensitivity_se_mode_mean)
  difference_sensitivity_95CI_upper_mode_mean <- se_95_CI_upper_difference_means(mean_difference = difference_sensitivity_mode_mean, se_difference_means = difference_sensitivity_se_mode_mean)
  
  ### specificity
  
  # difference in means
  difference_specificity_mode_mean <-  dataset_means_all["mode/mean",]$specificity - dataset_means_all["full",]$specificity
  # se
  difference_specificity_se_mode_mean <- se_difference_means(x1 = dataset_predictions_mode_mean$specificity, x2 = dataset_predictions_full$specificity)
  # 95% CI's
  difference_specificity_95CI_lower_mode_mean <- se_95_CI_lower_difference_means(mean_difference = difference_specificity_mode_mean, se_difference_means = difference_specificity_se_mode_mean)
  difference_specificity_95CI_upper_mode_mean <- se_95_CI_upper_difference_means(mean_difference = difference_specificity_mode_mean, se_difference_means = difference_specificity_se_mode_mean)
  
  ### accuracy
  
  # difference in means
  difference_accuracy_mode_mean <-  dataset_means_all["mode/mean",]$accuracy - dataset_means_all["full",]$accuracy
  # se
  difference_accuracy_se_mode_mean <- se_difference_means(x1 = dataset_predictions_mode_mean$accuracy, x2 = dataset_predictions_full$accuracy)
  # 95% CI's
  difference_accuracy_95CI_lower_mode_mean <- se_95_CI_lower_difference_means(mean_difference = difference_accuracy_mode_mean, se_difference_means = difference_accuracy_se_mode_mean)
  difference_accuracy_95CI_upper_mode_mean <- se_95_CI_upper_difference_means(mean_difference = difference_accuracy_mode_mean, se_difference_means = difference_accuracy_se_mode_mean)
  
  ### auc
  
  # difference in means
  difference_auc_mode_mean <-  dataset_means_all["mode/mean",]$auc - dataset_means_all["full",]$auc
  # se
  difference_auc_se_mode_mean <- se_difference_means(x1 = dataset_predictions_mode_mean$auc, x2 = dataset_predictions_full$auc)
  # 95% CI's
  difference_auc_95CI_lower_mode_mean <- se_95_CI_lower_difference_means(mean_difference = difference_auc_mode_mean, se_difference_means = difference_auc_se_mode_mean)
  difference_auc_95CI_upper_mode_mean <- se_95_CI_upper_difference_means(mean_difference = difference_auc_mode_mean, se_difference_means = difference_auc_se_mode_mean)
  
  ### brier_score
  
  # difference in means
  difference_brier_score_mode_mean <-  dataset_means_all["mode/mean",]$brier_score - dataset_means_all["full",]$brier_score
  # se
  difference_brier_score_se_mode_mean <- se_difference_means(x1 = dataset_predictions_mode_mean$brier_score, x2 = dataset_predictions_full$brier_score)
  # 95% CI's
  difference_brier_score_95CI_lower_mode_mean <- se_95_CI_lower_difference_means(mean_difference = difference_brier_score_mode_mean, se_difference_means = difference_brier_score_se_mode_mean)
  difference_brier_score_95CI_upper_mode_mean <- se_95_CI_upper_difference_means(mean_difference = difference_brier_score_mode_mean, se_difference_means = difference_brier_score_se_mode_mean)
  
  # bind all results together for mode/mean
  differences_prediction_measures_mode_mean <- as.data.frame(cbind(difference_sensitivity = difference_sensitivity_mode_mean, 
                                                                     difference_sensitivity_se = difference_sensitivity_se_mode_mean, 
                                                                     difference_sensitivity_95CI_lower = difference_sensitivity_95CI_lower_mode_mean, 
                                                                     difference_sensitivity_95CI_upper = difference_sensitivity_95CI_upper_mode_mean,
                                                                     difference_specificity = difference_specificity_mode_mean, 
                                                                     difference_specificity_se = difference_specificity_se_mode_mean, 
                                                                     difference_specificity_95CI_lower = difference_specificity_95CI_lower_mode_mean, 
                                                                     difference_specificity_95CI_upper = difference_specificity_95CI_upper_mode_mean,
                                                                     difference_accuracy = difference_accuracy_mode_mean, 
                                                                     difference_accuracy_se = difference_accuracy_se_mode_mean, 
                                                                     difference_accuracy_95CI_lower = difference_accuracy_95CI_lower_mode_mean, 
                                                                     difference_accuracy_95CI_upper = difference_accuracy_95CI_upper_mode_mean,
                                                                     difference_auc = difference_auc_mode_mean, 
                                                                     difference_auc_se = difference_auc_se_mode_mean, 
                                                                     difference_auc_95CI_lower = difference_auc_95CI_lower_mode_mean, 
                                                                     difference_auc_95CI_upper = difference_auc_95CI_upper_mode_mean,
                                                                     difference_brier_score = difference_brier_score_mode_mean, 
                                                                     difference_brier_score_se = difference_brier_score_se_mode_mean, 
                                                                     difference_brier_score_95CI_lower = difference_brier_score_95CI_lower_mode_mean, 
                                                                     difference_brier_score_95CI_upper = difference_brier_score_95CI_upper_mode_mean))
                  
  rownames(differences_prediction_measures_mode_mean) <- c("mode/mean")
  
  ############
  # mice 1 
  ############
  
  ### sensitivity
  
  # difference in means
  difference_sensitivity_mice1 <-  dataset_means_all["mice 1",]$sensitivity - dataset_means_all["full",]$sensitivity
  # se
  difference_sensitivity_se_mice1 <- se_difference_means(x1 = dataset_predictions_mice1$sensitivity, x2 = dataset_predictions_full$sensitivity)
  # 95% CI's
  difference_sensitivity_95CI_lower_mice1 <- se_95_CI_lower_difference_means(mean_difference = difference_sensitivity_mice1, se_difference_means = difference_sensitivity_se_mice1)
  difference_sensitivity_95CI_upper_mice1 <- se_95_CI_upper_difference_means(mean_difference = difference_sensitivity_mice1, se_difference_means = difference_sensitivity_se_mice1)
  
  ### specificity
  
  # difference in means
  difference_specificity_mice1 <-  dataset_means_all["mice 1",]$specificity - dataset_means_all["full",]$specificity
  # se
  difference_specificity_se_mice1 <- se_difference_means(x1 = dataset_predictions_mice1$specificity, x2 = dataset_predictions_full$specificity)
  # 95% CI's
  difference_specificity_95CI_lower_mice1 <- se_95_CI_lower_difference_means(mean_difference = difference_specificity_mice1, se_difference_means = difference_specificity_se_mice1)
  difference_specificity_95CI_upper_mice1 <- se_95_CI_upper_difference_means(mean_difference = difference_specificity_mice1, se_difference_means = difference_specificity_se_mice1)
  
  ### accuracy
  
  # difference in means
  difference_accuracy_mice1 <-  dataset_means_all["mice 1",]$accuracy - dataset_means_all["full",]$accuracy
  # se
  difference_accuracy_se_mice1 <- se_difference_means(x1 = dataset_predictions_mice1$accuracy, x2 = dataset_predictions_full$accuracy)
  # 95% CI's
  difference_accuracy_95CI_lower_mice1 <- se_95_CI_lower_difference_means(mean_difference = difference_accuracy_mice1, se_difference_means = difference_accuracy_se_mice1)
  difference_accuracy_95CI_upper_mice1 <- se_95_CI_upper_difference_means(mean_difference = difference_accuracy_mice1, se_difference_means = difference_accuracy_se_mice1)
  
  ### auc
  
  # difference in means
  difference_auc_mice1 <-  dataset_means_all["mice 1",]$auc - dataset_means_all["full",]$auc
  # se
  difference_auc_se_mice1 <- se_difference_means(x1 = dataset_predictions_mice1$auc, x2 = dataset_predictions_full$auc)
  # 95% CI's
  difference_auc_95CI_lower_mice1 <- se_95_CI_lower_difference_means(mean_difference = difference_auc_mice1, se_difference_means = difference_auc_se_mice1)
  difference_auc_95CI_upper_mice1 <- se_95_CI_upper_difference_means(mean_difference = difference_auc_mice1, se_difference_means = difference_auc_se_mice1)
  
  ### brier_score
  
  # difference in means
  difference_brier_score_mice1 <-  dataset_means_all["mice 1",]$brier_score - dataset_means_all["full",]$brier_score
  # se
  difference_brier_score_se_mice1 <- se_difference_means(x1 = dataset_predictions_mice1$brier_score, x2 = dataset_predictions_full$brier_score)
  # 95% CI's
  difference_brier_score_95CI_lower_mice1 <- se_95_CI_lower_difference_means(mean_difference = difference_brier_score_mice1, se_difference_means = difference_brier_score_se_mice1)
  difference_brier_score_95CI_upper_mice1 <- se_95_CI_upper_difference_means(mean_difference = difference_brier_score_mice1, se_difference_means = difference_brier_score_se_mice1)
  
  # bind all results together for mice1
  differences_prediction_measures_mice1 <- as.data.frame(cbind(difference_sensitivity = difference_sensitivity_mice1, 
                                                                 difference_sensitivity_se = difference_sensitivity_se_mice1, 
                                                                 difference_sensitivity_95CI_lower = difference_sensitivity_95CI_lower_mice1, 
                                                                 difference_sensitivity_95CI_upper = difference_sensitivity_95CI_upper_mice1,
                                                                 difference_specificity = difference_specificity_mice1, 
                                                                 difference_specificity_se = difference_specificity_se_mice1, 
                                                                 difference_specificity_95CI_lower = difference_specificity_95CI_lower_mice1, 
                                                                 difference_specificity_95CI_upper = difference_specificity_95CI_upper_mice1,
                                                                 difference_accuracy = difference_accuracy_mice1, 
                                                                 difference_accuracy_se = difference_accuracy_se_mice1, 
                                                                 difference_accuracy_95CI_lower = difference_accuracy_95CI_lower_mice1, 
                                                                 difference_accuracy_95CI_upper = difference_accuracy_95CI_upper_mice1,
                                                                 difference_auc = difference_auc_mice1, 
                                                                 difference_auc_se = difference_auc_se_mice1, 
                                                                 difference_auc_95CI_lower = difference_auc_95CI_lower_mice1, 
                                                                 difference_auc_95CI_upper = difference_auc_95CI_upper_mice1,
                                                                 difference_brier_score = difference_brier_score_mice1, 
                                                                 difference_brier_score_se = difference_brier_score_se_mice1, 
                                                                 difference_brier_score_95CI_lower = difference_brier_score_95CI_lower_mice1, 
                                                                 difference_brier_score_95CI_upper = difference_brier_score_95CI_upper_mice1))
                  
  rownames(differences_prediction_measures_mice1) <- c("mice 1")
  
  ############
  # mice 10 
  ############
  
  ### sensitivity
  
  # difference in means
  difference_sensitivity_mice10 <-  dataset_means_all["mice 10",]$sensitivity - dataset_means_all["full",]$sensitivity
  # se
  difference_sensitivity_se_mice10 <- se_difference_means(x1 = dataset_predictions_mice10$sensitivity, x2 = dataset_predictions_full$sensitivity)
  # 95% CI's
  difference_sensitivity_95CI_lower_mice10 <- se_95_CI_lower_difference_means(mean_difference = difference_sensitivity_mice10, se_difference_means = difference_sensitivity_se_mice10)
  difference_sensitivity_95CI_upper_mice10 <- se_95_CI_upper_difference_means(mean_difference = difference_sensitivity_mice10, se_difference_means = difference_sensitivity_se_mice10)
  
  ### specificity
  
  # difference in means
  difference_specificity_mice10 <-  dataset_means_all["mice 10",]$specificity - dataset_means_all["full",]$specificity
  # se
  difference_specificity_se_mice10 <- se_difference_means(x1 = dataset_predictions_mice10$specificity, x2 = dataset_predictions_full$specificity)
  # 95% CI's
  difference_specificity_95CI_lower_mice10 <- se_95_CI_lower_difference_means(mean_difference = difference_specificity_mice10, se_difference_means = difference_specificity_se_mice10)
  difference_specificity_95CI_upper_mice10 <- se_95_CI_upper_difference_means(mean_difference = difference_specificity_mice10, se_difference_means = difference_specificity_se_mice10)
  
  ### accuracy
  
  # difference in means
  difference_accuracy_mice10 <-  dataset_means_all["mice 10",]$accuracy - dataset_means_all["full",]$accuracy
  # se
  difference_accuracy_se_mice10 <- se_difference_means(x1 = dataset_predictions_mice10$accuracy, x2 = dataset_predictions_full$accuracy)
  # 95% CI's
  difference_accuracy_95CI_lower_mice10 <- se_95_CI_lower_difference_means(mean_difference = difference_accuracy_mice10, se_difference_means = difference_accuracy_se_mice10)
  difference_accuracy_95CI_upper_mice10 <- se_95_CI_upper_difference_means(mean_difference = difference_accuracy_mice10, se_difference_means = difference_accuracy_se_mice10)
  
  ### auc
  
  # difference in means
  difference_auc_mice10 <-  dataset_means_all["mice 10",]$auc - dataset_means_all["full",]$auc
  # se
  difference_auc_se_mice10 <- se_difference_means(x1 = dataset_predictions_mice10$auc, x2 = dataset_predictions_full$auc)
  # 95% CI's
  difference_auc_95CI_lower_mice10 <- se_95_CI_lower_difference_means(mean_difference = difference_auc_mice10, se_difference_means = difference_auc_se_mice10)
  difference_auc_95CI_upper_mice10 <- se_95_CI_upper_difference_means(mean_difference = difference_auc_mice10, se_difference_means = difference_auc_se_mice10)
  
  ### brier_score
  
  # difference in means
  difference_brier_score_mice10 <-  dataset_means_all["mice 10",]$brier_score - dataset_means_all["full",]$brier_score
  # se
  difference_brier_score_se_mice10 <- se_difference_means(x1 = dataset_predictions_mice10$brier_score, x2 = dataset_predictions_full$brier_score)
  # 95% CI's
  difference_brier_score_95CI_lower_mice10 <- se_95_CI_lower_difference_means(mean_difference = difference_brier_score_mice10, se_difference_means = difference_brier_score_se_mice10)
  difference_brier_score_95CI_upper_mice10 <- se_95_CI_upper_difference_means(mean_difference = difference_brier_score_mice10, se_difference_means = difference_brier_score_se_mice10)
  
  # bind all results together for mice10
  differences_prediction_measures_mice10 <- as.data.frame(cbind(difference_sensitivity = difference_sensitivity_mice10, 
                                                                  difference_sensitivity_se = difference_sensitivity_se_mice10, 
                                                                  difference_sensitivity_95CI_lower = difference_sensitivity_95CI_lower_mice10, 
                                                                  difference_sensitivity_95CI_upper = difference_sensitivity_95CI_upper_mice10,
                                                                  difference_specificity = difference_specificity_mice10, 
                                                                  difference_specificity_se = difference_specificity_se_mice10, 
                                                                  difference_specificity_95CI_lower = difference_specificity_95CI_lower_mice10, 
                                                                  difference_specificity_95CI_upper = difference_specificity_95CI_upper_mice10,
                                                                  difference_accuracy = difference_accuracy_mice10, 
                                                                  difference_accuracy_se = difference_accuracy_se_mice10, 
                                                                  difference_accuracy_95CI_lower = difference_accuracy_95CI_lower_mice10, 
                                                                  difference_accuracy_95CI_upper = difference_accuracy_95CI_upper_mice10,
                                                                  difference_auc = difference_auc_mice10, 
                                                                  difference_auc_se = difference_auc_se_mice10, 
                                                                  difference_auc_95CI_lower = difference_auc_95CI_lower_mice10, 
                                                                  difference_auc_95CI_upper = difference_auc_95CI_upper_mice10,
                                                                  difference_brier_score = difference_brier_score_mice10, 
                                                                  difference_brier_score_se = difference_brier_score_se_mice10, 
                                                                  difference_brier_score_95CI_lower = difference_brier_score_95CI_lower_mice10, 
                                                                  difference_brier_score_95CI_upper = difference_brier_score_95CI_upper_mice10))
                                        
  rownames(differences_prediction_measures_mice10) <- c("mice 10")
  
  ############
  # mice 40 
  ############
  
  ### sensitivity
  
  # difference in means
  difference_sensitivity_mice40 <-  dataset_means_all["mice 40",]$sensitivity - dataset_means_all["full",]$sensitivity
  # se
  difference_sensitivity_se_mice40 <- se_difference_means(x1 = dataset_predictions_mice40$sensitivity, x2 = dataset_predictions_full$sensitivity)
  # 95% CI's
  difference_sensitivity_95CI_lower_mice40 <- se_95_CI_lower_difference_means(mean_difference = difference_sensitivity_mice40, se_difference_means = difference_sensitivity_se_mice40)
  difference_sensitivity_95CI_upper_mice40 <- se_95_CI_upper_difference_means(mean_difference = difference_sensitivity_mice40, se_difference_means = difference_sensitivity_se_mice40)
  
  ### specificity
  
  # difference in means
  difference_specificity_mice40 <-  dataset_means_all["mice 40",]$specificity - dataset_means_all["full",]$specificity
  # se
  difference_specificity_se_mice40 <- se_difference_means(x1 = dataset_predictions_mice40$specificity, x2 = dataset_predictions_full$specificity)
  # 95% CI's
  difference_specificity_95CI_lower_mice40 <- se_95_CI_lower_difference_means(mean_difference = difference_specificity_mice40, se_difference_means = difference_specificity_se_mice40)
  difference_specificity_95CI_upper_mice40 <- se_95_CI_upper_difference_means(mean_difference = difference_specificity_mice40, se_difference_means = difference_specificity_se_mice40)
  
  ### accuracy
  
  # difference in means
  difference_accuracy_mice40 <-  dataset_means_all["mice 40",]$accuracy - dataset_means_all["full",]$accuracy
  # se
  difference_accuracy_se_mice40 <- se_difference_means(x1 = dataset_predictions_mice40$accuracy, x2 = dataset_predictions_full$accuracy)
  # 95% CI's
  difference_accuracy_95CI_lower_mice40 <- se_95_CI_lower_difference_means(mean_difference = difference_accuracy_mice40, se_difference_means = difference_accuracy_se_mice40)
  difference_accuracy_95CI_upper_mice40 <- se_95_CI_upper_difference_means(mean_difference = difference_accuracy_mice40, se_difference_means = difference_accuracy_se_mice40)
  
  ### auc
  
  # difference in means
  difference_auc_mice40 <-  dataset_means_all["mice 40",]$auc - dataset_means_all["full",]$auc
  # se
  difference_auc_se_mice40 <- se_difference_means(x1 = dataset_predictions_mice40$auc, x2 = dataset_predictions_full$auc)
  # 95% CI's
  difference_auc_95CI_lower_mice40 <- se_95_CI_lower_difference_means(mean_difference = difference_auc_mice40, se_difference_means = difference_auc_se_mice40)
  difference_auc_95CI_upper_mice40 <- se_95_CI_upper_difference_means(mean_difference = difference_auc_mice40, se_difference_means = difference_auc_se_mice40)
  
  ### brier_score
  
  # difference in means
  difference_brier_score_mice40 <-  dataset_means_all["mice 40",]$brier_score - dataset_means_all["full",]$brier_score
  # se
  difference_brier_score_se_mice40 <- se_difference_means(x1 = dataset_predictions_mice40$brier_score, x2 = dataset_predictions_full$brier_score)
  # 95% CI's
  difference_brier_score_95CI_lower_mice40 <- se_95_CI_lower_difference_means(mean_difference = difference_brier_score_mice40, se_difference_means = difference_brier_score_se_mice40)
  difference_brier_score_95CI_upper_mice40 <- se_95_CI_upper_difference_means(mean_difference = difference_brier_score_mice40, se_difference_means = difference_brier_score_se_mice40)
  
  # bind all results together for mice100
  differences_prediction_measures_mice40 <- as.data.frame(cbind(difference_sensitivity = difference_sensitivity_mice40, 
                                                                   difference_sensitivity_se = difference_sensitivity_se_mice40, 
                                                                   difference_sensitivity_95CI_lower = difference_sensitivity_95CI_lower_mice40, 
                                                                   difference_sensitivity_95CI_upper = difference_sensitivity_95CI_upper_mice40,
                                                                   difference_specificity = difference_specificity_mice40, 
                                                                   difference_specificity_se = difference_specificity_se_mice40, 
                                                                   difference_specificity_95CI_lower = difference_specificity_95CI_lower_mice40, 
                                                                   difference_specificity_95CI_upper = difference_specificity_95CI_upper_mice40,
                                                                   difference_accuracy = difference_accuracy_mice40, 
                                                                   difference_accuracy_se = difference_accuracy_se_mice40, 
                                                                   difference_accuracy_95CI_lower = difference_accuracy_95CI_lower_mice40, 
                                                                   difference_accuracy_95CI_upper = difference_accuracy_95CI_upper_mice40,
                                                                   difference_auc = difference_auc_mice40, 
                                                                   difference_auc_se = difference_auc_se_mice40, 
                                                                   difference_auc_95CI_lower = difference_auc_95CI_lower_mice40, 
                                                                   difference_auc_95CI_upper = difference_auc_95CI_upper_mice40,
                                                                   difference_brier_score = difference_brier_score_mice40, 
                                                                   difference_brier_score_se = difference_brier_score_se_mice40, 
                                                                   difference_brier_score_95CI_lower = difference_brier_score_95CI_lower_mice40, 
                                                                   difference_brier_score_95CI_upper = difference_brier_score_95CI_upper_mice40))
                                        
  rownames(differences_prediction_measures_mice40) <- c("mice 40")
  
  
  ##########################################################
  # mean in prediction measures: bind all datasets together
  ##########################################################
  
  differences_prediction_measures_all <- rbind(differences_prediction_measures_full,
                                               differences_prediction_measures_CCA, 
                                                 differences_prediction_measures_mode_mean, 
                                                 differences_prediction_measures_mice1, 
                                                 differences_prediction_measures_mice10, 
                                                 differences_prediction_measures_mice40)

  
  
  
  differences_prediction_measures_all <- cbind(name_dataset = dataset_means_all$name_dataset, n = dataset_means_all$n, differences_prediction_measures_all)
  differences_prediction_measures_all
}
  
  



