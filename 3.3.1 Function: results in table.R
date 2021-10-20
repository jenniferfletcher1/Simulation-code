############################################################################################################################################
# function: generates df with average performance measures with corresponding se's & difference in means with the full dataset 
############################################################################################################################################

dataset_for_table <- function(dataset_averages, dataset_difference_means){

############################################################################################################################################
# data averages  
############################################################################################################################################
  
# prediction measures & se's
data_averages <- dataset_averages[c("name_dataset", "n", 
                                      "sensitivity", "se_sensitivity",
                                      "specificity", "se_specificity",
                                      "accuracy", "se_accuracy",
                                      "auc", "se_auc",
                                      "brier_score", "se_brier_score")]

# add row names
rownames(data_averages) <- data_averages$name_dataset

# full dataset
se_full <- data_averages[c("full"),c("name_dataset", "n", "se_sensitivity", "se_specificity", "se_accuracy", "se_auc","se_brier_score")]
se_full[,"name_dataset"] <- "full_se"
rownames(se_full) <- NULL
colnames(se_full) <- c("name_dataset","n", "sensitivity", "specificity", "accuracy", "auc","brier_score")
se_full

# CCA dataset
se_CCA <- data_averages[c("CCA"),c("name_dataset", "n", "se_sensitivity", "se_specificity", "se_accuracy", "se_auc","se_brier_score")]
se_CCA[,"name_dataset"] <- "CCA_se"
rownames(se_CCA) <- NULL
colnames(se_CCA) <- c("name_dataset", "n", "sensitivity", "specificity", "accuracy", "auc","brier_score")
se_CCA

# mean/mode dataset
se_mode_mean <- data_averages[c("mode/mean"),c("name_dataset", "n", "se_sensitivity", "se_specificity", "se_accuracy", "se_auc","se_brier_score")]
se_mode_mean[,"name_dataset"] <- "mode_mean_se"
rownames(se_mode_mean) <- NULL
colnames(se_mode_mean) <- c("name_dataset", "n","sensitivity", "specificity", "accuracy", "auc","brier_score")
se_mode_mean

# mice1 dataset
se_mice1 <- data_averages[c("mice 1"),c("name_dataset", "n", "se_sensitivity", "se_specificity", "se_accuracy", "se_auc","se_brier_score")]
se_mice1[,"name_dataset"] <- "mice1_se"
rownames(se_mice1) <- NULL
colnames(se_mice1) <- c("name_dataset", "n","sensitivity", "specificity", "accuracy", "auc","brier_score")
se_mice1

# mice10 dataset
se_mice10 <- data_averages[c("mice 10"),c("name_dataset","n", "se_sensitivity", "se_specificity", "se_accuracy", "se_auc","se_brier_score")]
se_mice10[,"name_dataset"] <- "mice10_se"
rownames(se_mice10) <- NULL
colnames(se_mice10) <- c("name_dataset", "n","sensitivity", "specificity", "accuracy", "auc","brier_score")
se_mice10

# mice40 dataset
se_mice40 <- data_averages[c("mice 40"),c("name_dataset", "n", "se_sensitivity", "se_specificity", "se_accuracy", "se_auc","se_brier_score")]
se_mice40[,"name_dataset"] <- "mice40_se"
rownames(se_mice40) <- NULL
colnames(se_mice40) <- c("name_dataset","n", "sensitivity", "specificity", "accuracy", "auc","brier_score")
se_mice40

# extract prediction measures
data_prediction_measures <- data_averages[c("name_dataset", "n", 
                                                    "sensitivity",
                                                    "specificity",
                                                    "accuracy",
                                                    "auc",
                                                    "brier_score")]

# bind with standard errors
predictions <- rbind(data_prediction_measures, se_full, se_CCA, se_mode_mean, se_mice1, se_mice10, se_mice40)

############################################################################################################################################
# data difference in means  
############################################################################################################################################

# difference in means & se's
data_difference_means <- dataset_difference_means[c("name_dataset", "n", 
                                                    "difference_sensitivity",
                                                    "difference_specificity",
                                                    "difference_accuracy",
                                                    "difference_auc", 
                                                    "difference_brier_score")]

# add row names
rownames(data_difference_means) <- data_difference_means$name_dataset

# full dataset
difference_full <- data_difference_means[c("full"),c("name_dataset", "n", "difference_sensitivity", "difference_specificity", "difference_accuracy", "difference_auc","difference_brier_score")]
difference_full[,"name_dataset"] <- "difference_full"
rownames(difference_full) <- NULL
colnames(difference_full) <- c("name_dataset","n", "sensitivity", "specificity", "accuracy", "auc","brier_score")
difference_full

# CCA dataset
difference_CCA <- data_difference_means[c("CCA"),c("name_dataset", "n", "difference_sensitivity", "difference_specificity", "difference_accuracy", "difference_auc","difference_brier_score")]
difference_CCA[,"name_dataset"] <- "difference_CCA"
rownames(difference_CCA) <- NULL
colnames(difference_CCA) <- c("name_dataset", "n", "sensitivity", "specificity", "accuracy", "auc","brier_score")
difference_CCA

# mean/mode dataset
difference_mode_mean <- data_difference_means[c("mode/mean"),c("name_dataset", "n", "difference_sensitivity", "difference_specificity", "difference_accuracy", "difference_auc","difference_brier_score")]
difference_mode_mean[,"name_dataset"] <- "difference_mode_mean"
rownames(difference_mode_mean) <- NULL
colnames(difference_mode_mean) <- c("name_dataset", "n","sensitivity", "specificity", "accuracy", "auc","brier_score")
difference_mode_mean

# mice1 dataset
difference_mice1 <- data_difference_means[c("mice 1"),c("name_dataset", "n", "difference_sensitivity", "difference_specificity", "difference_accuracy", "difference_auc","difference_brier_score")]
difference_mice1[,"name_dataset"] <- "difference_mice1"
rownames(difference_mice1) <- NULL
colnames(difference_mice1) <- c("name_dataset", "n","sensitivity", "specificity", "accuracy", "auc","brier_score")
difference_mice1

# mice10 dataset
difference_mice10 <- data_difference_means[c("mice 10"),c("name_dataset", "n", "difference_sensitivity", "difference_specificity", "difference_accuracy", "difference_auc","difference_brier_score")]
difference_mice10[,"name_dataset"] <- "difference_mice10"
rownames(difference_mice10) <- NULL
colnames(difference_mice10) <- c("name_dataset", "n","sensitivity", "specificity", "accuracy", "auc","brier_score")
difference_mice10

# mice40 dataset
difference_mice40 <- data_difference_means[c("mice 40"),c("name_dataset", "n", "difference_sensitivity", "difference_specificity", "difference_accuracy", "difference_auc","difference_brier_score")]
difference_mice40[,"name_dataset"] <- "difference_mice40"
rownames(difference_mice40) <- NULL
colnames(difference_mice40) <- c("name_dataset","n", "sensitivity", "specificity", "accuracy", "auc","brier_score")
difference_mice40

# bind together
differences <- rbind(difference_full, difference_CCA, difference_mode_mean, difference_mice1, difference_mice10, difference_mice40)

############################################################################################################################################
# data difference in means se
############################################################################################################################################

# difference in means & se's
data_difference_means_se <- dataset_difference_means[c("name_dataset", "n", 
                                                        "difference_sensitivity_se",
                                                        "difference_specificity_se",
                                                        "difference_accuracy_se",
                                                        "difference_auc_se", 
                                                        "difference_brier_score_se")]

# add row names
rownames(data_difference_means_se) <- data_difference_means_se$name_dataset

# full dataset
difference_full_se <- data_difference_means_se[c("full"),c("name_dataset", "n", "difference_sensitivity_se", "difference_specificity_se", "difference_accuracy_se", "difference_auc_se","difference_brier_score_se")]
difference_full_se[,"name_dataset"] <- "difference_full_se"
rownames(difference_full_se) <- NULL
colnames(difference_full_se) <- c("name_dataset","n", "sensitivity", "specificity", "accuracy", "auc","brier_score")
difference_full_se

# CCA dataset
difference_CCA_se <- data_difference_means_se[c("CCA"),c("name_dataset", "n", "difference_sensitivity_se", "difference_specificity_se", "difference_accuracy_se", "difference_auc_se","difference_brier_score_se")]
difference_CCA_se[,"name_dataset"] <- "difference_CCA_se"
rownames(difference_CCA_se) <- NULL
colnames(difference_CCA_se) <- c("name_dataset", "n", "sensitivity", "specificity", "accuracy", "auc","brier_score")
difference_CCA_se

# mean/mode dataset
difference_mode_mean_se <- data_difference_means_se[c("mode/mean"),c("name_dataset", "n", "difference_sensitivity_se", "difference_specificity_se", "difference_accuracy_se", "difference_auc_se","difference_brier_score_se")]
difference_mode_mean_se[,"name_dataset"] <- "difference_mode_mean_se"
rownames(difference_mode_mean_se) <- NULL
colnames(difference_mode_mean_se) <- c("name_dataset", "n","sensitivity", "specificity", "accuracy", "auc","brier_score")
difference_mode_mean_se

# mice1 dataset
difference_mice1_se <- data_difference_means_se[c("mice 1"),c("name_dataset", "n", "difference_sensitivity_se", "difference_specificity_se", "difference_accuracy_se", "difference_auc_se","difference_brier_score_se")]
difference_mice1_se[,"name_dataset"] <- "difference_mice1_se"
rownames(difference_mice1_se) <- NULL
colnames(difference_mice1_se) <- c("name_dataset", "n","sensitivity", "specificity", "accuracy", "auc","brier_score")
difference_mice1_se

# mice10 dataset
difference_mice10_se <- data_difference_means_se[c("mice 10"),c("name_dataset", "n", "difference_sensitivity_se", "difference_specificity_se", "difference_accuracy_se", "difference_auc_se","difference_brier_score_se")]
difference_mice10_se[,"name_dataset"] <- "difference_mice10_se"
rownames(difference_mice10_se) <- NULL
colnames(difference_mice10_se) <- c("name_dataset", "n","sensitivity", "specificity", "accuracy", "auc","brier_score")
difference_mice10_se

# mice40 dataset
difference_mice40_se <- data_difference_means_se[c("mice 40"),c("name_dataset", "n", "difference_sensitivity_se", "difference_specificity_se", "difference_accuracy_se", "difference_auc_se","difference_brier_score_se")]
difference_mice40_se[,"name_dataset"] <- "difference_mice40_se"
rownames(difference_mice40_se) <- NULL
colnames(difference_mice40_se) <- c("name_dataset","n", "sensitivity", "specificity", "accuracy", "auc","brier_score")
difference_mice40_se

# bind together
differences_se <- rbind(difference_full_se, difference_CCA_se, difference_mode_mean_se, difference_mice1_se, difference_mice10_se, difference_mice40_se)

############################################################################################################################################
# bind all together
############################################################################################################################################

# bind results together
df <- rbind(predictions, differences, differences_se)
rownames(df) <- df$name_dataset

# change ordering of rows
df <- df[c("full", "full_se", "difference_full", "difference_full_se",
           "CCA", "CCA_se", "difference_CCA", "difference_CCA_se",
           "mode/mean", "mode_mean_se", "difference_mode_mean", "difference_mode_mean_se",
           "mice 1", "mice1_se", "difference_mice1", "difference_mice1_se",
           "mice 10", "mice10_se", "difference_mice10", "difference_mice10_se",
           "mice 40", "mice40_se", "difference_mice40", "difference_mice40_se"),]

# round n
df$n <- round(df$n, 0)

df

}
