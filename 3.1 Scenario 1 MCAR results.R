####################################################################################################
# import datasets
####################################################################################################

# set working directory
setwd("/Users/jennyfletcher/Desktop/LSHTM/Summer Project/PWV Data/Dataset/Simulation study R code /Scenario 1 complete datasets")

# import: full dataset
data_full <- read.csv("predictions_df_full_MCAR1.csv")

# import: CCA dataset
data_CCA <- read.csv("predictions_df_CCA_MCAR1.csv")

# import: mean/mode dataset
data_mode_mean <- read.csv("predictions_df_mode_mean_MCAR1.csv")

# import: mice1 dataset
data_mice1 <- read.csv("predictions_df_mice_1_MCAR1.csv")

# import: mice10 dataset
data_mice10 <- read.csv("predictions_df_mice_10_MCAR1.csv")

# import: mice40 dataset
data_mice40 <- read.csv("predictions_df_mice_40_MCAR1.csv")

########################################################################################################
# averaging datasets: generates the average of results from all simulations
########################################################################################################

#######################################################
# generate df that contains averages for all datasets
#######################################################

# raw averages of each dataset
average_data_full <- averageing_predictions(predictions_df = data_full)
average_data_CCA <- averageing_predictions(predictions_df = data_CCA)
average_data_mode_mean <- averageing_predictions(predictions_df = data_mode_mean)
average_data_mice1 <- averageing_predictions(predictions_df = data_mice1)
average_data_mice10 <- averageing_predictions(predictions_df = data_mice10)
average_data_mice40 <- averageing_predictions(predictions_df = data_mice40)

# bind averages to create df of averages
average_results <- rbind(average_data_full, average_data_CCA, average_data_mode_mean, average_data_mice1, average_data_mice10, average_data_mice40)

# round averages
round_average_results <- rounding_average_predictions(dataset = average_results)

# convert auc_X1 & auc_X4 to NA as more appropriate (mechanism is not applicable for MCAR rather than 0)
round_average_results$auc_X1 <- NA
round_average_results$auc_X4 <- NA

# print results
round_average_results

# export results
write.csv(x = round_average_results , file = "average_results_MCAR1.csv")


