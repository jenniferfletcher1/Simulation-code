####################################################################################################
# import datasets
####################################################################################################

# set working directory for import of 1000 predictions generated
setwd("")

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

# print results
round_average_results

# export results
write.csv(x = round_average_results , file = "average_results_MCAR1.csv")

#######################################################################################################
# difference in means (compared to full dataset) df
#######################################################################################################

difference_in_means_df <- predictions_difference_in_means(dataset_means_all = average_results, 
                                                          dataset_predictions_full = data_full, 
                                                          dataset_predictions_CCA = data_CCA,
                                                          dataset_predictions_mode_mean = data_mode_mean,
                                                          dataset_predictions_mice1 = data_mice1, 
                                                          dataset_predictions_mice10 = data_mice10,
                                                          dataset_predictions_mice40 = data_mice40)

########################################################################################################
# export table of average performance measures & difference in means
########################################################################################################

data_table <- dataset_for_table(dataset_averages = average_results, dataset_difference_means = difference_in_means_df)

# export table as doc
stargazer(data_table,
          summary=FALSE, 
          rownames=FALSE, 
          type = "html", out = "table.doc",
          digits = 4)

#######################################################################################################
# plot average performance measures
#######################################################################################################

# plot
plot_results <- annotate_figure(prediction_measures_plots(dataset = average_results),
                                top = text_grob("Scenario 1: MCAR", face = "bold", size = 14))

# export plot
pdf(file="plot_results.pdf")
plot_results
dev.off()


