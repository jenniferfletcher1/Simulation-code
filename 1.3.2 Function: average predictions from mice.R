##############################################################################################
# function to generate average prediction scores across all imputations
##############################################################################################

averageing_predictions_mice <- function(predictions_df){
  
  # take average of each numeric column
  average_predictions_df <- t(as.data.frame(colMeans(predictions_df[sapply(predictions_df, is.numeric)])))
  # add name of dataset
  average_predictions_df <- cbind(average_predictions_df, predictions_df$name_dataset[1]) 
  # update column names
  colnames(average_predictions_df) <- colnames(predictions_df)
  # remove row names
  rownames(average_predictions_df) <- NULL
  
  # set to data frame
  average_predictions_df <- as.data.frame(average_predictions_df)
  average_predictions_df
  
}
