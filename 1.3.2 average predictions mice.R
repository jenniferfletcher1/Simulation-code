##############################################################################################
# generating dataframe of prediction measures & parameter estimates
# with corresponding standard errors & 95% CI's
##############################################################################################

averageing_predictions_mice <- function(predictions_df){
  
  # take average of each numeric column
  average_predictions_df <- transpose(as.data.frame(colMeans(predictions_df[sapply(predictions_df, is.numeric)])))
  # add name of dataset
  average_predictions_df <- cbind(average_predictions_df, predictions_df$name_dataset[1]) 
  # update column names
  colnames(average_predictions_df) <- colnames(predictions_df)
  average_predictions_df
  
  # set to data frame
  as.data.frame(average_predictions_df)  
  
}
