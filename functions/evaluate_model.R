#Trains model upon selected criteria
train_model <- function(data, filter_col, threshold, iter = 3, param_selection = "best_bic") {
  # Get indices where filter_col <= threshold
  train_inds <- which(data[[filter_col]] <= threshold)
  
  # Train the model
  model <- trainSWR(data$rain[train_inds], data$gauge[train_inds], 
                    iter = iter, param_selection = param_selection)
  return(model)
}

#Evaluates and returns model performance metrics for a given dataset and rain condition.
evaluate_model <- function(model, test_data, dataset_name, rain_status) {
  preds <- predict(model, newdata = test_data$rain)
  metrics <- extract_metrics(eval_all(preds, test_data$gauge))  # already a named numeric vector
  
  df <- as.data.frame(t(metrics)) # transpose → metrics as columns
  df$Model <- paste(dataset_name, rain_status)
  
  # reorder to put "Model" first
  df <- df[, c("Model", setdiff(names(df), "Model"))]
  return(df)
}

# Function to extract and convert list values to a numeric vector
extract_metrics <- function(metrics_list) {
  # Extract values from the list (assuming `metrics_list` contains named elements)
  return(unlist(metrics_list))
}

#New function to evaluate between new_pred and gauge values
new_evaluate_model <- function(df, model_name) {
  # Filter data
  df_sub <- df[df$hydr_year > 30, ]
  
  # Evaluate predictions
  metrics <- extract_metrics(eval_all(df_sub$new_pred, df_sub$gauge))
  
  # Ensure metrics are one row
  metrics_df <- as.data.frame(t(metrics))
  
  # Add model name as first column
  metrics_df <- cbind(model = model_name, metrics_df)
  
  return(metrics_df)
}
