#Importing packages and data
source("packages.R")
source("functions/load_data.R")
source("functions/evaluate_model.R")
source("functions/mathematical.R")

seasons <- read.csv('C:/Users/99451/Desktop/Thesis/SeasonData/Aggregated_Months_Growing_Dormancy_Probability.csv')
data_1344 = load_data(
  data_directory = 'C:/Users/99451/Desktop/Thesis/1344.Rdata',
  gridcode = 1344)
data_2152 = load_data(
  data_directory = 'C:/Users/99451/Desktop/Thesis/2152.Rdata',
  gridcode = 2152)
data_2183 = load_data(
  data_directory = 'C:/Users/99451/Desktop/Thesis/2183.Rdata',
  gridcode = 2183)

df_1344 = data_1344$full_data #Loading watershed 1344
raining_1344 = data_1344$raining_data
nonraining_1344 = data_1344$nonraining_data
df_2152 = data_2152$full_data #Loading watershed 2152
raining_2152 = data_2152$raining_data
nonraining_2152 = data_2152$nonraining_data
df_2183 = data_2183$full_data #Loading watershed 2183
raining_2183 = data_2183$raining_data
nonraining_2183 = data_2183$nonraining_data

# Splitting datasets to training and testing
dataset_names <- c("raining_1344", "nonraining_1344", "raining_2152", "nonraining_2152", 
                   "raining_2183", "nonraining_2183")
train_dfs <- list()
test_dfs <- list()

# Loop through each dataset name and split into train & test
for (name in dataset_names) {
  df <- get(name)  # Get dataset from environment
  
  # Split into train (hydr_year <= 30) and test (hydr_year > 30)
  train_dfs[[name]] <- df[df$hydr_year <= 30, ]
  test_dfs[[name]] <- df[df$hydr_year > 30, ]
}

#Training Models (For Different Seasons)
set.seed(42)
models <- list()

# Loop through each dataset and train the model
for (name in dataset_names) {
  train_df <- train_dfs[[name]]  # Get the train dataset
  
  # Train the model
  models[[name]] <- trainSWR(train_df$rain,
                             train_df$gauge,
                             iter = 3,
                             param_selection = "best_bic")
}

#Evaluating Models
results <- do.call(rbind, list(
  evaluate_model(models$raining_1344, test_dfs$raining_1344, "1344", "raining"),
  evaluate_model(models$nonraining_1344, test_dfs$nonraining_1344, "1344", "nonraining"),
  evaluate_model(models$raining_2152, test_dfs$raining_2152, "2152", "raining"),
  evaluate_model(models$nonraining_2152, test_dfs$nonraining_2152, "2152", "nonraining"),
  evaluate_model(models$raining_2183, test_dfs$raining_2183, "2183", "raining"),
  evaluate_model(models$nonraining_2183, test_dfs$nonraining_2183, "2183", "nonraining")
))

knitr::kable(results, caption = "Evaluation Metrics for All Models")

#Plotting An Example Model
plot(models$raining_1344, 
     type = "prediction",
     reference = test_dfs$raining_1344$gauge,
     newdata = test_dfs$raining_1344$rain)

#Adding spline interpolation to fill missing vegetation values and normalization
df_1344 <- spline_interpolation(df_1344, subset(seasons, gridcode == 1344), normalize_values = TRUE)
df_2152 <- spline_interpolation(df_2152, subset(seasons, gridcode == 2152), normalize_values = TRUE)
df_2183 <- spline_interpolation(df_2183, subset(seasons, gridcode == 2183), normalize_values = TRUE)
knitr::kable(head(df_1344), caption = "Watershed 1344")

#Dividing datasets into high and low vegetation models (x < -0.95 & x > 0.95)
df_1344_low <- subset(df_1344, seasonal_value_filled <= -0.95)
df_1344_high <- subset(df_1344, seasonal_value_filled >= 0.95)
df_2152_low <- subset(df_2152, seasonal_value_filled <= -0.95)
df_2152_high <- subset(df_2152, seasonal_value_filled >= 0.95)
df_2183_low <- subset(df_2183, seasonal_value_filled <= -0.95)
df_2183_high <- subset(df_2183, seasonal_value_filled >= 0.95)

#Training high and low vegetation models
model_1344_high <- train_model(df_1344_high, "hydr_year", 30)
model_1344_low <- train_model(df_1344_low, "hydr_year", 30)
model_2152_high <- train_model(df_2152_high, "hydr_year", 30)
model_2152_low <- train_model(df_2152_low, "hydr_year", 30)
model_2183_high <- train_model(df_2183_high, "hydr_year", 30)
model_2183_low <- train_model(df_2183_low, "hydr_year", 30)

#Evaluating Models
high_low_results <- do.call(rbind, list(
  evaluate_model(model_1344_high, df_1344_high[df_1344_high$hydr_year > 30, ], "1344", "high"),
  evaluate_model(model_1344_low, df_1344_low[df_1344_low$hydr_year > 30, ], "1344", "low"),
  evaluate_model(model_2152_high, df_2152_high[df_2152_high$hydr_year > 30, ], "2152", "high"),
  evaluate_model(model_2152_low, df_2152_low[df_2152_low$hydr_year > 30, ], "2152", "low"),
  evaluate_model(model_2183_high, df_2183_high[df_2183_high$hydr_year > 30, ], "2183", "high"),
  evaluate_model(model_2183_low, df_2183_low[df_2183_low$hydr_year > 30, ], "2183", "low")
))
knitr::kable(results, caption = "Evaluation Metrics for Low and High Models")
plot(model_1344_high, #Visualizing example model
     type = "prediction",
     reference = df_1344_high[df_1344_high$hydr_year > 30, ]$gauge,
     newdata = df_1344_high[df_1344_high$hydr_year > 30, ]$rain)

#Continuing with Combining Models according to KD Similarity
divergence_matrix_1344 <- compare_models_kld(model_1344_high, model_1344_low, 3)
divergence_matrix_2152 <- compare_models_kld(model_2152_high, model_2152_low, 3)
divergence_matrix_2183 <- compare_models_kld(model_2183_high, model_2183_low, 3)

# for each alpha in df_high$seasonal_value_norm, compute and store the matrix for df_high
df_1344_high$params <- lapply(df_1344_high$seasonal_value_norm,
                              function(a) combine_params(divergence_matrix_1344$param_matrix, alpha = a))
df_2152_high$params <- lapply(df_2152_high$seasonal_value_norm,
                              function(a) combine_params(divergence_matrix_2152$param_matrix, alpha = a))
df_2183_high$params <- lapply(df_2183_high$seasonal_value_norm,
                              function(a) combine_params(divergence_matrix_2183$param_matrix, alpha = a))

# for each alpha in df_low$seasonal_value_norm, compute and store the matrix for df_low
df_1344_low$params <- lapply(df_1344_low$seasonal_value_norm,
                             function(a) combine_params(divergence_matrix_1344$param_matrix, alpha = a))
df_2152_low$params <- lapply(df_2152_low$seasonal_value_norm,
                             function(a) combine_params(divergence_matrix_2152$param_matrix, alpha = a))
df_2183_low$params <- lapply(df_2183_low$seasonal_value_norm,
                             function(a) combine_params(divergence_matrix_2183$param_matrix, alpha = a))

# Adding SWR() objects as new columns, assume df_high$params is your list-column of delta/sigma/beta matrices
df_1344_high$swr <- lapply(df_1344_high$params, function(mat) {
  # mat is the param-matrix for one row, with columns delta, sigma, beta
  param <- mat[, c("delta", "sigma"), drop = FALSE]
  mix   <- mat[, "beta"]
  createSWR(param = param, mix = mix)
})

df_2152_high$swr <- lapply(df_2152_high$params, function(mat) {
  # mat is the param-matrix for one row, with columns delta, sigma, beta
  param <- mat[, c("delta", "sigma"), drop = FALSE]
  mix   <- mat[, "beta"]
  createSWR(param = param, mix = mix)
})

df_2183_high$swr <- lapply(df_2183_high$params, function(mat) {
  # mat is the param-matrix for one row, with columns delta, sigma, beta
  param <- mat[, c("delta", "sigma"), drop = FALSE]
  mix   <- mat[, "beta"]
  createSWR(param = param, mix = mix)
})

df_1344_low$swr <- lapply(df_1344_low$params, function(mat) {
  # mat is the param-matrix for one row, with columns delta, sigma, beta
  param <- mat[, c("delta", "sigma"), drop = FALSE]
  mix   <- mat[, "beta"]
  createSWR(param = param, mix = mix)
})

df_2152_low$swr <- lapply(df_2152_low$params, function(mat) {
  # mat is the param-matrix for one row, with columns delta, sigma, beta
  param <- mat[, c("delta", "sigma"), drop = FALSE]
  mix   <- mat[, "beta"]
  createSWR(param = param, mix = mix)
})

df_2183_low$swr <- lapply(df_2183_low$params, function(mat) {
  # mat is the param-matrix for one row, with columns delta, sigma, beta
  param <- mat[, c("delta", "sigma"), drop = FALSE]
  mix   <- mat[, "beta"]
  createSWR(param = param, mix = mix)
})

#Adding new predictions using SWR() objects
df_1344_high <- add_new_predictions(df_1344_high)
df_2152_high <- add_new_predictions(df_2152_high)
df_2183_high <- add_new_predictions(df_2183_high)
df_1344_low <- add_new_predictions(df_1344_low)
df_2152_low <- add_new_predictions(df_2152_low)
df_2183_low <- add_new_predictions(df_2183_low)

#Evaluating New Predictions
high_low_results <- do.call(rbind, list(
  extract_metrics(eval_all(high_test$new_pred, high_test$gauge)),
  evaluate_model(model_1344_low, df_1344_low[df_1344_low$hydr_year > 30, ], "1344", "low"),
  evaluate_model(model_2152_high, df_2152_high[df_2152_high$hydr_year > 30, ], "2152", "high"),
  evaluate_model(model_2152_low, df_2152_low[df_2152_low$hydr_year > 30, ], "2152", "low"),
  evaluate_model(model_2183_high, df_2183_high[df_2183_high$hydr_year > 30, ], "2183", "high"),
  evaluate_model(model_2183_low, df_2183_low[df_2183_low$hydr_year > 30, ], "2183", "low")
))
