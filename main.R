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
  new_evaluate_model(df_1344_high, "1344_high"),
  new_evaluate_model(df_1344_low,  "1344_low"),
  new_evaluate_model(df_2152_high, "2152_high"),
  new_evaluate_model(df_2152_low,  "2152_low"),
  new_evaluate_model(df_2183_high, "2183_high"),
  new_evaluate_model(df_2183_low,  "2183_low")
))
knitr::kable(high_low_results, caption = "Evaluation Metrics for Low and High Models Trained on SWR() Kernels")

#Training Vanilla Models
model_1344 <- trainSWR(df_1344[df_1344$hydr_year <= 30, ]$rain,
                       df_1344[df_1344$hydr_year <= 30, ]$gauge,
                       iter = 3, param_selection = "best_bic")
pred_1344 <- predict(model_1344, newdata = df_1344[df_1344$hydr_year > 30, ]$rain)

model_2152 <- trainSWR(df_2152[df_2152$hydr_year <= 30, ]$rain,
                       df_2152[df_2152$hydr_year <= 30, ]$gauge,
                       iter = 3, param_selection = "best_bic")
pred_2152 <- predict(model_2152, newdata = df_2152[df_2152$hydr_year > 30, ]$rain)

model_2183 <- trainSWR(df_2183[df_2183$hydr_year <= 30, ]$rain,
                       df_2183[df_2183$hydr_year <= 30, ]$gauge,
                       iter = 3, param_selection = "best_bic")
pred_2183 <- predict(model_2183, newdata = df_2183[df_2183$hydr_year > 30, ]$rain)

#Training SWR() Models for whole dataset
df_1344$params <- lapply(df_1344$seasonal_value_norm,
                        function(a) combine_params(divergence_matrix_1344$param_matrix, alpha = a))
df_2152$params <- lapply(df_2152$seasonal_value_norm,
                         function(a) combine_params(divergence_matrix_2152$param_matrix, alpha = a))
df_2183$params <- lapply(df_2183$seasonal_value_norm,
                         function(a) combine_params(divergence_matrix_2183$param_matrix, alpha = a))

#creating swr objects from parameters of the datasets
df_1344$swr <- lapply(df_1344$params, function(mat) {
  # mat is the param-matrix for one row, with columns delta, sigma, beta
  param <- mat[, c("delta", "sigma"), drop = FALSE]
  mix   <- mat[, "beta"]
  createSWR(param = param, mix = mix)})
df_2152$swr <- lapply(df_2152$params, function(mat) {
  # mat is the param-matrix for one row, with columns delta, sigma, beta
  param <- mat[, c("delta", "sigma"), drop = FALSE]
  mix   <- mat[, "beta"]
  createSWR(param = param, mix = mix)})
df_2183$swr <- lapply(df_2183$params, function(mat) {
  # mat is the param-matrix for one row, with columns delta, sigma, beta
  param <- mat[, c("delta", "sigma"), drop = FALSE]
  mix   <- mat[, "beta"]
  createSWR(param = param, mix = mix)})

#Adding new predictions
df_1344 <- add_new_predictions(df_1344)
df_2152 <- add_new_predictions(df_2152)
df_2183 <- add_new_predictions(df_2183)

#Collecting Metrics for both Vanilla and Combined Models
test_1344 <- subset(df_1344, hydr_year > 30)
metrics_1344 <- extract_metrics(eval_all(test_1344$new_pred, test_1344$gauge))
test_2152 <- subset(df_2152, hydr_year > 30)
metrics_2152 <- extract_metrics(eval_all(test_2152$new_pred, test_2152$gauge))
test_2183 <- subset(df_2183, hydr_year > 30)
metrics_2183 <- extract_metrics(eval_all(test_2183$new_pred, test_2183$gauge))
#Combined Models Metrics
model_1344_metrics <- extract_metrics(eval_all(pred_1344, test_1344$gauge))
model_2152_metrics <- extract_metrics(eval_all(pred_2152, test_2152$gauge))
model_2183_metrics <- extract_metrics(eval_all(pred_2183, test_2183$gauge))

#Displaying Model Metrics
#Evaluating New Predictions
# List of variable names
model_vars <- c("metrics_1344", "model_1344_metrics","metrics_2152",
                "model_2152_metrics","metrics_2183", "model_2183_metrics")
model_names <- c("Vanilla 1344", "Combined 1344", "Vanilla 2152", 
                 "Combined 2152", "Vanilla 2183", "Combined 2183")

# Build data frame for each and combine
vanilla_combined <- do.call(rbind, lapply(seq_along(model_vars), function(i) {
  metrics <- get(model_vars[i])  # get the variable by name
  data.frame(model = model_names[i], t(metrics), check.names = FALSE)
}))
knitr::kable(vanilla_combined, caption = "Comparison of Vanilla and Combined Models")

#Combining all metrics tables together (high, low, combined, vanilla)
names(high_low_results)[names(high_low_results) == "Model"] <- "model"
final_results <- cbind(vanilla_combined, high_low_results)
knitr::kable(final_results, caption = "High, Low, Combined and Vanilla Models")

#Visualizing results
analyze_residuals(df_1344)

