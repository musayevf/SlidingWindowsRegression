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

