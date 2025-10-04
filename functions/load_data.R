#loads data and relevant season information
load_data <- function(
  data_directory = 'C:/Users/99451/Desktop/Thesis/1344.Rdata',
  seasons_directory = 'C:/Users/99451/Desktop/Thesis/SeasonData/Aggregated_Months_Growing_Dormancy_Probability.csv',
  gridcode = 1344) {
  
  df <- get(load(data_directory))
  seasons <- read.csv(seasons_directory)
  seasons_df <- subset(seasons, gridcode == gridcode)
  binary_df <- ifelse(seasons_df[1, -1] < 0, 1, 0) #(1 if dormancy < 0, else 0) raining or non-vegatative
  names(binary_df) <- names(seasons_df)[-1] # Name the elements of the vector with the corresponding month names
  df$month <- format(df$date, "%b") # Extract month abbreviations
  df$seasonal <- binary_df[df$month] # Creating seasonal binary (0/1) columns
  # Dividing dataset into raining (1) and non-raining seasons
  raining_df <- df[df$seasonal == 1,]
  nonraining_df <- df[df$seasonal == 0,]
  return (list(full_data = df, raining_data = raining_df, nonraining_data = nonraining_df))
  }