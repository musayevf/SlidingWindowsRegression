#Importing packages and data
source("packages.R")
source("functions/load_data.R")

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
head(df_2183)
