library(here)
library(readr)
library(dplyr)
library(purrr)

# Define the function to get column details
get_column_details <- function(file) {
  data <- read_csv(file, guess_max = 1000, show_col_types = FALSE)
  tibble(
    FileName = basename(file),
    ColumnName = names(data),
    DataType = sapply(data, function(x) class(x)[1]), # Ensures consistent data type as character
    SampleValue = sapply(data, function(column) toString(column[1])) # First non-NA value
  )
}

# Set the path to the directory containing the CSV files
path_to_files <- here("data", "processed-data")

# List all CSV files in the directory but exclude the "rds" folder
file_list <- list.files(path = path_to_files, pattern = "\\.csv$", full.names = TRUE)
file_list <- file_list[!grepl("/rds/", file_list)] # Excludes files in "rds" subfolder

# Apply the function to each file and combine the results
data_dictionary <- map_df(file_list, get_column_details)

# Save the data dictionary to a CSV file
write_csv(data_dictionary, here("data", "processed-data", "Data_Dictionary.csv"))
