library(readxl)
library(dplyr)
library(lubridate)
library(tidyr) # For separate_rows()
library(here)

# Read the dataset
pcr_data <- read_excel(here("data", "raw-data", "virus_iso.xlsx"))

# Add week number based on the collection data
# This code ensures that the ISO 8601 standard is applied to the week number calculation
# Week 1 of any year is the week that contains January 4th, or equivalently, it's the week that contains the first Thursday of January.
# Weeks start on Monday and end on Sunday.
# The last week of the year, week 52 or 53, is the one that contains December 28th.
# we will use week_num to join tables
# the format will be YYYY-WW. Week 8 of 2019 will look like: 2019-08
pcr_data <- pcr_data %>%
  mutate(
    # Directly format the collection date into "YYYY-WW"
    week_num = paste0(year(`Collection Date`), "-", 
                      sprintf("%02d", isoweek(`Collection Date`)))
  )


# Correctly expand "Both" in Test Type, then adjust to "EEE" and "WNV"
pcr_data_expanded <- pcr_data %>%
  mutate(Test_Type_Expanded = case_when(
    `Test Type` == "Both" ~ "EEE&WNV", # Temporarily mark "Both" for special handling
    TRUE ~ `Test Type`
  )) %>%
  separate_rows(Test_Type_Expanded, sep = "&") %>%
  mutate(
    Year = year(`Collection Date`),
    Week = week(`Collection Date`),
    Submitted_for_Testing = `Submitted for Testing` == "Y",
    # Assuming positive results are marked as "Positive" in `Result`
    Is_Positive = Result == "Positive"
  )

# Save as RDS
saveRDS(pcr_data_expanded, here("data", "processed-data", "rds", "pcr_data_expanded.rds"))

# Save as CSV
write_csv(pcr_data_expanded, here("data", "processed-data", "pcr_data_expanded.csv"))

# Filter records where Submitted for Testing is "Y"
pcr_data_expanded_filtered <- pcr_data_expanded %>%
  filter(Submitted_for_Testing)

# Save as RDS
saveRDS(pcr_data_expanded_filtered, here("data", "processed-data", "rds", "pcr_data_expanded_filtered.rds"))

# Save as CSV
write_csv(pcr_data_expanded_filtered, here("data", "processed-data", "pcr_data_expanded_filtered.csv"))

# Calculate MIR for each Test Type category ("EEE", "WNV", and both considered in each)
summary_data <- pcr_data_expanded_filtered %>%
  group_by(MCD, Year, Week, Town, Result, Test_Type_Expanded) %>%
  summarise(
    Pools_Tested = sum(Submitted_for_Testing, na.rm = TRUE),
    Total_Tested = sum(`Pool Size`, na.rm = TRUE),
    Positive_Pools = sum(Is_Positive, na.rm = TRUE), # Directly use Is_Positive
    MIR = (Positive_Pools / Total_Tested) * 1000,
    .groups = 'drop'
  )
 
# Print the summary
print(summary_data)

# First, calculate the total number of tests submitted for testing by week_num
total_tests_by_week <- pcr_data %>%
  filter(`Submitted for Testing` == "Y") %>%
  group_by(week_num) %>%
  summarise(Total_Tested = sum(`Pool Size`, na.rm = TRUE), .groups = 'drop')

# Now, expand the data for detailed analysis as before
pcr_data_expanded <- pcr_data %>%
  mutate(Test_Type_Expanded = case_when(
    `Test Type` == "Both" ~ "EEE&WNV",
    TRUE ~ `Test Type`
  )) %>%
  separate_rows(Test_Type_Expanded, sep = "&") %>%
  mutate(
    Submitted_for_Testing = `Submitted for Testing` == "Y",
    Is_Positive = Result == "Positive"
  ) %>%
  filter(Submitted_for_Testing)


#calculate MIR for all sites by week
MIR_by_week <- pcr_data_expanded %>%
  group_by(week_num) %>%
  summarise(
    Pools_Tested = sum(Submitted_for_Testing, na.rm = TRUE),
    Positive_Pools = sum(Is_Positive, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  left_join(total_tests_by_week, by = "week_num") %>%
  mutate(
    MIR = (Positive_Pools / Total_Tested) * 1000,
    # Extract Year and Week from week_num
    Year = as.numeric(str_extract(week_num, "^[0-9]{4}")),
    Week = as.numeric(str_extract(week_num, "(?<=-)[0-9]{2}$")) # Extract two digits after the hyphen
  )

# Save as CSV
write_csv(MIR_by_week, here("data", "processed-data", "MIR_by_week.csv"))

# Save as RDS
saveRDS(MIR_by_week, here("data", "processed-data", "rds", "MIR_by_week.rds"))

#CALCULATE MIR BY TRAP TYPE
# Define the trap types to include
trap_types_to_include <- c("500cc CO2 - CDC Miniature Light Trap", "Dry Ice CO2 - CDC Miniature Light Trap with CO2")

# Filter pcr_data for specific CO2 baited light traps
filtered_pcr_data_co2 <- pcr_data %>%
  filter(`Trap Type` %in% trap_types_to_include) %>%
  mutate(
    Submitted_for_Testing = ifelse(`Submitted for Testing` == "Y", 1, 0), # Convert to numeric
    Is_Positive = ifelse(Result == "Positive", 1, 0) # Ensure Is_Positive is numeric
  )

# Assuming total_tests_by_week is correctly prepared and contains 'Total_Tested' as numeric

# Calculate MIR for CO2 baited traps by week
MIR_by_week_co2 <- filtered_pcr_data_co2 %>%
  group_by(week_num) %>%
  summarise(
    Pools_Tested = sum(Submitted_for_Testing, na.rm = TRUE),
    Positive_Pools = sum(Is_Positive, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  left_join(total_tests_by_week, by = "week_num") %>%
  mutate(
    MIR = ifelse(Total_Tested > 0, (Positive_Pools / Total_Tested) * 1000, NA_real_) # Avoid division by zero, produce NA if Total_Tested is 0
  )

# Save the calculated MIR for CO2 baited traps by week
write_csv(MIR_by_week_co2, here("data", "processed-data", "MIR_by_week_co2.csv"))
saveRDS(MIR_by_week_co2, here("data", "processed-data", "rds", "MIR_by_week_co2.rds"))

# MIR BY MCD
# First, calculate the total number of tests submitted for testing by week_num
total_tests_by_week_mcd <- pcr_data %>%
  filter(`Submitted for Testing` == "Y") %>%
  group_by(week_num, MCD) %>%
  summarise(Total_Tested = sum(`Pool Size`, na.rm = TRUE), .groups = 'drop')

MIR_by_week_mcd <- pcr_data_expanded %>%
  group_by(week_num, MCD) %>%
  summarise(
    Pools_Tested = sum(Submitted_for_Testing, na.rm = TRUE),
    Positive_Pools = sum(Is_Positive, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  left_join(total_tests_by_week_mcd, by = c("week_num", "MCD")) %>%
  mutate(
    MIR = (Positive_Pools / Total_Tested) * 1000,
    Year = as.numeric(str_extract(week_num, "^[0-9]{4}")),
    Week = as.numeric(str_extract(week_num, "(?<=-)[0-9]{2}$"))
  )
# Save as CSV
write_csv(MIR_by_week_mcd, here("data", "processed-data", "MIR_by_week_mcd.csv"))

# Save as RDS
saveRDS(MIR_by_week_mcd, here("data", "processed-data", "rds", "MIR_by_week_mcd.rds"))

# Create a function to generate a heatmap of MIR by week
create_heatmap <- function(MIR_by_week) {
  library(ggplot2)
  library(RColorBrewer)
  
  MIR_by_week_filtered <- MIR_by_week %>%
    mutate(Week = as.numeric(Week)) %>%  # Ensure that Week is numeric for filtering
    filter(Week >= 26 & Week <= 42)
  
  MIR_by_week_heatmap <- ggplot(MIR_by_week_filtered, aes(x = factor(Week), y = factor(Year), fill = MIR)) +
    geom_tile(color = "white", size = 0.5) +  # Add borders to tiles for clarity
    scale_fill_gradientn(colors = brewer.pal(8, "Reds")) +  # Use a color palette from RColorBrewer
    scale_x_discrete(limits = as.character(26:42)) +  # Set discrete limits for weeks 25 to 45
    labs(
      title = "Weekly MIR from Week 26 to 42 from 2007 to 2023",
      x = "Week of the Year",
      y = "Year",
      fill = "MIR"
    ) +
    theme_minimal(base_size = 14) +  # Increase base font size for minimal theme
    theme(
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),  # Rotate x-axis labels for better readability
      axis.title = element_text(size = 16),  # Increase size of axis titles
      plot.title = element_text(size = 20, hjust = 0.5),  # Increase size of plot title
      legend.title = element_text(size = 14),  # Adjust size of legend title
      legend.text = element_text(size = 12)  # Adjust size of legend text
    )
  
  return(MIR_by_week_heatmap)
}

# Save this function as an RDS file
saveRDS(create_heatmap, here("data", "processed-data", "rds", "create_heatmap_function.rds"))

