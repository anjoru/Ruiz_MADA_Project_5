# Load necessary libraries for data manipulation, date handling, and file path management
library(lubridate)
library(here)
library(dplyr)
library(ggplot2)
library(pscl)
library(MASS)
library(readr)
library(purrr)


# Load the datasets:
MIR_by_week_co2 <- read_csv(here("data", "processed-data", "MIR_by_week_co2.csv"))
wx_selected <- read_csv(here("data", "processed-data", "wx_selected.csv"))
mosquito_wide <- read_csv(here("data", "processed-data", "mosquito_wide_all.csv"))
drought_fema1_4 <- read_csv(here("data", "processed-data", "drought_fema1_4.csv"))
drought_ma_counties <- read_csv(here("data", "processed-data", "drought_ma_counties.csv"))
#average_moon_phase_by_week <- read_csv(here("data", "processed-data", "average_moon_phase_by_week.csv"))
average_daylight_by_week <- read_csv(here("data", "processed-data", "average_daylight_by_week.csv"))
average_mel_weekco2 <- read_csv(here("data", "processed-data", "average_mel_weekco2.csv"))
wx_hourly_clean <- read_csv(here("data", "processed-data", "hourly_wx_clean.csv"))
bird_clean <- read_csv(here("data", "processed-data", "bird_clean.csv"))
mel_prop_weekly <- read_csv(here("data", "processed-data", "mel_proportion_by_week.csv"))

# Aggregate weather data by week:
# Calculate the average maximum temperature (Avg_TMAX) and total precipitation (Total_PRCP) for each week.
# This simplification allows for weekly comparisons with the MIR data.
wx_aggregated <- wx_selected %>%
  group_by(week_num) %>%
  summarise(Avg_TMAX = mean(TMAX, na.rm = TRUE),
            Total_PRCP = sum(PRCP, na.rm = TRUE),
            .groups = 'drop')

# Adjust for a one-week lag in weather data:
# Create a new column (Lagged_week_num) that represents each week's preceding week. 
# This adjustment accounts for the delayed effect weather might have on MIR.
wx_aggregated <- wx_aggregated %>%
  mutate(
    # Split week_num to get year and week as numeric
    year = as.integer(substr(week_num, 1, 4)),
    week = as.integer(substr(week_num, 6, 8)),
    # Calculate the previous week accounting for year transition
    Lagged_week_num = pmap_chr(list(year, week), function(year, week) {
      if (week == 1) {
        # Transition to the last week of the previous year
        lagged_year = year - 1
        # Assume the last week of the year could be either 52 or 53
        # This may need adjustment based on your dataset specifics
        last_week_of_year = ifelse(lubridate::isoweek(lubridate::ymd(paste(lagged_year, "12", "28"))) == 53, 53, 52)
        paste0(lagged_year, "-", sprintf("%02d", last_week_of_year))
      } else {
        # Just subtract one from the week for the same year
        paste0(year, "-", sprintf("%02d", week - 1))
      }
    })
  ) %>%
  select(-year, -week) # Removing intermediate columns
head(wx_aggregated)

wx_aggregated_lead <- wx_aggregated %>%
  mutate(
    # Split week_num to get year and week as numeric
    year = as.integer(substr(week_num, 1, 4)),
    week = as.integer(substr(week_num, 6, 8)),
    # Calculate the next week, accounting for year transition
    Lead_week_num = pmap_chr(list(year, week), function(year, week) {
      last_week_of_year = ifelse(lubridate::isoweek(lubridate::ymd(paste(year, "12", "28"))) == 53, 53, 52)
      if (week == last_week_of_year) {
        # Transition to the first week of the next year
        lead_year = year + 1
        paste0(lead_year, "-", sprintf("%02d", 1))
      } else {
        # Just add one to the week for the same year
        paste0(year, "-", sprintf("%02d", week + 1))
      }
    })
  ) %>%
  select(-year, -week) # Removing intermediate columns

str(wx_aggregated_lead)

# Calculate a new colum in hourly weather data for number of hours the dry bulb temperature is above 75 and 80 degrees
# Calculating the conditions and averaging other columns
wx_hourly_lag <- wx_hourly_clean %>%
  group_by(week_num) %>%
  summarise(
    Hrs_Above_80_lag = sum(HourlyDryBulbTemperature >= 80, na.rm = TRUE),
    Hrs_Above_75_lag = sum(HourlyDryBulbTemperature >= 75, na.rm = TRUE),
    Hrs_Below_50_lag = sum(HourlyDryBulbTemperature < 50, na.rm = TRUE),
    Hrs_Between_50_86_lag = sum(HourlyDryBulbTemperature >= 50 & HourlyDryBulbTemperature <= 86, na.rm = TRUE), # Added line for temperature between 50 and 86
    Avg_HourlyDryBulbTemp_lag = mean(HourlyDryBulbTemperature, na.rm = TRUE),
    Avg_HourlyWetBulbTemp_lag = mean(HourlyWetBulbTemperature, na.rm = TRUE),
    .groups = 'drop'
  )
wx_hourly_lead <- wx_hourly_lag %>%
  mutate(
    Year = as.integer(substr(week_num, 1, 4)),
    Week = as.integer(substr(week_num, 6, 7)),
    Lead_week_num = map2_chr(Year, Week, ~ {
      last_week_of_year = ifelse(isoweek(ymd(paste(.x, "12", "28"))) == 53, 53, 52)
      if (.y == last_week_of_year) {
        lead_year = .x + 1
        paste0(lead_year, "-", sprintf("%02d", 1))
      } else {
        paste0(.x, "-", sprintf("%02d", .y + 1))
      }
    })
  )

# Define the path for the updated CSV file
updated_csv_path <- here("data", "processed-data", "wx_hourly_lead.csv")

# Save the updated dataframe
write.csv(wx_hourly_lead, updated_csv_path, row.names = FALSE)

wx_hourly_current <- wx_hourly_clean %>%
  group_by(week_num) %>%
  summarise(
    Hrs_Below_50_current = sum(HourlyDryBulbTemperature < 50, na.rm = TRUE),
    Avg_HourlyRelativeHumidity_current = mean(HourlyRelativeHumidity, na.rm = TRUE),
    Avg_HourlyWetBulbTemperature_current = mean(HourlyWetBulbTemperature, na.rm = TRUE),
    Avg_HourlyWindSpeed_current = mean(HourlyWindSpeed, na.rm = TRUE),
    .groups = 'drop'
  )

#save as csv
write_csv(wx_hourly_current, here("data", "processed-data", "wx_hourly_current.csv"))


# Check the structure to confirm transformations
str(wx_hourly_summary)



#### Calculate monthly wx stats
wx_monthly <- read_csv(here("data", "processed-data", "hourly_wx_clean.csv"))

wx_monthly$DATE <- as.POSIXct(wx_monthly$DATE, format = "%m/%d/%y %H:%M", tz = "UTC")
head(wx_monthly$DATE)
# Extract year and month for grouping
wx_monthly$year <- year(wx_monthly$DATE)
wx_monthly$month <- month(wx_monthly$DATE)

# Group and summarize as previously outlined
wx_monthly_stats <- wx_monthly %>%
  group_by(STATION, year, month) %>%
  summarize(
    hours_over_80 = sum(HourlyDryBulbTemperature > 80, na.rm = TRUE),
    hours_over_90 = sum(HourlyDryBulbTemperature > 90, na.rm = TRUE),
    hours_over_100 = sum(HourlyDryBulbTemperature > 100, na.rm = TRUE),
    hours_between_50_and_86 = sum(HourlyDryBulbTemperature >= 50 & HourlyDryBulbTemperature <= 86, na.rm = TRUE),
    .groups = 'drop'  # This automatically ungroups the data after summarizing
  )

# View the monthly stats
print(wx_monthly_stats)

# Save the monthly stats to a CSV file
write_csv(wx_monthly_stats, here("data", "processed-data", "wx_monthly_stats.csv"))

# # Correct join operations
# combined_data_mir_all <- MIR_by_week %>%
#   left_join(wx_aggregated, by = c("week_num" = "Lagged_week_num")) %>%
#   left_join(wx_hourly_summary, by = c("week_num" = "Lagged_week_num")) %>%
#   left_join(wx_hourly_summary %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
#   left_join(drought_fema1_4 %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
#   left_join(drought_ma_counties %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
#   left_join(average_daylight_by_week %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
#   left_join(average_mel_weekco2 %>% distinct(week_num, .keep_all = TRUE), by = "week_num")

# combined_data_mir_all4 <- MIR_by_week_co2 %>%
#   # Joining wx_aggregated and wx_hourly_summary on a lagged week_num
#   left_join(wx_aggregated1, by = c("week_num" = "Lead_week_num")) %>%
#   left_join(wx_hourly_summary, by = c("week_num" = "Lagged_week_num")) %>%
#   # Direct joins for other datasets
#   left_join(wx_hourly_summary %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
#   left_join(drought_fema1_4 %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
#   left_join(drought_ma_counties %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
#   left_join(average_daylight_by_week %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
#   left_join(average_mel_weekco2 %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
#   #left_join(bird_clean %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
#   left_join(mel_prop_weekly %>% distinct(week_num, .keep_all = TRUE), by = "week_num")
# View(combined_data_mir_all4)


combined_data_mir_all <- MIR_by_week_co2 %>%
  # Joining wx_aggregated and wx_hourly_summary on a lagged week_num
  left_join(wx_aggregated_lead, by = c("week_num" = "Lead_week_num")) %>%
  left_join(wx_hourly_lead, by = c("week_num" = "Lead_week_num")) %>%
  # Direct joins for other datasets
  #left_join(mosquito_wide %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
  left_join(wx_hourly_current %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
  left_join(drought_fema1_4 %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
  left_join(drought_ma_counties %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
  #left_join(average_moon_phase_by_week %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
  left_join(average_daylight_by_week %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
  left_join(average_mel_weekco2 %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
  #left_join(bird_clean %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
  left_join(mel_prop_weekly %>% distinct(week_num, .keep_all = TRUE), by = "week_num")


# Verify the structure and first few rows to ensure accuracy

print(str(combined_data_mir_all))
# For example, if wanting the first occurrence of each week_num
combined_data_mir_all <- combined_data_mir_all %>%
  ungroup() %>% # Ensure data is not grouped before proceeding
  select(
    -Year.y, -MapDate.x, -ValidEnd.x, -MapDate.y, -State, 
    -StatisticFormatID.y, -week_num.y, -Week.y, -RegionName, 
    -StatisticFormatID.x, -FIPS, -Week.x, -ValidStart.x, -County, -ValidEnd.y
  ) %>%
  summarise(across(everything(), first), .groups = 'drop')

# Save the modified dataframe as a CSV
write_csv(combined_data_mir_all, "joined_data_mir_co2.csv")


#scatter plots: 
scatter_plotMELMIR <- function(data) {
  library(ggplot2)
  
ggplot(combined_data_mir_all, aes(x = Average_MEL, y = MIR)) +
    geom_point(aes(color = Average_MEL), alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = "Scatter Plot of Average MEL vs MIR",
         x = "Average MEL",
         y = "MIR") +
    theme_minimal()
  


saveRDS(create_scatter_plot, here("data", "processed-data", "RDS", "create_scatter_plot_function.rds"))


ggplot(combined_data_mir_all, aes(x = Avg_TMAX, y = MIR)) +
  geom_point(aes(color = Total_PRCP), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of Avg_TMAX vs MIR",
       x = "Avg_TMAX",
       y = "MIR") +
  theme_minimal()

ggplot(combined_data_mir_all, aes(x = Hrs_Above_80_lag, y = MIR)) +
  geom_point(aes(color = Hrs_Above_80_lag), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of Hours Above 80 (y) vs MIR",
       x = "Hours Above 80 (y)",
       y = "MIR") +
  theme_minimal()

ggplot(combined_data_mir_all, aes(x = Avg_HourlyRelativeHumidity_current, y = MIR)) +
  geom_point(aes(color = Avg_HourlyRelativeHumidity_current), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of Hours Above 80 (y) vs MIR",
       x = "Hours Above 80 (y)",
       y = "MIR") +
  theme_minimal()


bubble_plot <- ggplot(combined_data_mir_all, aes(x = Average_MEL, y = MIR, size = mel_prop) +
  geom_point(alpha = 0.6) + # Adjust alpha for point transparency if needed
  scale_size_continuous(range = c(1, 10)) + # Adjust the size range as needed
  labs(title = "Bubble Plot of Average MEL vs MIR",
       x = "Average MEL",
       y = "MIR",
       size = "MIR") +
  theme_minimal()
bubble_plot
# Fit a linear model to analyze the relationship between MIR and weather conditions:

model <- lm(MEL_Prop ~ Avg_TMAX + DSCI_fema, data = combined_data_mir_all)


# joined_mir_clean <- read_csv(here("data", "processed-data", "joined_data_mir_co2_cleaned.csv"))
# na_rows_mir = joined_mir[is.na(joined_mir_clean$MIR), ]
# head(na_rows_mir)  # To view the first few rows
# sum(is.na(joined_mir_clean$MIR))
# sum(is.infinite(joined_mir_clean$MIR))
# # Check for NA values in the MIR column of the correct dataset
# na_rows_mir = joined_mir[is.na(joined_mir$MIR), ]
# # If `joined_mir_clean` is the correct cleaned dataset, use:
# # na_rows_mir = joined_mir_clean[is.na(joined_mir_clean$MIR), ]
# 
# # View the first few rows of data with NAs in MIR
# head(na_rows_mir)
# # Round or convert the MIR values to integers
# joined_mir_clean$MIR <- round(joined_mir_clean$MIR)





# Display a summary of the model to review coefficients, significance levels, and overall model fit.
# This summary helps understand how weekly weather conditions (from the previous week) influence the MIR of mosquito pools.
summary(model)


