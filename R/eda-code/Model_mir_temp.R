# Load necessary libraries for data manipulation, date handling, and file path management
library(lubridate)
library(here)
library(dplyr)
library(ggplot2)


# Load the datasets:
# MIR_by_week: Contains information on mosquito pool tests, including the week number and MIR.
# wx_selected: Weather data with daily maximum temperature (TMAX) and precipitation (PRCP).
# mosquito_wide: Another dataset related to mosquito data, although it's loaded, it's not used in the subsequent analysis.
MIR_by_week_co2 <- read_csv(here("data", "processed-data", "MIR_by_week_co2.csv"))
wx_selected <- read_csv(here("data", "processed-data", "wx_selected.csv"))
mosquito_wide <- read_csv(here("data", "processed-data", "mosquito_wide.csv"))
drought_fema4 <- read_csv(here("data", "processed-data", "drought_fema4.csv"))
drought_ma_ri_ct <- read_csv(here("data", "processed-data", "drought_ma_ri_ct.csv"))
*average_moon_phase_by_week <- read_csv(here("data", "processed-data", "average_moon_phase_by_week.csv"))
average_daylight_by_week <- read_csv(here("data", "processed-data", "average_daylight_by_week.csv"))
average_mel_weekco2 <- read_csv(here("data", "processed-data", "average_mel_week_co2.csv"))
wx_hourly_clean <- read_csv(here("data", "processed-data", "hourly_wx_clean.csv"))

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

# Calculate a new colum in hourly weather data for number of hours the dry bulb temperature is above 75 and 80 degrees
# Calculating the conditions and averaging other columns
wx_hourly_summary <- wx_hourly_clean %>%
  group_by(week_num) %>%
  summarise(
    Hours_Above_80 = sum(HourlyDryBulbTemperature >= 80, na.rm = TRUE),
    Hours_Above_75 = sum(HourlyDryBulbTemperature >= 75, na.rm = TRUE),
    Hours_Below_50 = sum(HourlyDryBulbTemperature < 50, na.rm = TRUE),
    Avg_HourlyRelativeHumidity = mean(HourlyRelativeHumidity, na.rm = TRUE),
    Avg_HourlyWetBulbTemperature = mean(HourlyWetBulbTemperature, na.rm = TRUE),
    Avg_HourlyWindSpeed = mean(HourlyWindSpeed, na.rm = TRUE),
    .groups = 'drop'
  )

# Adjusting for a one-week lag in weather data
wx_hourly_summary <- wx_hourly_summary %>%
  mutate(
    Year = as.integer(substr(week_num, 1, 4)),
    Week = as.integer(substr(week_num, 6, 7)),
    Lagged_week_num = map2_chr(Year, Week, ~ {
      if (.y == 1) {
        lagged_year = .x - 1
        # Check if the last week of the previous year is 52 or 53
        last_week_of_year = ifelse(isoweek(ymd(paste(lagged_year, "12", "28"))) == 53, 53, 52)
        paste0(lagged_year, "-", sprintf("%02d", last_week_of_year))
      } else {
        paste0(.x, "-", sprintf("%02d", .y - 1))
      }
    })
  ) %>%
  select(-Year, -Week) # Remove intermediate columns after lagging

# Check the structure to confirm transformations
str(wx_hourly_summary)

# Define the path for the updated CSV file
updated_csv_path <- here("data", "processed-data", "wx_hourly_summary.csv")

# Save the updated dataframe
write.csv(wx_hourly_summary, updated_csv_path, row.names = FALSE)

# Correct join operations
combined_data_mir_all <- MIR_by_week %>%
  left_join(wx_aggregated, by = c("week_num" = "Lagged_week_num")) %>%
  left_join(wx_hourly_summary, by = c("week_num" = "Lagged_week_num")) %>%
  left_join(wx_hourly_summary %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
  left_join(drought_fema4 %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
  left_join(drought_ma_ri_ct %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
  left_join(average_daylight_by_week %>% distinct(week_num, .keep_all = TRUE), by = "week_num") %>%
  left_join(average_mel_weekco2 %>% distinct(week_num, .keep_all = TRUE), by = "week_num")


# Verify the structure and first few rows to ensure accuracy
# Replace `print` with `View` or similar if using an R IDE that supports it
print(str(combined_data_mir_all))
# For example, if wanting the first occurrence of each week_num
combined_data_mir_all <- combined_data_mir_all %>%
  group_by(week_num) %>%
  summarise(across(everything(), first))

# View the resulting combined data
View(combined_data_mir_all)

#save combined_data_mir_all as csv
write_csv(combined_data_mir_all, here("data", "processed-data", "joined_data_mir_co2.csv"))

#scatter plots: 
scatter_plotMELMIR <- function(data) {
  library(ggplot2)
  
  plot <- ggplot(data, aes(x = Average_MEL, y = MIR)) +
    geom_point(aes(color = Average_MEL), alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE, color = "blue") +
    labs(title = "Scatter Plot of Average MEL vs MIR",
         x = "Average MEL",
         y = "MIR") +
    theme_minimal()
  
  return(plot)
}
saveRDS(create_scatter_plot, here("data", "processed-data", "RDS", "create_scatter_plot_function.rds"))


ggplot(combined_data_mir_all, aes(x = Avg_TMAX, y = MIR)) +
  geom_point(aes(color = Total_PRCP), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of Avg_TMAX vs MIR",
       x = "Avg_TMAX",
       y = "MIR") +
  theme_minimal()

ggplot(combined_data_mir_all, aes(x = Hours_Above_80.y, y = MIR)) +
  geom_point(aes(color = Hours_Above_80.y), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of Hours Above 80 (y) vs MIR",
       x = "Hours Above 80 (y)",
       y = "MIR") +
  theme_minimal()

ggplot(combined_data_mir_all, aes(x = Hours_Above_80.y, y = MIR)) +
  geom_point(aes(color = Hours_Above_80.y), alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Scatter Plot of Hours Above 80 (y) vs MIR",
       x = "Hours Above 80 (y)",
       y = "MIR") +
  theme_minimal()


# Fit a linear model to analyze the relationship between MIR and weather conditions:

model <- lm(MIR ~ Avg_TMAX + Hours_Above_80.y, data = combined_data_mir_all)




# Display a summary of the model to review coefficients, significance levels, and overall model fit.
# This summary helps understand how weekly weather conditions (from the previous week) influence the MIR of mosquito pools.
summary(model)


