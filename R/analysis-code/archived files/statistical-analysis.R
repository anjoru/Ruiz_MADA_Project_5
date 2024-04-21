library(here)
library(tidymodels)
library(ggplot2)
library(GGally)
library(ggcorrplot)
library(glmnet)
library(ranger)
library(dplyr)

# Load the datasets:
# Joined_data_MIR_CO2: Contains information on mosquito infection rates (MIR) and weather data.
joined_data_mir_co2 <- read_csv(here("data", "processed-data", "joined_data_mir_co2.csv"))

# Assuming combined_data_mir_all is loaded and prepared
continuous_vars_model <- joined_data_mir_co2[, c("MIR", "Average_MEL", "Total_PRCP", "Hours_Above_80.y", 
                                                   "Hours_Above_80.x", "Hours_Below_50.x", "Hours_Below_50.y", 
                                                   "Avg_HourlyRelativeHumidity.y", "dsci", "dsdi")]

# Imputing NA values for Average_MEL with the mean (or choose another method as appropriate)
continuous_vars_model$Average_MEL[is.na(continuous_vars_model$Average_MEL)] <- mean(continuous_vars_model$Average_MEL, na.rm = TRUE)

# Removing dsdi from the analysis due to zero variance
continuous_vars_model <- continuous_vars_model[, !names(continuous_vars_model) %in% c("dsdi")]

# Recompute the correlation matrix without dsdi and with imputed values for Average_MEL
cor_matrix <- cor(continuous_vars_model, use = "complete.obs")

# Visualize the updated correlation matrix
cor_mir_plot = ggcorrplot(cor_matrix, method = "circle", hc.order = TRUE, type = "lower",
           lab = TRUE, lab_size = 3, colors = c("gold", "snow", "tomato"))

# Print the updated correlation matrix
print(cor_matrix)
# Save the plot as a PNG file
ggsave(filename = here("results", "figures", "correlation_plot.png"), plot = cor_mir_plot, width = 10, height = 8, dpi = 300)

# Or save as a PDF for high-quality printing
ggsave(filename = here("results", "figures", "correlation_plot.pdf"), plot = cor_mir_plot, width = 10, height = 8)

