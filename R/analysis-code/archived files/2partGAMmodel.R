library()
###Two Part Model###
mir_2part <- read_csv(here("data", "processed-data", "joined_data_mir_co2.csv"))

library(dplyr)
str(mir_2part)
# Creating a binary variable for zero vs. positive MIR
mir_2part <- mir_2part %>%
  mutate(MIR_Positive = as.integer(MIR > 0))

# Fitting the logistic regression model
logistic_model <- glm(MIR_Positive ~ Avg_HourlyRelativeHumidity_current + Avg_HourlyWindSpeed_current + Hrs_Below_50_current,
                      family = binomial(link = "logit"),
                      data = mir_2part)

summary(logistic_model)


# Filtering the dataset for positive MIR values
positive_mir_data <- mir_2part %>% 
  filter(MIR > 0)

# Fitting the linear regression model to the positive MIR values
linear_model <- lm(MIR ~ Avg_HourlyRelativeHumidity + Avg_TMAX + DSCI_ma + Avg_TMAX,
                   data = positive_mir_data)

summary(linear_model)

# Plot the diagnostic plots for the linear model
par(mfrow = c(2, 2))
plot(linear_model)
### These suggest that the issues of non-linearity, heteroscedasticity, and influential outliers need to be addressed using different models

# For this we will use a two-part model, which consists of a GAM logistic regression model to predict the probability of observing a positive MIR value,

library(mgcv)
# Fit a GAM with a smoothing function for each predictor
gam_model <- gam(MIR ~ s(Avg_TMAX),
                 data = mir_2part, 
                 family = gaussian(),
                 method = "REML")

# Check the summary for detailed output
summary(gam_model)

# Plot the smooths to interpret the effect of each predictor
plot(gam_model)



##2 stage GAM

zero_gam_model <- gam(I(MIR > 0) ~ s(DSCI_ma) + s(Average_MEL) + s(Avg_HourlyWindSpeed_current),
                      family = binomial(link = "logit"), 
                      data = mir_2part)

summary(zero_gam_model)
plot(zero_gam_model)

names(mir_2part)
positive_gam_model <- gam(MIR ~ s(Hrs_Between_50_86_lag), 
                          data = mir_2part[mir_2part$MIR > 0, ], 
                          family = gaussian())
# Check the summary for detailed output
summary(positive_gam_model)
# Plot the smooths to interpret the effect of each predictor
plot(gam_model)



library(mgcv)

# Response variable
response_var <- "MIR"
# Variables to exclude from the analysis
exclude_vars <- c("week_num", "Pools_Tested", "Pools_Tested", "Positive_Pools", "Total_Tested", "Year", "Week.x", "Town", "Result", "Test_Type_Expanded", "MIR_Positive")
# List all predictors excluding the response variable
predictors <- setdiff(names(mir_2part), response_var)

# Initiate lists to store models and summaries
gam_models_zero <- list()
gam_models_positive <- list()
summary_models_zero <- list()
summary_models_positive <- list()

# Loop through each predictor
for (predictor in predictors) {
  # Formula for the zero-inflation part (binary outcome)
  formula_zero <- as.formula(paste("I(", response_var, " > 0) ~ s(", predictor, ")", sep = ""))
  # Formula for the positive count part
  formula_positive <- as.formula(paste(response_var, " ~ s(", predictor, ")", sep = ""))
  
  # Fit the zero-inflation model
  zero_model <- gam(formula_zero, data = mir_2part, family = binomial(link = "logit"))
  # Fit the positive count model
  positive_model <- gam(formula_positive, data = mir_2part[mir_2part[[response_var]] > 0, ], family = gaussian())
  
  # Store the models and summaries using predictor names to keep them unique
  gam_models_zero[[predictor]] <- zero_model
  gam_models_positive[[predictor]] <- positive_model
  summary_models_zero[[paste("zero", predictor, sep = "_")]] <- summary(zero_model)
  summary_models_positive[[paste("positive", predictor, sep = "_")]] <- summary(positive_model)
  
  # Print and plot the summaries for the zero model
  print(paste("Zero Model for", predictor))
  print(summary(zero_model))
  plot(zero_model, pages = 1)
  
  # Print and plot the summaries for the positive model, if there are any positive counts
  if (any(mir_2part[[response_var]] > 0)) {
  
  
  
  