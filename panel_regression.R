#Libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(tools)
library(summarytools)
library(flextable)
library(knitr)
library(kableExtra)



panel_data <- read.csv("/home/akindele/Downloads/Panel_regression_2.csv")

colnames(panel_data)

# Convert all columns to numeric
panel_data[] <- lapply(panel_data, as.numeric)

mean(panel_data$T, na.rm = TRUE)

# Set options to display numbers without scientific notation
options(scipen = 999)


# Calculate descriptive statistics for your variables and round to two decimal points
mean_values <- round(colMeans(panel_data[, c("T", "P", "L", "LA", "K", "F", "Y2")], na.rm = TRUE), 2)
variance_values <- round(sapply(panel_data[, c("T", "P", "L", "LA", "K", "F", "Y2")], function(x) var(x, na.rm = TRUE)), 2)
std_dev_values <- round(sqrt(variance_values), 2)



# Calculate the correlation matrix
cor_panel <- round(cor(panel_data[, c("T", "P", "L", "LA", "K", "F", "Y2")], 
                       y = panel_data[, c("T", "P", "L", "LA", "K", "F", "Y2")], 
                       use = "pairwise.complete.obs"), 2)

# Create a data frame for descriptive statistics
descriptive_stats <- data.frame(
  Variable = c("T", "P", "L", "LA", "K", "F", "Y2"),
  Mean = mean_values,
  Variance = variance_values,
  StdDev = std_dev_values
)

# Save the descriptive statistics as a CSV file
write.csv(descriptive_stats, file.path("descriptive_stats.csv"), row.names = FALSE)

# Save the correlation matrix as a CSV file
write.csv(cor_panel, file.path("correlation_matrix.csv"))





# Install and load the 'plm' package if you haven't already
library(plm)

# Specify the regression model
model_formula <- Y2 ~ T + P + L + LA + K + F

# Convert your data to a panel data frame
panel_data <- pdata.frame(panel_data, index = c("id", "year"))

# Pooled OLS
pooled_ols_model <- plm(model_formula, data = panel_data, model = "pooling")
pooled_ols_summary <- summary(pooled_ols_model)

# Fixed Effects
fixed_effects_model <- plm(model_formula, data = panel_data, model = "within")
fixed_effects_summary <- summary(fixed_effects_model)

# Extract the relevant information from the summaries
pooled_ols_info <- data.frame(
  Coefficient = row.names(pooled_ols_summary$coefficients),
  Estimate = pooled_ols_summary$coefficients[, "Estimate"],
  Std.Error = pooled_ols_summary$coefficients[, "Std. Error"]
)

fixed_effects_info <- data.frame(
  Coefficient = row.names(fixed_effects_summary$coefficients),
  Estimate = fixed_effects_summary$coefficients[, "Estimate"],
  Std.Error = fixed_effects_summary$coefficients[, "Std. Error"]
)


# Save the extracted information as CSV files
write.csv(pooled_ols_info, file.path("pooled_ols_summary.csv"), row.names = FALSE)
write.csv(fixed_effects_info, file.path("fixed_effects_summary.csv"), row.names = FALSE)


# Install and load the lme4 package if you haven't already
library(lme4)

# Assuming your panel data is stored in a data frame called panel_data
# and your model_formula is defined

# Assuming 'id' is the variable representing the panel (individual) identifier,
# and 'year' is the variable representing the time (year) identifier.
# Modify the formula accordingly based on your data.

model_formula <- Y2 ~ T + P + L + LA + K + F + (1 | id) + (1 | year)

# Estimate the random effects model
random_effects_model <- lmer(model_formula, data = panel_data)

# Print a summary of the random effects model
random_effects_summary <- summary(random_effects_model)



# Assuming you have already estimated the random effects model and stored it in 'random_effects_model'

# Extract the fixed effects coefficients
fixed_effects_coefficients <- fixef(random_effects_model)

# Extract the random effects (variance) components
random_effects_components <- ranef(random_effects_model)


# Get the summary of the fixed effects model
random_effects_summary <- summary(random_effects_model)

# Create a data frame with coefficients, estimates, and standard errors
random_effects_info <- data.frame(
  Coefficient = row.names(random_effects_summary$coefficients),
  Estimate = random_effects_summary$coefficients[, "Estimate"],
  Std.Error = random_effects_summary$coefficients[, "Std. Error"]
)

# Save the data frame as a CSV file
write.csv(random_effects_info, file = "random_effects_info.csv", row.names = FALSE)

