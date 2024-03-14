library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggthemes)

agric <- read.csv("/home/akindele/Downloads/agric_1.csv")


colnames(agric)




# Install and load necessary packages if not already installed
install.packages("e1071")
install.packages("moments")
library(e1071)
library(moments)


# Specify the variable names you want to analyze
variables <- c("LFE1", "LFE2", "LFE3", "LFE4", "LFE5", "AQ1", "AQ2", "AQ3", "AQ4", "AQ5", "FD1", "FD2", "FD3", "EE1", "EE2", "EE3", "EE4", "EE5")

# Create empty data frames to store the results
skewness_results <- data.frame(Variable = character(0), Skewness = numeric(0))
kurtosis_results <- data.frame(Variable = character(0), Kurtosis = numeric(0))
normality_test_results <- data.frame(Variable = character(0), P_Value = numeric(0), Is_Normal = character(0))

# Loop through each variable and calculate skewness, kurtosis, and perform normality test
for (variable in variables) {
  # Calculate skewness
  skew <- skewness(agric[[variable]])
  
  # Calculate kurtosis
  kurt <- kurtosis(agric[[variable]])
  
  # Perform Shapiro-Wilk normality test
  shapiro_test <- shapiro.test(agric[[variable]])
  
  # Store the results in the respective data frames
  skewness_results <- rbind(skewness_results, data.frame(Variable = variable, Skewness = skew))
  kurtosis_results <- rbind(kurtosis_results, data.frame(Variable = variable, Kurtosis = kurt))
  normality_test_results <- rbind(normality_test_results, data.frame(Variable = variable, P_Value = shapiro_test$p.value, Is_Normal = ifelse(shapiro_test$p.value > 0.05, "Yes", "No")))
}

# Print the results
cat("Skewness Results:\n")
print(skewness_results)

cat("\nKurtosis Results:\n")
print(kurtosis_results)

cat("\nNormality Test Results:\n")
print(normality_test_results)

# Save the results as CSV files
write.csv(skewness_results, "skewness_results.csv", row.names = FALSE)
write.csv(kurtosis_results, "kurtosis_results.csv", row.names = FALSE)
write.csv(normality_test_results, "normality_test_results.csv", row.names = FALSE)


# Save skewness results to a CSV file
write.csv(skewness_results, file = "skewness_results.csv", row.names = FALSE)

# Save kurtosis results to a CSV file
write.csv(kurtosis_results, file = "kurtosis_results.csv", row.names = FALSE)

# Save normality test results to a CSV file
write.csv(normality_test_results, file = "normality_test_results.csv", row.names = FALSE)
