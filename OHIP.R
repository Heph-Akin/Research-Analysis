library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggthemes)

OHIP <- read.csv("/home/akindele/Downloads/OHIP_ORTHO_Adeloye Clean.csv")


colnames(OHIP)

# Calculate the mean values for the variables
mean_values <- OHIP %>%
  summarise(across(c(OHRQoL, Funtional.Disability, Physical.pain, Physical.disability,
                     Psychological.Discomfort, Psychological.Disability, Social.Disability, Handicap), mean))

# Load the reshape2 package to melt the data
install.packages("reshape2")
library(reshape2)

# Melt the data for plotting
melted_data <- melt(mean_values, variable.name = "Variable")

# Create the bubble chart
bubble_plot <- ggplot(melted_data, aes(x = reorder(Variable, -value), y = 1, size = value)) +
  geom_point(shape = 21, fill = "blue", color = "black") +
  scale_size_continuous(range = c(5, 20)) +
  labs(x = "Variable", y = "", title = "Bubble Chart of Mean Values") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Show the bubble chart
print(bubble_plot)


library(stringr)

# Create a function to format the variable labels
format_variable_labels <- function(label) {
  # Use stringr functions to add spaces and format the labels
  formatted_label <- str_replace_all(label, "\\.", " ")  # Replace dots with spaces
  formatted_label <- str_to_title(formatted_label)  # Capitalize the first letter of each word
  return(formatted_label)
}

# Transpose the axis and format variable labels
bubble_plot <- ggplot(melted_data, aes(y = reorder(Variable, -value), x = 1, size = value)) +
  geom_point(shape = 21, fill = "lightblue", color = "blue") +
  scale_size_continuous(range = c(5, 20)) +
  labs(y = "Variable", x = "", title = "Bubble Chart of Mean Values") +
  theme_minimal() +
  theme(axis.text.y = element_text(hjust = 1)) +
  theme(axis.text.y = element_text(hjust = 1, face = "bold")) +  # Make y-axis labels bolder +
  scale_y_discrete(labels = format_variable_labels)  # Format the y-axis labels

# Add the mean values beside the bubbles with adjusted position
bubble_plot <- bubble_plot +
  geom_text(data = melted_data, aes(label = round(value, 2), y = reorder(Variable, -value), hjust = -2.5), size = 6, family = "Roboto", face = "bold")  # Using the "Roboto" font

# Show the updated bubble chart
print(bubble_plot)
