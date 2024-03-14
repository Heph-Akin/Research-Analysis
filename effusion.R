library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggthemes)

effusion <- read.csv("/home/akindele/Downloads/Effusion.csv")

colnames(effusion)

head(effusion)


# Assuming your data frame is called 'effusion'
# Make sure 'Etiology' and 'Age.y' are appropriately named in your data frame

library(dplyr)

# Group by 'Etiology' and calculate count and mean age
summary_df <- effusion %>%
  group_by(Etiology) %>%
  summarise(
    Count = n(),
    Mean_Age = mean(Age)
  ) %>%
  mutate(Percentage = Count/sum(Count) * 100)



# Print the summary data frame
print(summary_df)


# Install and load the 'viridis' package
#install.packages("viridis")
library(viridis)

# Your code for the regular bar plot
plt <- ggplot(summary_df) +
  geom_bar(
    aes(
      x = reorder(Etiology, -Mean_Age),
      y = Percentage,
      fill = Mean_Age
    ),
    stat = "identity",
    position = "dodge",
    show.legend = TRUE,
    alpha = 0.9
  ) +
  # Additional modifications for clarity and styling
  labs(
    title = "\nBar Chart for Pleural Effusion Etiology",
    subtitle = "\nBar color represents the mean age of presentation of the various etiologies of Pleaural Effusion,\n and height represents the Percentage for each etiology.",
    x = "Etiology of Pleural Effusion",  # Added X-axis label
    y = "Percentage",  # Added Y-axis label
    ) +
  scale_fill_viridis_c(
    name = "Mean Age of Presentation",
    option = "plasma"
  ) +
  theme_minimal() +
  coord_flip() +
  # Improving axis labels and text appearance
  theme(
    axis.text = element_text(size = 12, color = "black", family = "Arial"),
    axis.title = element_text(size = 14, color = "black", family = "Arial"),
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5, family = "Arial"),
    plot.subtitle = element_text(size = 14, hjust = 0.5, family = "Arial"),
    plot.caption = element_text(size = 12, hjust = 0.5, family = "Arial")
  )

# Print the plot
print(plt)

# Assuming your data frame is called 'effusion'
# Make sure 'Procedure' is a factor variable
effusion$Procedure <- factor(effusion$Procedure)

# Calculate the count for each procedure
procedure_counts <- table(effusion$Procedure)

# Calculate the total count
total_count <- sum(procedure_counts)

# Calculate the percentage for each procedure
procedure_percentages <- procedure_counts / total_count * 100

# Create a data frame for the lollipop chart
procedure_df <- data.frame(
  Procedure = names(procedure_percentages),
  Percentage = as.numeric(procedure_percentages)
)

# Sort the data frame by percentage in descending order
procedure_df <- procedure_df[order(-procedure_df$Percentage), ]

# Your code for the lollipop chart with percentages
lollipop <- ggplot(procedure_df, aes(x = reorder(Procedure, Percentage), y = Percentage)) +
  geom_segment(aes(xend = reorder(Procedure, Percentage), yend = 0), color = "gray", size = 1.5) +
  geom_point(color = "steelblue", size = 3) +
  labs(
    title = "\nLollipop Chart for Pleural Effusion Procedures",
    subtitle = "\nPercentages of different procedures for Pleural Effusion",
    x = "Procedure",
    y = "Percentage"
  ) +
  theme_minimal() +
  coord_flip() +
  theme(
    text = element_text(family = "Arial", size = 12),  # Adjust font family and size
    axis.text.x = element_text(angle = 45, hjust = 1)  # Rotate X-axis labels for better readability
  )

# Print the lollipop chart
print(lollipop)





###
library(ggplot2)
library(dplyr)

# Assuming your data frame is called 'effusion'
# Make sure 'Pattern.of.Effusion' is appropriately named in your data frame

# Sample data
pattern_data <- effusion %>%
  group_by(Pattern.of.Effusion) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Bar chart
bar_chart <- ggplot(pattern_data, aes(x = str_wrap(Pattern.of.Effusion,5), y = Percentage)) +
  geom_bar(stat = "identity", show.legend = FALSE) +
  labs(
    title = "Distribution of Pattern of Pleural Effusion",
    x = "Pattern of Effusion",
    y = "Percentage"
  ) +
  theme_minimal()

# Display the bar chart
print(bar_chart)
