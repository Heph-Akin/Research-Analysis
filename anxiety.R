library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggthemes)

anxiety <- read.csv("/home/akindele/Downloads/Anxiety.csv")


colnames(anxiety)


library(ggplot2)

# Create a modern-looking bar chart for Age Groups
age_bar_plot <- ggplot(anxiety, aes(x = Age.Category, fill = Age)) +
  geom_bar(stat = "count", position = "dodge") +
  labs(title = "Distribution of Participants by Age Group",
       x = "Age Groups",
       y = "Number of Participants") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_brewer(palette = "Set3")   # Change the color palette
  
  
  MDAS_histogram <- ggplot(anxiety, aes(x = MDAS_Total)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
    geom_vline(xintercept = 19, color = "red", linetype = "dashed", size = 1) +  # Add a red dashed line at x = 19
    labs(title = "Histogram of Anxiety Scores",
         x = "Modified Dental Anxiety Scale Scores",
         y = "Frequency") +
    theme_minimal() +
    theme(panel.grid.major = element_line(color = "lightgray")) +  # Add grid lines
    scale_fill_brewer(palette = "Blues") +  # Change the color palette
    scale_color_brewer(palette = "Dark2")  # Change histogram outline color
  
  
  
  # Create a modern-looking histogram for Anxiety Scores
  GAD_histogram <- ggplot(anxiety, aes(x = GAD_Total)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
    labs(title = "Histogram of Anxiety Scores",
         x = "General Anxiety Disorder Scores",
         y = "Frequency") +
    theme_minimal() +
    theme(panel.grid.major = element_line(color = "lightgray")) +  # Add grid lines
    scale_fill_brewer(palette = "Blues") +  # Change the color palette
    scale_color_brewer(palette = "Dark2")  # Change histogram outline color
  
  
  # Create a modern-looking histogram for "Experience_Based"
  experience_histogram <- ggplot(anxiety, aes(x = Experience_Based)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
    labs(title = "Histogram of Mean Experience-Based Dental Avoidance Scores",
         x = "Mean Dental Avoidance Scores",
         y = "Frequency") +
    theme_minimal() +
    theme(panel.grid.major = element_line(color = "lightgray")) +
    scale_fill_brewer(palette = "Blues") +
    scale_color_brewer(palette = "Dark2")
  
  # Create a modern-looking histogram for "Imagination_Based"
  imagination_histogram <- ggplot(anxiety, aes(x = Imagination_Based)) +
    geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
    labs(title = "Histogram of Mean Imagination-Based Dental Avoidance Scores",
         x = "Mean Dental Avoidance Scores",
         y = "Frequency") +
    theme_minimal() +
    theme(panel.grid.major = element_line(color = "lightgray")) +
    scale_fill_brewer(palette = "Blues") +
    scale_color_brewer(palette = "Dark2")
  
  
  
  