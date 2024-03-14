library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(gtsummary)
library(dplyr)


renal_data <- read.csv("/home/akindele/Documents/Data_Science_Projects/R/Research Analysis/renal_spread.csv")


# Merge Cardiovascular Disorders columns
renal_data$Cardiovascular_Disorders <- as.integer(renal_data$Cardiovascular.Disorders | renal_data$Other.CormobiditiesCardiovascular.Disorders)
# Merge Metabolic Disorders columns
renal_data$Metabolic_Disorders <- as.integer(renal_data$Metabolic.Disorders | renal_data$Other.CormobiditiesMetabolic.Disorders)

# Drop the original columns
renal_data <- renal_data[, !(names(renal_data) %in% c("Cardiovascular.Disorders", "Other.CormobiditiesCardiovascular.Disorders",
                                                      "Metabolic.Disorders", "Other.CormobiditiesMetabolic.Disorders"))]

colnames(renal_data)

renal_data$Time.Spent.on.dialysis..months. <- as.numeric(renal_data$Time.Spent.on.dialysis..months.)



renal_data <- renal_data %>%
  mutate(
    Occupation = ifelse(Occupation == "", NA, Occupation),
    Renal.transplant.outcome = ifelse(Renal.transplant.outcome == "", NA, Renal.transplant.outcome),
    Graft.survival.at.1.year = ifelse(Graft.survival.at.1.year == "", NA, Graft.survival.at.1.year),
    Graft.survival.at.3.years = ifelse(Graft.survival.at.3.years == "", NA, Graft.survival.at.3.years),
    Complications.during.hemodialysis = ifelse(Complications.during.hemodialysis == "", "Nil", Complications.during.hemodialysis),
    Complictions.during.surgery = ifelse(Complictions.during.surgery == "", "Nil", Complictions.during.surgery),
    Deceased = ifelse(Deceased == "", "No", Deceased)
  )

library(gtsummary)

# Create a gtsummary table
summary_table <- tbl_summary(
  data = renal_data,
  include = c(Age..years., Age_category, Gender, Occupation),
  missing = "no"
)

# Display the table
summary_table





variables_of_interest <- c(
  "Prolonged.analgesic.use",
  "hx.of.alcohol",
  "hx.of.tobacco",
  "Cardiovascular_Disorders",
  "Metabolic_Disorders",
  "Other.CormobiditiesAutoimmune.Disorders",
  "Other.CormobiditiesGastrointestinal.Disorders",
  "Other.CormobiditiesInfectious.Diseases",
  "Other.CormobiditiesNeurological.Conditions",
  "Other.CormobiditiesNil",
  "Other.CormobiditiesNon.Renal.Genetic.Disorders",
  "Other.CormobiditiesOncological.Conditions",
  "Other.CormobiditiesRenal.Disorders",
  "Other.CormobiditiesRespiratory.Disorders",
  "Other.CormobiditiesUrological.Conditions"
)

# Define custom labels for the variables
labels <- c(
  "Prolonged.analgesic.use" = "Prolonged Analgesic Use",
  "hx.of.alcohol" = "History of Alcohol Use",
  "hx.of.tobacco" = "History of Tobacco Use",
  "Cardiovascular_Disorders" = "Cardiovascular Disorders",
  "Metabolic_Disorders" = "Metabolic Disorders",
  "Other.CormobiditiesAutoimmune.Disorders" = "Autoimmune Disorders",
  "Other.CormobiditiesGastrointestinal.Disorders" = "Gastrointestinal Disorders",
  "Other.CormobiditiesInfectious.Diseases" = "Infectious Diseases",
  "Other.CormobiditiesNeurological.Conditions" = "Neurological Conditions",
  "Other.CormobiditiesNon.Renal.Genetic.Disorders" = "Non-Renal Genetic Disorders",
  "Other.CormobiditiesOncological.Conditions" = "Oncological Conditions",
  "Other.CormobiditiesRenal.Disorders" = "Renal Disorders",
  "Other.CormobiditiesRespiratory.Disorders" = "Respiratory Disorders",
  "Other.CormobiditiesUrological.Conditions" = "Urological Conditions"
)

# Create a gtsummary table with custom labels
summary_table <- tbl_summary(
  data = renal_data,
  include = all_of(variables_of_interest),
  missing = "no",
  label = labels
) 

# Display the table
summary_table


# Define custom labels for the variables
labels <- c(
  "Fem.V" = "Femoral Vein",
  "SubC.V" = "Subclavian Vein",
  "IJV" = "Internal Jugular Vein",
  "Time.Spent.on.dialysis..months." = "Time Spent on Dialysis (months)",
  "Complications.during.hemodialysis" = "Complications During Hemodialysis",
  "Complictions.during.surgery" = "Complications During Surgery",
  "Renal.transplant.outcome" = "Renal Transplant Outcome",
  "Graft.survival.at.1.year" = "Graft Survival at 1 Year",
  "Graft.survival.at.3.years" = "Graft Survival at 3 Years",
  "Deceased" = "Deceased"
)

# Specify the variables of interest
variables_of_interest <- c(
  "Fem.V", "SubC.V", "IJV",
  "Time.Spent.on.dialysis..months.", "Complications.during.hemodialysis",
  "Complictions.during.surgery", "Renal.transplant.outcome",
  "Graft.survival.at.1.year", "Graft.survival.at.3.years",
  "Deceased"
)

# Create a gtsummary table with custom labels
summary_table <- tbl_summary(
  data = renal_data,
  include = all_of(variables_of_interest),
  missing = "no",
  label = labels
) 

# Display the table
summary_table

renal_data %>%
  select(Graft.survival.at.1.year, Graft.survival.at.3.years, Deceased, Cardiovascular_Disorders) %>%
  tbl_summary(by = Cardiovascular_Disorders,
              percent = "row") %>%
  add_p()


renal_data %>%
  select(Graft.survival.at.1.year, Graft.survival.at.3.years, Deceased, Metabolic_Disorders) %>%
  tbl_summary(by = Metabolic_Disorders,
              percent = "row") %>%
  add_p()

renal_data %>%
  select(Graft.survival.at.1.year, Graft.survival.at.3.years, Deceased, IJV) %>%
  tbl_summary(by = IJV,
              percent = "row") %>%
  add_p()

renal_data %>%
  select(Graft.survival.at.1.year, Graft.survival.at.3.years, Deceased, Fem.V) %>%
  tbl_summary(by = Fem.V,
              percent = "row") %>%
  add_p()

renal_data %>%
  select(Graft.survival.at.1.year, Graft.survival.at.3.years, Deceased, SubC.V) %>%
  tbl_summary(by = SubC.V,
              percent = "row") %>%
  add_p()



# Convert Graft.survival.at.1.year and Graft.survival.at.3.years to factors
renal_data$Graft.survival.at.1.year <- factor(renal_data$Graft.survival.at.1.year)
renal_data$Graft.survival.at.3.years <- factor(renal_data$Graft.survival.at.3.years)
renal_data$Graft.survival.at.1.year[is.na(renal_data$Graft.survival.at.1.year)] <- "No"
renal_data$Graft.survival.at.3.years[is.na(renal_data$Graft.survival.at.3.years)] <- "No"



# Define custom labels for the variables
labels <- c(
  Age_category = "Age Category",
  Gender = "Gender",
  Prolonged.analgesic.use = "Prolonged Analgesic Use",
  hx.of.alcohol = "History of Alcohol Use",
  hx.of.tobacco = "History of Tobacco Use",
  Cardiovascular_Disorders = "Cardiovascular Disorders",
  Metabolic_Disorders = "Metabolic Disorders",
  Fem.V = "Femoral Vein",
  SubC.V = "Subclavian Vein",
  IJV = "Internal Jugular Vein"
)

# Fit the logistic regression model
model <- glm(Graft.survival.at.1.year ~ Age_category + Gender +
               Prolonged.analgesic.use + hx.of.alcohol + hx.of.tobacco +
               Cardiovascular_Disorders +
               Metabolic_Disorders,
             data = renal_data, family = binomial)

# Get the model summary
summary_table <- tbl_regression(model, label = labels)

# Display the table
summary_table

# Fit the logistic regression model
model_additional <- glm(Graft.survival.at.1.year ~ Fem.V + SubC.V + IJV +
                          Time.Spent.on.dialysis..months.,
                        data = renal_data, family = binomial)

# Get the model summary
summary_table_additional <- tbl_regression(model_additional, label = labels)

# Display the table
summary_table_additional

# Fit the logistic regression model for 3 years
model_3_years <- glm(Graft.survival.at.3.years ~ Age_category + Gender +
                       Prolonged.analgesic.use + hx.of.alcohol + hx.of.tobacco + Metabolic_Disorders + Cardiovascular_Disorders,
                     data = renal_data, family = binomial)

# Get the model summary
summary_table_3_years <- tbl_regression(model_3_years, label = labels)

# Display the table
summary_table_3_years

# Fit the logistic regression model for 3 years with additional variables
model_additional_3_years <- glm(Graft.survival.at.3.years ~ Fem.V + SubC.V + IJV +
                                  Time.Spent.on.dialysis..months.,
                                data = renal_data, family = binomial)

# Get the model summary
summary_table_additional_3_years <- tbl_regression(model_additional_3_years, label = labels)

# Display the table
summary_table_additional_3_years




#### Viz
library(ggplot2)
library(reshape2)


# Filter the data
filtered_renal_data <- renal_data %>%
  select(
    Renal.transplant.outcome,
    Graft.survival.at.1.year,
    Graft.survival.at.3.years,
    Deceased,
    IJV,
    Fem.V,
    SubC.V
  ) %>%
  mutate(Renal.transplant.outcome = if_else(Renal.transplant.outcome != "mortality", "Survival", Renal.transplant.outcome))

# Reshape the data to long format
melted_data <- filtered_renal_data %>%
  gather(key = "variable", value = "value", -c(IJV, Fem.V, SubC.V))

# Add a new variable for the combinations of IJV, FemV, and SubC.V
melted_data <- melted_data %>%
  mutate(combination = case_when(
    IJV == "Yes" & Fem.V == "No" & SubC.V == "No" ~ "Internal Jugular Only",
    Fem.V == "Yes" & IJV == "No" & SubC.V == "No" ~ "Femoral Only",
    SubC.V == "Yes" & IJV == "No" & Fem.V == "No" ~ "Subclavian Only",
    IJV == "Yes" & Fem.V == "Yes" & SubC.V == "No" ~ "Internal Jugular & Femoral",
    IJV == "Yes" & SubC.V == "Yes" & Fem.V == "No" ~ "Internal Jugular & Subclavian",
    SubC.V == "Yes" & Fem.V == "Yes" & IJV == "No" ~ "Subclavian & Femoral",
    IJV == "Yes" & Fem.V == "Yes" & SubC.V == "Yes" ~ "Internal Jugular, Subclavian and Femoral"
  )) 

# Summarize the frequencies
summarized_data <- melted_data %>%
  group_by(variable, value, combination) %>%
  summarise(frequency = n(), .groups = "drop")

# Merge the summarized data back into the melted data
melted_data <- left_join(melted_data, summarized_data, by = c("variable", "value", "combination"))

# Check the updated melted data
print(melted_data)







# Filter the data
plot_data <- melted_data %>%
  filter(
    variable == "Renal.transplant.outcome" & value == "Survival" |
      variable == "Graft.survival.at.1.year" & value == "Yes" |
      variable == "Graft.survival.at.3.years" & value == "Yes"
  )

# Define the mapping of x-axis variables to legible labels
variable_labels <- c(
  "Renal.transplant.outcome" = "Initial Graft Survival",
  "Graft.survival.at.1.year" = "Graft Survival at 1 year",
  "Graft.survival.at.3.years" = "Graft Survival at 3 years"
)

# Apply the mapping to the variable column
plot_data <- plot_data %>%
  mutate(variable = case_when(
    variable == "Renal.transplant.outcome" ~ "Initial Graft Survival",
    variable == "Graft.survival.at.1.year" ~ "Graft Survival at 1 year",
    variable == "Graft.survival.at.3.years" ~ "Graft Survival at 3 years",
    TRUE ~ as.character(variable)
  ))

# Filter data for combinations
combination_data <- plot_data %>%
  filter(
    combination %in% c(
      "Internal Jugular & Femoral",
      "Internal Jugular & Subclavian",
      "Subclavian & Femoral",
      "Internal Jugular, Subclavian and Femoral"
    )
  )

# Filter data for single veins
single_vein_data <- plot_data %>%
  filter(
    combination %in% c(
      "Subclavian Only",
      "Femoral Only",
      "Internal Jugular Only"
    )
  )

# Define the order of the x-axis categories
order <- c("Initial Graft Survival", "Graft Survival at 1 year", "Graft Survival at 3 years")

# Convert the variable column to a factor with the defined order
plot_data$variable <- factor(plot_data$variable, levels = order)

# Create plot for combinations
# Create plot for combinations with adjusted y-axis
plot_combinations <- ggplot(combination_data, aes(x = variable, y = frequency, color = combination, linetype = combination, shape = combination, group = combination)) +
  geom_line() +
  geom_point() +  # Use default shapes
  scale_color_manual(values = c("blue", "red", "green", "purple")) +  # More contrasting colors
  labs(x = "Graft Survival", y = "Frequency", title = "Frequency of Renal Transplant Outcomes by Vascular Access Combination (Combinations)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  scale_y_continuous(breaks = seq(35, 75, by = 10), limits = c(35, 75))

# Create plot for single veins
plot_single_veins <- ggplot(single_vein_data, aes(x = variable, y = frequency, color = combination, linetype = combination, shape = combination, group = combination)) +
  geom_line() +
  geom_point() +  # Use default shapes
  scale_color_manual(values = c("blue", "red", "green")) +  # More contrasting colors
  labs(x = "Graft Survival", y = "Frequency", title = "Frequency of Renal Transplant Outcomes by Vascular Access Combination (Single Veins)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5))

# Display the plots
plot_combinations
plot_single_veins






library(ggplot2)
library(cowplot)


# Create plot for combinations with lower plots
plot_combinations_legend <- ggplot(plot_data, aes(x = variable, y = frequency, color = combination, linetype = combination, shape = combination, group = combination)) +
  geom_line() +
  geom_point() +  # Use default shapes
  scale_color_manual(values = c("blue", "red", "green", "purple", "orange", "black")) +  # 6 distinct colors
  labs(x = "Graft Survival", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +  # Adjust x-axis labels and remove legend
  scale_y_continuous(breaks = seq(0, 4, by = 1), limits = c(0, 4)) 


# Display the plot
print(plot_combinations_legend)


# Create plot for combinations with lower plots
plot_combinations_lower <- ggplot(plot_data, aes(x = variable, y = frequency, color = combination, linetype = combination, shape = combination, group = combination)) +
  geom_line() +
  geom_point() +  # Use default shapes
  scale_color_manual(values = c("blue", "red", "green", "purple", "orange", "black")) +  # 6 distinct colors
  labs(x = "Graft Survival", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5), legend.position = "none") +  # Adjust x-axis labels and remove legend
  scale_y_continuous(breaks = seq(0, 4, by = 1), limits = c(0, 4)) 

# Create plot for combinations with higher plots
plot_combinations_higher <- ggplot(plot_data, aes(x = variable, y = frequency, color = combination, linetype = combination, shape = combination, group = combination)) +
  geom_line() +
  geom_point() +  # Use default shapes
  scale_color_manual(values = c("blue", "red", "green", "purple", "orange", "black")) +  # 6 distinct colors
  labs(x = "Graft Survival", y = "Frequency", title = "Frequency of Renal Transplant Outcomes by Vascular Access Combination (Combinations) - Higher Plots") +
  theme_minimal() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), legend.position = "none") +  # Remove x-axis labels and title, remove legend
  scale_y_continuous(breaks = seq(35, 75, by = 10), limits = c(35, 75))  # Adjust y-axis breaks and limits

# Combine the two plots
combined_plot <- plot_grid(plot_combinations_higher, plot_combinations_lower, nrow = 2, align = "v", axis = "l")

# Get the legend
legend <- get_legend(plot_combinations_legend)

# Move the legend to the right
combined_plot_with_legend <- plot_grid(combined_plot, legend, ncol = 2, rel_widths = c(1, 0.2))

# Display the combined plot with legend on the right
print(combined_plot_with_legend)



# Save the combined plot with legend as a high-resolution PNG file
ggsave("combined_plot_with_legend.png", combined_plot_with_legend, dpi = 300)



library(ggplot2)
library(dplyr)
# Filter data for "Yes" values in Fem.V, SubC.V, and IJV columns
filtered_data <- renal_data

# Count the number of occurrences for each variable
counts <- filtered_data %>%
  summarize(Fem.V = sum(Fem.V == "Yes"),
            SubC.V = sum(SubC.V == "Yes"),
            IJV = sum(IJV == "Yes"))

# Convert counts to long format for ggplot
counts_long <- tidyr::pivot_longer(counts, cols = c(Fem.V, SubC.V, IJV), names_to = "Vein", values_to = "Count")

# Create the plot
access_vein_bar_plot <- ggplot(counts_long, aes(x = Vein, y = Count, fill = Vein)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = Count), vjust = -0.5, size = 3.5, color = "black") +
  theme_minimal() +
  labs(x = "Hemodialysis Cathether Venous Access", y = "Frequency", title = "Frequency of 'Yes' Responses for Vein Access Types") +
  guides(fill = FALSE) +
  scale_fill_manual(values = c("blue", "red", "green")) +  # Custom colors if needed
  scale_x_discrete(labels = c("Fem.V" = "Femoral Vein", "SubC.V" = "Subclavian Vein", "IJV" = "Internal Jugular Vein"))

# Save the combined plot with legend as a high-resolution PNG file
ggsave("access vein.png", access_vein_bar_plot, dpi = 300)




