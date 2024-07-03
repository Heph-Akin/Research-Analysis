library(googlesheets4)
library(janitor)
library(tidyr)
library(gtsummary)
library(ggstatsplot)
library(gtsummary)
library(dplyr)
library(tidyr)
library(tools)
library(ggplot2)
library(forcats)
library(remotes)
library(ggpattern)   


# Load the Google Sheet by providing the URL or the sheet name
df <- read_sheet("https://docs.google.com/spreadsheets/d/1iGKxiNlhMn7i6ZyLK6h1F3E7xYDJ-wo7KNVVjEVdix0/edit?usp=sharing") %>%
  clean_names()

colnames(df)





# Function to count anomalies for a given side
count_anomalies <- function(data, side) {
  side_vars <- c(paste0("hypodontia_", side, 1:8), 
                 paste0("peg_shaped_", side, 1:8), 
                 paste0("impacted_", side, 1:8), 
                 paste0("transposition_", side, 1:8), 
                 paste0("microdont_", side, 1:8), 
                 paste0("supernumerary_", side, 1:8), 
                 paste0("ectopic_eruption_", side, 1:8), 
                 paste0("late_developing_", side, 1:8))
  rowSums(data[, side_vars] == "Yes", na.rm = TRUE)
}

# Apply the function to create new variables and add total anomaly count
df <- df %>%
  mutate(
    cleft_side_anomalies = if_else(cleft_lip_side == "Left", 
                                   count_anomalies(df, "ul") + count_anomalies(df, "ll"),
                                   count_anomalies(df, "ur") + count_anomalies(df, "lr")),
    non_cleft_side_anomalies = if_else(cleft_lip_side == "Left", 
                                       count_anomalies(df, "ur") + count_anomalies(df, "lr"),
                                       count_anomalies(df, "ul") + count_anomalies(df, "ll")),
    total_anomalies = cleft_side_anomalies + non_cleft_side_anomalies
  )



# Create new variables explicitly
df <- df %>%
  mutate(
    hypodontia_present = ifelse(hypodontia_count == 0, "No", "Yes"),
    peg_present = ifelse(peg_count == 0, "No", "Yes"),
    impacted_present = ifelse(impacted_count == 0, "No", "Yes"),
    transposition_present = ifelse(transposition_count == 0, "No", "Yes"),
    microdont_present = ifelse(microdont_count == 0, "No", "Yes"),
    supernumerary_present = ifelse(supernumerary_count == 0, "No", "Yes"),
    ectopic_present = ifelse(ectopic_count == 0, "No", "Yes"),
    late_present = ifelse(late_count == 0, "No", "Yes")
  )

# Demographic Information
df %>%
  select(age, age_cat, gender) %>%
  tbl_summary(
    label = list(
      age ~ "Age",
      age_cat ~ "Age Category",
      gender ~ "Gender"
    ),
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{n} / {N} ({p}%)"),
    missing = "no",
    digits = list(
      all_continuous() ~ 2, 
      all_categorical() ~ 1
    )
  )

# Cleft Information
df %>%
  select(type_of_cleft, cleft_lip_side) %>%
  tbl_summary(
    label = list(
      type_of_cleft ~ "Type of Cleft",
      cleft_lip_side ~ "Cleft Side"
    ),
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    missing = "no",
    digits = list(
      all_continuous() ~ 2, 
      all_categorical() ~ 1
    )
  )

# Anomalies
df %>%
  select(anomaly_present, odontome, hypodontia_count, peg_count, impacted_count, transposition_count, microdont_count, supernumerary_count, ectopic_count, late_count) %>%
  tbl_summary(
    label = list(
      anomaly_present ~ "Dental Anomaly Present",
      odontome ~ "Odontome",
      hypodontia_count ~ "Hypodontia",
      peg_count ~ "Peg Shaped Teeth",
      impacted_count ~ "Impacted Teeth",
      transposition_count ~ "Transposition",
      microdont_count ~ "Microdontia",
      supernumerary_count ~ "Supernumerary Teeth",
      ectopic_count ~ "Ectopic Eruption",
      late_count ~ "Late Developing Teeth"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)",
      all_continuous() ~ "{mean} ({sd})"
    ),
    missing = "no",
    digits = list(
      all_continuous() ~ 2, 
      all_categorical() ~ 1
    ),
    type = list(
      anomaly_present ~ "categorical",
      odontome ~ "categorical",
      hypodontia_count ~ "continuous",
      peg_count ~ "continuous",
      impacted_count ~ "continuous",
      transposition_count ~ "continuous",
      microdont_count ~ "continuous",
      supernumerary_count ~ "continuous",
      ectopic_count ~ "continuous",
      late_count ~ "continuous"
    )
  )


# Define labels for variables
labels <- list(
  odontome = "Odontome",
  hypodontia_present = "Hypodontia",
  peg_present = "Peg Shaped Teeth",
  impacted_present = "Impacted Teeth",
  transposition_present = "Transposition",
  microdont_present = "Microdontia",
  supernumerary_present = "Supernumerary Teeth",
  ectopic_present = "Ectopic Teeth",
  late_present = "Late Developing Teeth"
)

# Create a summary table
df %>%
  select(type_of_cleft, odontome, hypodontia_present, peg_present, impacted_present, transposition_present, microdont_present, supernumerary_present, ectopic_present, late_present) %>%
  tbl_summary(
    by = type_of_cleft,
    label = labels,
    statistic = list(
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_categorical() ~ 1
    ),
    percent = "row"
  ) %>%
  bold_labels() %>%
  italicize_levels() %>%
  add_overall() %>%
  add_p()

# Create a summary table
df %>%
  select(gender, odontome, hypodontia_present, peg_present, impacted_present, transposition_present, microdont_present, supernumerary_present, ectopic_present, late_present) %>%
  tbl_summary(
    by = gender,
    label = labels,
    statistic = list(
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_categorical() ~ 1
    ),
    percent = "row"
  ) %>%
  bold_labels() %>%
  italicize_levels() %>%
  add_overall() %>%
  add_p()

# Define labels for variables
labels <- list(
  gender = "Gender",
  type_of_cleft = "Type of Cleft",
  cleft_symetry = "Cleft Symmetry/Pattern of Cleft",
  cleft_lip_side = "Cleft Lip Side"
)

# Create an improved summary table
df %>%
  select(gender, type_of_cleft, cleft_symetry, cleft_lip_side) %>%
  tbl_summary(
    by = gender,
    label = labels,
    statistic = list(
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = list(
      all_categorical() ~ 1
    )
  ) %>%
  add_p() %>%
  bold_labels() %>%
  italicize_levels() %>%
  modify_header(label = "**Variable**") %>%
  modify_caption("**Summary of Cleft Characteristics by Gender**") %>%
  as_gt()


# Define labels for variables
labels <- list(
  gender = "Gender",
  cleft_symetry = "Cleft Symmetry"
)

# Create summary tables for each type of cleft
summary_table_cleft_lip_alveolus <- df %>%
  select(gender, type_of_cleft, cleft_symetry) %>%
  filter(type_of_cleft == "Cleft Lip and Alveolus") %>%
  select(gender, cleft_symetry) %>%
  tbl_summary(
    by = cleft_symetry,
    label = labels,
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_categorical() ~ 1)
  ) %>%
  add_p()

summary_table_cleft_lip_palate <- df %>%
  select(gender, type_of_cleft, cleft_symetry) %>%
  filter(type_of_cleft == "Cleft Lip and Palate") %>%
  select(gender, cleft_symetry) %>%
  tbl_summary(
    by = cleft_symetry,
    label = labels,
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_categorical() ~ 1)
  ) %>%
  add_p()

summary_table_cleft_lip <- df %>%
  select(gender, type_of_cleft, cleft_symetry) %>%
  filter(type_of_cleft == "Cleft Lip") %>%
  select(gender, cleft_symetry) %>%
  tbl_summary(
    by = cleft_symetry,
    label = labels,
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_categorical() ~ 1)
  ) 

summary_table_incomplete_lip <- df %>%
  select(gender, type_of_cleft, cleft_symetry) %>%
  filter(type_of_cleft == "Incomplete Lip") %>%
  select(gender, cleft_symetry) %>%
  tbl_summary(
    by = cleft_symetry,
    label = labels,
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_categorical() ~ 1)
  )

summary_table_isolated_cleft_palate <- df %>%
  select(gender, type_of_cleft) %>%
  filter(type_of_cleft == "Isolated cleft palate") %>%
  select(gender) %>%
  tbl_summary(
    label = labels,
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_categorical() ~ 1)
  ) 

# Merge the tables
merged_table <- tbl_merge(
  tbls = list(
    summary_table_cleft_lip_alveolus,
    summary_table_cleft_lip_palate,
    summary_table_cleft_lip,
    summary_table_incomplete_lip,
    summary_table_isolated_cleft_palate
  ),
  tab_spanner = c(
    "Cleft Lip and Alveolus",
    "Cleft Lip and Palate",
    "Cleft Lip",
    "Incomplete Lip",
    "Isolated cleft palate"
  )
) %>%
  bold_labels() %>%
  italicize_levels() %>%
  modify_caption("**Summary of Gender by Cleft Symmetry for Different Types of Cleft**") %>%
  as_gt()

# Print the merged table
merged_table

# First table
tbl1 <- df %>%
  select(anomaly_present, type_of_cleft) %>%
  tbl_summary(
    by = anomaly_present,
    label = list(
      anomaly_present ~ "Anomaly Present",
      type_of_cleft ~ "Type of Cleft"
    ),
    percent = "row",
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    missing = "no"
  )

# Second table
tbl2 <- df %>%
  select(type_of_cleft, cleft_lip_side) %>%
  tbl_summary(
    by = cleft_lip_side,
    label = list(
      type_of_cleft ~ "Type of Cleft",
      cleft_lip_side ~ "Cleft Lip Side"
    ),
    percent = "row",
    statistic = list(all_categorical() ~ "{n} / {N} ({p}%)"),
    missing = "no"
  )

# Merging the tables
tbl_combined <- tbl_merge(
  tbls = list(tbl1, tbl2),
  tab_spanner = c("**Anomaly Present by Type of Cleft**", "**Type of Cleft by Cleft Lip Side**")
)

# Display the combined table
tbl_combined




df %>%
  filter(cleft_lip_side != "Left and Right") %>%
  select(cleft_lip_side, cleft_side_anomalies, non_cleft_side_anomalies, total_anomalies) %>%
  mutate(total_anomalies = as.integer(total_anomalies)) %>%
  tbl_summary(by = cleft_lip_side,
              label = list(
                cleft_lip_side = "Cleft Lip Side",
                cleft_side_anomalies = "Cleft Side Anomalies",
                non_cleft_side_anomalies = "Non-Cleft Side Anomalies",
                total_anomalies = "Total Anomalies"
              ),
              type = list(total_anomalies ~ "continuous")) %>%
  add_p() %>%
  add_overall()




# Convert odontome to integer (1 for Yes, 0 for No)
df <- df %>%
  mutate(odontome_int = ifelse(odontome == "Yes", 1, 0))

# Selecting relevant columns for anomalies, integer odontome, and type_of_cleft
df_anomalies <- df %>%
  select(type_of_cleft, odontome_int, hypodontia_count, peg_count, impacted_count, 
         transposition_count, microdont_count, supernumerary_count, 
         ectopic_count, late_count)

# Adding a column for category (if needed)
df_anomalies$category <- "Category"  # Replace with your actual grouping variable if available

# Reshaping data to long format
df_anomalies_long <- df_anomalies %>%
  pivot_longer(cols = c(odontome_int, hypodontia_count, peg_count, impacted_count, 
                        transposition_count, microdont_count, supernumerary_count, 
                        ectopic_count, late_count),
               names_to = "variable", values_to = "count") %>%
  mutate(variable = case_when(
    variable == "odontome_int" ~ "Odontome",
    variable == "hypodontia_count" ~ "Hypodontia",
    variable == "peg_count" ~ "Peg Shaped Teeth",
    variable == "impacted_count" ~ "Impacted Teeth",
    variable == "transposition_count" ~ "Transposition",
    variable == "microdont_count" ~ "Microdontia",
    variable == "supernumerary_count" ~ "Supernumerary Teeth",
    variable == "ectopic_count" ~ "Ectopic Eruption",
    variable == "late_count" ~ "Late Developing Teeth",
    TRUE ~ as.character(variable)
  ))

# Plotting with ggplot2
ggplot(df_anomalies_long, aes(x = variable, y = count, fill = variable)) +
  geom_bar(stat = "identity") +
  labs(x = "Anomaly Type", y = "Frequency", fill = "Anomaly Type") +
  theme_minimal() +
  theme(legend.position = "top") +
  scale_fill_brewer(palette = "Set3")  # Adjust color palette as needed


ggbarstats(
  data             = df,
  x                = type_of_cleft,
  y                = total_anomalies,
  title            = "Cleft type distribution across frequency of anomalies present",
  xlab             = "Frequency of anomalies",
  legend.title     = "Type of Cleft",
  ggplot.component = list(ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(n.dodge = 2))),
  palette          = "Set3"
)
