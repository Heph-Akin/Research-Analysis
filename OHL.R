library(googlesheets4)
library(janitor)
library(tidyr)
library(gtsummary)


# Load the Google Sheet by providing the URL or the sheet name
df <- read_sheet("https://docs.google.com/spreadsheets/d/1Ye-u79_hXmIkLP6QNs5mdHtRwEwGNvK2GmrV3FKJF-w/edit?usp=sharing") %>%
  clean_names()

colnames(df)
skimr::skim(df)


### Replacing NA In age
# Find NA values in "age" column
na_indices <- is.na(df$age)

# Calculate min and max age
min_age <- min(df$age, na.rm = TRUE)
max_age <- max(df$age, na.rm = TRUE)

# Generate random numbers within the range
random_ages <- sample(min_age:max_age, sum(na_indices), replace = TRUE)

# Replace NA values with random numbers
df$age[na_indices] <- random_ages

# Check the updated "age" column
print(df$age)


# Define age categories
age_breaks <- c(51, 61, 71, 81, 91, Inf)
age_labels <- c("51-60", "61-70", "71-80", "81-90", ">90")

# Create df$age_cat using cut function
df$age_cat <- cut(df$age, breaks = age_breaks, labels = age_labels, include.lowest = TRUE)

# Check the updated data frame with the new age category variable
print(df$age_cat)





### Replacing NA In gender
# Calculate proportions of "M" and "F" in the "gend" column
gender_counts <- table(df$gend, useNA = "ifany")
gender_proportions <- prop.table(gender_counts)

# Check the proportions
print(gender_proportions)

# Find NA indices in the "gend" column
na_indices_gender <- which(is.na(df$gend))

# Generate random gender values based on proportions
random_genders <- sample(names(gender_proportions), length(na_indices_gender), 
                         replace = TRUE, prob = gender_proportions)

# Replace NA values in "gend" with random genders
df$gend[na_indices_gender] <- random_genders

# Check the updated "gend" column
print(df$gend)



# Create a new column "OHL_cat" based on the condition
df$OHL_cat <- ifelse(df$score >= 4, "High", "Low")

# Check the updated data frame with the new column
print(df)



##Tables
# Demographics Tables
# Summarize demographics with mean ± SD for continuous variables
tbl_age_gender <- df %>%
  select(age, age_cat, gend) %>%
  tbl_summary(
    type = age ~ "continuous",
    statistic = all_continuous() ~ "{mean} ± {sd}",
    digits = list(all_continuous() ~ 2, all_categorical() ~ 1)  # Set 2 decimal places for continuous, 1 decimal place for categorical (percentages)
  ) 

# Define labels for variables
labels <- list(
  com1 = "Communication",
  recep_1 = "Receptivity 1",
  recep_2 = "Receptivity 2",
  und = "Understanding/Education",
  util_1 = "Utilisation 1",
  util2 = "Utilisation 2",
  sup = "Support",
  score = "Score",
  OHL_cat = "Oral Health Literacy Category"
)

# Summarize oral health literacy with mean ± SD for score and proper labels
tbl_oh_literacy <- df %>%
  select(com1, recep_1, recep_2, und, util_1, util2, sup, score, OHL_cat) %>%
  tbl_summary(
    type = score ~ "continuous",
    label = labels,
    statistic = score ~ "{mean} ± {sd}",
    digits = list(score ~ 2)  # Set 2 decimal places for the "score" variable
  )

# Display the summarized oral health literacy table with proper labels
tbl_oh_literacy




df %>%
  select(age_cat, com1, recep_1, recep_2, und, util_1, util2, sup, score, OHL_cat) %>%
  tbl_summary(
    by = age_cat,
    type = score ~ "continuous",
    label = labels,
    statistic = score ~ "{mean} ± {sd}",
    digits = list(score ~ 2),  # Set 2 decimal places for the "score" variable
    percent = "column"
  ) %>%
  add_p()



df %>%
  select(gend, com1, recep_1, recep_2, und, util_1, util2, sup, score, OHL_cat) %>%
  tbl_summary(
    by = gend,
    type = score ~ "continuous",
    label = labels,
    statistic = score ~ "{mean} ± {sd}",
    digits = list(score ~ 2),  # Set 2 decimal places for the "score" variable
    percent = "column"
  ) %>%
  add_p()

