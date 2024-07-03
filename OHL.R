library(googlesheets4)
library(janitor)
library(tidyr)
library(gtsummary)
library(ggstatsplot)


# Load the Google Sheet by providing the URL or the sheet name
df <- read_sheet("https://docs.google.com/spreadsheets/d/1Ye-u79_hXmIkLP6QNs5mdHtRwEwGNvK2GmrV3FKJF-w/edit?usp=sharing") %>%
  clean_names()

# Load the Google Sheet by providing the URL or the sheet name
health_belief <- read_sheet("https://docs.google.com/spreadsheets/d/1GnWhulFPXXO5-5zmNcRhx2xZZpIZ99JteLn9cDT5KW4/edit?usp=sharing") %>%
  clean_names()

motivation <- read.csv("C:/Users/user/Documents/Research-Analysis/motivation_with_dummies.csv")

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

# Convert age_cat to a factor with appropriate order
df$age_cat <- factor(df$age_cat, levels = age_labels, ordered = TRUE)


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

write.csv(df, file = "OHL_1.csv", row.names = FALSE)



##Tables
# Demographics Tables
# Summarize demographics with mean ± SD for continuous variables
df %>%
  select(age, age_cat, gend, und) %>%
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
df %>%
  select(com1, recep_1, recep_2, und, util_1, util2, sup, score, OHL_cat) %>%
  tbl_summary(
    type = score ~ "continuous",
    label = labels,
    statistic = score ~ "{mean} ± {sd}",
    digits = list(all_continuous() ~ 2, all_categorical() ~ 1)  # Set 2 decimal places for the "score" variable
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


df %>%
  select(com1, recep_1, recep_2, und, util_1, util2, sup, score, OHL_cat) %>%
  tbl_summary(
    by = und,
    type = score ~ "continuous",
    label = labels,
    statistic = score ~ "{mean} ± {sd}",
    digits = list(score ~ 2),  # Set 2 decimal places for the "score" variable
    percent = "column"
  ) %>%
  add_p()


ggbetweenstats(
  data  = df,
  x     = und,
  y     = score,
  title = "Oral health literacy across education catetgory"
)


gghistostats(
  data       = df,
  x          = score,
  title      = "Oral health literacy ",
  binwidth   = 0.5
)



##Make age a factor
ggdotplotstats(
  data       = df,
  y          = age_cat,
  x          = score,
  title      = "Distribution of Oral Health literacy across age groups",
  xlab       = "Oral Health literacy score"
)





# Health Belief -----------------------------------------------------------

colnames(health_belief)


# Loop through columns and replace "N/A" and "NA" text with NA
for (col in colnames(health_belief)) {
  health_belief[[col]][health_belief[[col]] %in% c("N/A", "NA")] <- NA
}




health_belief %>%
  select(believe, pain, dis, death, finance, family, prevent, steps, how, trigger, action) %>%
  tbl_summary(
    label = list(
      believe ~ "Perceived Susceptibility: Belief in having oral disease",
      pain ~ "Perceived Severity: Belief that oral disease causes pain",
      dis ~ "Perceived Severity: Belief that oral disease causes disability",
      death ~ "Perceived Severity: Belief that oral disease can cause death",
      finance ~ "Perceived Severity: Belief that oral disease affects finances",
      family ~ "Perceived Severity: Belief that oral disease causes family problems",
      prevent ~ "Perceived Benefits: Belief in ability to prevent oral disease",
      steps ~ "Perceived Barriers: Belief that taking prevention steps is difficult",
      how ~ "Perceived Barriers: How taking prevention steps is difficult",
      trigger ~ "Perceived Barriers: Preventive action trigger",
      action ~ "Self Efficacy: Readiness to take preventive action"
    ),
    digits = list(all_continuous() ~ 2),
    statistic = list(all_continuous() ~ "{mean} ({sd})"),
    missing_text = "Unanswered"
  )




# Motivation --------------------------------------------------------------

colnames(motivation)



# Combine Financial.constraint and Financial.constraint.1 into a single variable
motivation_combined <- motivation %>%
  mutate(Financial_constraint_combined = Financial.constraint + Financial.constraint.1) %>%
  select(-Financial.constraint, -Financial.constraint.1)  # Remove the original variables

# Select the variables for the summary table
motivation_subset <- motivation_combined %>%
  select(PHASES, Distance, Fear.and.anxiety, Ignorance, Lack.of.assistance, Mobility, Financial_constraint_combined)

# Create a gt summary table
tbl_motivation_summary <- motivation_subset %>%
  tbl_summary(
    label = list(
      PHASES ~ "Phases",
      Distance ~ "Distance",
      Fear.and.anxiety ~ "Fear and Anxiety",
      Ignorance ~ "Ignorance",
      Lack.of.assistance ~ "Lack of Assistance",
      Mobility ~ "Mobility",
      Financial_constraint_combined ~ "Combined Financial Constraint"
    ),
    digits = list(all_continuous() ~ 2),
    statistic = list(all_continuous() ~ "{mean} ({sd})")
  )

# Display the gt summary table
tbl_motivation_summary