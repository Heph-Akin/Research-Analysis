library(googlesheets4)
library(janitor)
library(tidyr)
library(gtsummary)
library(ggstatsplot)
library(janitor)
library(tidyr)
library(gtsummary)
library(ggstatsplot)
library(knitr)
library(flextable)
library(ggplot2)
library(dplyr)


# Load the Google Sheet by providing the URL or the sheet name
df <- read_sheet("https://docs.google.com/spreadsheets/d/1K6mQnK8ehLXtHH18zz-CjgP-FLmGTzTUAJkNdj8aHQE/edit?usp=sharing") %>%
  clean_names()

colnames(df)

df_scored <- df

# List of columns to convert
columns_to_convert <- c(
  "x17_does_female_circumcision_involve_the_cutting_of_flesh_from_the_female_genital_area",                            
  "x18_does_female_circumcision_involve_the_sewing_of_the_female_external_genital_area",                               
  "x19_is_female_circumcision_a_practice_that_causes_injuries_such_as_piercing_and_cutting_to_the_vaginal_area",       
  "x20_is_female_circumcision_painless_to_the_victim",
  "x21_can_female_circumcision_cause_bleeding_in_the_victim",
  "x22_can_female_circumcision_cause_wound_infections_in_the_victim",
  "x23_can_viral_infections_such_as_hiv_and_hepatitis_b_virus_be_transmitted_to_the_victim_during_female_circumcision",
  "x24_can_victims_of_female_circumcision_suffer_depression_in_future",
  "x25_can_victims_of_female_circumcision_experience_difficulty_bearing_a_child_in_future",
  "x26_can_female_circumcision_lead_to_infertility",
  "x27_can_female_cause_a_broken_home",
  "x28_can_a_victim_of_female_circumcision_experience_problems_during_urination"
)

# Convert "Yes" to 1 and all other values to 0
df_scored[columns_to_convert] <- lapply(df_scored[columns_to_convert], function(x) ifelse(x == "Yes", 1, 0))

# Verify the conversion
print(df_scored[columns_to_convert])

# List of columns to sum for knowledge_score
knowledge_score_columns <- c(
  "x17_does_female_circumcision_involve_the_cutting_of_flesh_from_the_female_genital_area",                            
  "x18_does_female_circumcision_involve_the_sewing_of_the_female_external_genital_area",                               
  "x19_is_female_circumcision_a_practice_that_causes_injuries_such_as_piercing_and_cutting_to_the_vaginal_area",       
  "x20_is_female_circumcision_painless_to_the_victim",
  "x21_can_female_circumcision_cause_bleeding_in_the_victim",
  "x22_can_female_circumcision_cause_wound_infections_in_the_victim",
  "x23_can_viral_infections_such_as_hiv_and_hepatitis_b_virus_be_transmitted_to_the_victim_during_female_circumcision",
  "x24_can_victims_of_female_circumcision_suffer_depression_in_future",
  "x25_can_victims_of_female_circumcision_experience_difficulty_bearing_a_child_in_future",
  "x26_can_female_circumcision_lead_to_infertility",
  "x27_can_female_cause_a_broken_home",
  "x28_can_a_victim_of_female_circumcision_experience_problems_during_urination"
)

# Create the knowledge_score variable by summing the specified columns
df_scored$knowledge_score <- rowSums(df_scored[knowledge_score_columns], na.rm = TRUE)

# Verify the knowledge_score
print(df_scored$knowledge_score)


# Create knowledge_cat variable
df_scored <- df_scored %>%
  mutate(knowledge_cat = case_when(
    knowledge_score >= 0 & knowledge_score <= 6 ~ "Poor",
    knowledge_score > 6 & knowledge_score <= 12 ~ "Good",
    TRUE ~ NA_character_  # to handle any unexpected values
  ))

# Verify the new variable
table(df_scored$knowledge_cat)




# Define the columns to score with the two methods
method_1_columns <- c(
  "x32_female_genital_mutilation_causes_reduced_sexual_satisfaction_for_the_woman",
  "x35_the_practice_of_female_circumcision_is_an_uncivilized_act_and_should_be_stopped"
)

method_2_columns <- c(
  "x30_female_genital_mutilation_sustains_tradition_and_should_be_continued",
  "x31_female_genital_mutilation_helps_to_promote_a_woman_s_faithfulness_to_her_husband",
  "x33_female_genital_mutilation_makes_the_female_genital_area_more_attractive",
  "x34_the_practice_of_female_genital_mutilation_protects_a_girl_s_virginity",
  "x36_female_circumcision_improves_a_girl_s_chances_of_getting_married",
  "x37_the_practice_of_female_circumcision_is_supported_by_my_religion",
  "x38_females_who_are_not_circumcised_are_likely_to_be_stigmatized_in_the_society",
  "x39_females_that_are_not_circumcised_are_likely_to_be_promiscuous",
  "x40_female_genital_mutilation_is_beneficial_for_the_female",
  "x41_female_genital_mutilation_improves_the_sexual_hygiene_of_the_female",
  "x42_female_circumcision_brings_honor_to_a_girl_child",
  "x43_men_prefer_to_marry_women_who_have_been_circumcised"
)

# Define scoring functions
score_method_1 <- function(response) {
  switch(response,
         "SA" = 5,
         "A" = 4,
         "U" = 3,
         "D" = 2,
         "SD" = 1,
         NA)
}

score_method_2 <- function(response) {
  switch(response,
         "SA" = 1,
         "A" = 2,
         "U" = 3,
         "D" = 4,
         "SD" = 5,
         NA)
}

# Apply the scoring functions to the respective columns
df_scored[method_1_columns] <- lapply(df_scored[method_1_columns], function(col) sapply(col, score_method_1))
df_scored[method_2_columns] <- lapply(df_scored[method_2_columns], function(col) sapply(col, score_method_2))

# Verify the conversion
print(df_scored[c(method_1_columns, method_2_columns)])





# List of columns to sum for perception_score
perception_score_columns <- c(
  "x32_female_genital_mutilation_causes_reduced_sexual_satisfaction_for_the_woman",
  "x35_the_practice_of_female_circumcision_is_an_uncivilized_act_and_should_be_stopped",
  "x30_female_genital_mutilation_sustains_tradition_and_should_be_continued",
  "x31_female_genital_mutilation_helps_to_promote_a_woman_s_faithfulness_to_her_husband",
  "x33_female_genital_mutilation_makes_the_female_genital_area_more_attractive",
  "x34_the_practice_of_female_genital_mutilation_protects_a_girl_s_virginity",
  "x36_female_circumcision_improves_a_girl_s_chances_of_getting_married",
  "x37_the_practice_of_female_circumcision_is_supported_by_my_religion",
  "x38_females_who_are_not_circumcised_are_likely_to_be_stigmatized_in_the_society",
  "x39_females_that_are_not_circumcised_are_likely_to_be_promiscuous",
  "x40_female_genital_mutilation_is_beneficial_for_the_female",
  "x41_female_genital_mutilation_improves_the_sexual_hygiene_of_the_female",
  "x42_female_circumcision_brings_honor_to_a_girl_child",
  "x43_men_prefer_to_marry_women_who_have_been_circumcised"
)

# Create the perception_score variable by summing the specified columns
df_scored$perception_score <- rowSums(df_scored[perception_score_columns], na.rm = TRUE)

# Verify the perception_score
print(df_scored$perception_score)


# Create perception_cat variable
df_scored <- df_scored %>%
  mutate(perception_cat = case_when(
    perception_score >= 1 & perception_score <= 35 ~ "Negative",
    perception_score > 35 & perception_score <= 70 ~ "Positive",
    TRUE ~ NA_character_  # to handle any unexpected values
  ))

# Verify the new variable
table(df_scored$perception_cat)


# Demographics --------------------------------------------------------
# Define labels for variables
labels <- list(
  x1_age_as_at_last_birthday_in_years = "Age at last birthday (in years)",
  x2_highest_educatonal_level_of_respondent = "Highest educational level of respondent",
  x3_marital_status = "Marital status",
  x4_highest_educational_level_of_respondent = "Highest educational level of respondent's husband/partner",
  x5_geopolitical_zone = "Geopolitical zone",
  x6_religion = "Religion",
  x7_type_of_family = "Type of family",
  x8_occupation = "Occupation",
  x9_estimated_household_monthly_income_n = "Estimated household monthly income"
)

# Summarize socio-demographic characteristics
df %>%
  select(x1_age_as_at_last_birthday_in_years, x2_highest_educatonal_level_of_respondent, x3_marital_status, x4_highest_educational_level_of_respondent,
         x5_geopolitical_zone, x6_religion, x7_type_of_family, x8_occupation, x9_estimated_household_monthly_income_n) %>%
  tbl_summary(
    label = labels,
    statistic = list(all_categorical() ~ "{n} ({p}%)", all_continuous() ~ "{mean} ± {sd}"),
    digits = list(all_continuous() ~ 2, all_categorical() ~ 1)
  )


# Obs History -------------------------------------------------------------
# Check and convert relevant columns to factors for categorical variables
df_scored <- df_scored %>% as.data.frame()

# Ensure columns are not lists
df_scored <- df_scored %>%
  mutate(
    x10_how_pregnancies_have_you_ever_had = unlist(x10_how_pregnancies_have_you_ever_had),
    x11_how_many_children_are_alive = unlist(x11_how_many_children_are_alive),
    x12_when_did_you_register_for_ante_natal_in_this_pregnancy = unlist(x12_when_did_you_register_for_ante_natal_in_this_pregnancy)
  )

# Convert relevant columns to factors for categorical variables
df_scored <- df_scored %>%
  mutate(
    x10_how_pregnancies_have_you_ever_had = as.factor(x10_how_pregnancies_have_you_ever_had),
    x11_how_many_children_are_alive = as.factor(x11_how_many_children_are_alive),
    x12_when_did_you_register_for_ante_natal_in_this_pregnancy = as.factor(x12_when_did_you_register_for_ante_natal_in_this_pregnancy)
  )

# Define labels for variables
labels <- list(
  x10_how_pregnancies_have_you_ever_had = "Number of pregnancies",
  x11_how_many_children_are_alive = "Number of living children",
  x12_when_did_you_register_for_ante_natal_in_this_pregnancy = "Antenatal registration in this pregnancy"
)

# Summarize the variables
df_scored %>%
  select(
    x10_how_pregnancies_have_you_ever_had,
    x11_how_many_children_are_alive,
    x12_when_did_you_register_for_ante_natal_in_this_pregnancy
  ) %>%
  tbl_summary(
    label = labels,
    digits = list(all_continuous() ~ 2, all_categorical() ~ 1),
    type = list(
      x10_how_pregnancies_have_you_ever_had ~ "categorical",
      x11_how_many_children_are_alive ~ "categorical",
      x12_when_did_you_register_for_ante_natal_in_this_pregnancy ~ "categorical"
    )
  )


# Awareness --------------------------------------------------------------
# Define labels for variables
labels <- list(
  x13_have_you_ever_heard_of_female_genital_mutilation = "Have you ever heard of female genital mutilation?",
  x15a_your_parents = "Your parents",
  x15b_television = "Television",
  x15c_radio = "Radio",
  x15d_newspaper_magazine = "Newspaper/Magazine",
  x15e_internet = "Internet",
  x15f_health_workers = "Health workers",
  x15g_religious_homes = "Religious homes",
  x15h_school = "School",
  x15i_non_governmental_organizations_ngo = "Non-governmental organizations (NGO)",
  x15_have_you_ever_heard_about_campaigns_against_female_circumcision = "Have you ever heard about campaigns against female circumcision?",
  x16_how_long_ago_have_you_been_hearing_about_female_circumcision = "How long ago have you been hearing about female circumcision?",
  x29_is_there_any_law_that_forbids_the_practice_of_female_circumcision_in_nigeria = "Is there any law that forbids the practice of female circumcision in Nigeria?"
)

# Summarize the variables
df_scored %>%
  select(
    x13_have_you_ever_heard_of_female_genital_mutilation,
    x15a_your_parents,
    x15b_television,
    x15c_radio,
    x15d_newspaper_magazine,
    x15e_internet,
    x15f_health_workers,
    x15g_religious_homes,
    x15h_school,
    x15i_non_governmental_organizations_ngo,
    x15_have_you_ever_heard_about_campaigns_against_female_circumcision,
    x16_how_long_ago_have_you_been_hearing_about_female_circumcision,
    x29_is_there_any_law_that_forbids_the_practice_of_female_circumcision_in_nigeria
  ) %>%
  tbl_summary(
    label = labels,
    type = list(
      x13_have_you_ever_heard_of_female_genital_mutilation ~ "categorical",
      x15a_your_parents ~ "categorical",
      x15b_television ~ "categorical",
      x15c_radio ~ "categorical",
      x15d_newspaper_magazine ~ "categorical",
      x15e_internet ~ "categorical",
      x15f_health_workers ~ "categorical",
      x15g_religious_homes ~ "categorical",
      x15h_school ~ "categorical",
      x15i_non_governmental_organizations_ngo ~ "categorical",
      x15_have_you_ever_heard_about_campaigns_against_female_circumcision ~ "categorical",
      x16_how_long_ago_have_you_been_hearing_about_female_circumcision ~ "categorical",
      x29_is_there_any_law_that_forbids_the_practice_of_female_circumcision_in_nigeria ~ "categorical"
    ),
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_continuous() ~ 2, all_categorical() ~ 1)
  )

# Knowledge ---------------------------------------------------------------
# Define labels for variables
labels <- list(
  `x17_does_female_circumcision_involve_the_cutting_of_flesh_from_the_female_genital_area` = "Does female circumcision involve the cutting of flesh from the female genital area?",
  `x18_does_female_circumcision_involve_the_sewing_of_the_female_external_genital_area` = "Does female circumcision involve the sewing of the female external genital area?",
  `x19_is_female_circumcision_a_practice_that_causes_injuries_such_as_piercing_and_cutting_to_the_vaginal_area` = "Is female circumcision a practice that causes injuries such as piercing and cutting to the vaginal area?",
  `x20_is_female_circumcision_painless_to_the_victim` = "Is female circumcision painless to the victim?",
  `x21_can_female_circumcision_cause_bleeding_in_the_victim` = "Can female circumcision cause bleeding in the victim?",
  `x22_can_female_circumcision_cause_wound_infections_in_the_victim` = "Can female circumcision cause wound infections in the victim?",
  `x23_can_viral_infections_such_as_hiv_and_hepatitis_b_virus_be_transmitted_to_the_victim_during_female_circumcision` = "Can viral infections such as HIV and hepatitis B virus be transmitted to the victim during female circumcision?",
  `x24_can_victims_of_female_circumcision_suffer_depression_in_future` = "Can victims of female circumcision suffer depression in the future?",
  `x25_can_victims_of_female_circumcision_experience_difficulty_bearing_a_child_in_future` = "Can victims of female circumcision experience difficulty bearing a child in the future?",
  `x26_can_female_circumcision_lead_to_infertility` = "Can female circumcision lead to infertility?",
  `x27_can_female_cause_a_broken_home` = "Can female circumcision cause a broken home?",
  `x28_can_a_victim_of_female_circumcision_experience_problems_during_urination` = "Can a victim of female circumcision experience problems during urination?"
)

# Summarize knowledge of FGM
df_scored %>%
  select(
    `x17_does_female_circumcision_involve_the_cutting_of_flesh_from_the_female_genital_area`,                            
    `x18_does_female_circumcision_involve_the_sewing_of_the_female_external_genital_area`,                               
    `x19_is_female_circumcision_a_practice_that_causes_injuries_such_as_piercing_and_cutting_to_the_vaginal_area`,       
    `x20_is_female_circumcision_painless_to_the_victim`,
    `x21_can_female_circumcision_cause_bleeding_in_the_victim`,
    `x22_can_female_circumcision_cause_wound_infections_in_the_victim`,
    `x23_can_viral_infections_such_as_hiv_and_hepatitis_b_virus_be_transmitted_to_the_victim_during_female_circumcision`,
    `x24_can_victims_of_female_circumcision_suffer_depression_in_future`,
    `x25_can_victims_of_female_circumcision_experience_difficulty_bearing_a_child_in_future`,
    `x26_can_female_circumcision_lead_to_infertility`,
    `x27_can_female_cause_a_broken_home`,
    `x28_can_a_victim_of_female_circumcision_experience_problems_during_urination`,
    knowledge_score
  ) %>%
  tbl_summary(
    label = labels,
    statistic = list(all_categorical() ~ "{n} ({p}%)"),
    digits = list(all_continuous() ~ 2, all_categorical() ~ 1)
  )


# Perception --------------------------------------------------------------
# Selecting section E variables
selected_vars <- df %>%
  select(
    x30_female_genital_mutilation_sustains_tradition_and_should_be_continued,
    x31_female_genital_mutilation_helps_to_promote_a_woman_s_faithfulness_to_her_husband,
    x32_female_genital_mutilation_causes_reduced_sexual_satisfaction_for_the_woman,
    x33_female_genital_mutilation_makes_the_female_genital_area_more_attractive,
    x34_the_practice_of_female_genital_mutilation_protects_a_girl_s_virginity,
    x35_the_practice_of_female_circumcision_is_an_uncivilized_act_and_should_be_stopped,
    x36_female_circumcision_improves_a_girl_s_chances_of_getting_married,
    x37_the_practice_of_female_circumcision_is_supported_by_my_religion,
    x38_females_who_are_not_circumcised_are_likely_to_be_stigmatized_in_the_society,
    x39_females_that_are_not_circumcised_are_likely_to_be_promiscuous,
    x40_female_genital_mutilation_is_beneficial_for_the_female,
    x41_female_genital_mutilation_improves_the_sexual_hygiene_of_the_female,
    x42_female_circumcision_brings_honor_to_a_girl_child,
    x43_men_prefer_to_marry_women_who_have_been_circumcised
  )

# Remapping variable names
selected_vars <- selected_vars %>%
  pivot_longer(cols = everything(), names_to = "Item", values_to = "value") %>%
  mutate(
    Item = case_when(
      Item == "x30_female_genital_mutilation_sustains_tradition_and_should_be_continued" ~ "FGM sustains tradition and should be continued",
      Item == "x31_female_genital_mutilation_helps_to_promote_a_woman_s_faithfulness_to_her_husband" ~ "FGM helps to promote a woman’s faithfulness to her husband",
      Item == "x32_female_genital_mutilation_causes_reduced_sexual_satisfaction_for_the_woman" ~ "FGM causes reduced sexual satisfaction for the woman",
      Item == "x33_female_genital_mutilation_makes_the_female_genital_area_more_attractive" ~ "FGM makes the female genital area more attractive",
      Item == "x34_the_practice_of_female_genital_mutilation_protects_a_girl_s_virginity" ~ "The practice of FGM protects a girl’s virginity",
      Item == "x35_the_practice_of_female_circumcision_is_an_uncivilized_act_and_should_be_stopped" ~ "The practice of female circumcision is an uncivilized act and should be stopped",
      Item == "x36_female_circumcision_improves_a_girl_s_chances_of_getting_married" ~ "Female circumcision improves a girl’s chances of getting married",
      Item == "x37_the_practice_of_female_circumcision_is_supported_by_my_religion" ~ "The practice of female circumcision is supported by my religion",
      Item == "x38_females_who_are_not_circumcised_are_likely_to_be_stigmatized_in_the_society" ~ "Females who are not circumcised are likely to be stigmatized in society",
      Item == "x39_females_that_are_not_circumcised_are_likely_to_be_promiscuous" ~ "Females that are not circumcised are likely to be promiscuous",
      Item == "x40_female_genital_mutilation_is_beneficial_for_the_female" ~ "FGM is beneficial for the female",
      Item == "x41_female_genital_mutilation_improves_the_sexual_hygiene_of_the_female" ~ "FGM improves the sexual hygiene of the female",
      Item == "x42_female_circumcision_brings_honor_to_a_girl_child" ~ "Female circumcision brings honor to a girl child",
      Item == "x43_men_prefer_to_marry_women_who_have_been_circumcised" ~ "Men prefer to marry women who have been circumcised",
      TRUE ~ Item
    ),
    value = fct_relevel(value, "SA", "A", "U", "D", "SD")
  )

# Create the summary table
summary_table <- selected_vars %>%
  tbl_summary(
    by = value,
    digits = list(all_continuous() ~ 2, all_categorical() ~ 1),
    percent = "row",
    label = list(Item ~ "**Support for Items**")
  ) %>%
  modify_header(label = "**Perception of Female Genital Mutilation**") %>%
  as_gt()

# Print the table
print(summary_table)

df_scored %>%
  select(perception_score) %>%
  tbl_summary()

# Intending Practices -----------------------------------------------------
# Inspect the structure of the problematic column
str(df_scored$x47_how_many_of_your_daughters_above_are_circumcised)

# If the column is a list, convert it to a suitable type, e.g., character
if (is.list(df_scored$x47_how_many_of_your_daughters_above_are_circumcised)) {
  df_scored$x47_how_many_of_your_daughters_above_are_circumcised <- sapply(df_scored$x47_how_many_of_your_daughters_above_are_circumcised, toString)
}

# Proceed with converting relevant columns to factors for categorical variables
df_scored <- df_scored %>%
  mutate(
    x44_were_you_circumcised = as.factor(x44_were_you_circumcised),
    x45_do_you_have_a_daughter_s = as.factor(x45_do_you_have_a_daughter_s),
    x46_if_yes_to_question_45_how_many_daughters_do_you_have = as.factor(x46_if_yes_to_question_45_how_many_daughters_do_you_have),
    x47_how_many_of_your_daughters_above_are_circumcised = as.factor(x47_how_many_of_your_daughters_above_are_circumcised),
    x48_how_old_was_your_last_daughter_when_she_was_circumcised_in_years_58 = as.factor(x48_how_old_was_your_last_daughter_when_she_was_circumcised_in_years_58),
    x48_how_old_was_your_last_daughter_when_she_was_circumcised_in_years_59 = as.factor(x48_how_old_was_your_last_daughter_when_she_was_circumcised_in_years_59),
    x49_who_performed_the_circumcision = as.factor(x49_who_performed_the_circumcision),
    x50_do_you_plan_to_have_your_daughter_s_circumcised_in_the_future = as.factor(x50_do_you_plan_to_have_your_daughter_s_circumcised_in_the_future),
    x51_if_yes_to_question_50_who_do_you_plan_to_perform_the_circumcision = as.factor(x51_if_yes_to_question_50_who_do_you_plan_to_perform_the_circumcision),
    x52_i_i_plan_to_circumcise_my_daughter_because_it_is_a_practice_that_is_required_by_my_religion = as.factor(x52_i_i_plan_to_circumcise_my_daughter_because_it_is_a_practice_that_is_required_by_my_religion),
    x52_ii_i_plan_to_circumcise_my_daughter_to_avoid_promiscuity = as.factor(x52_ii_i_plan_to_circumcise_my_daughter_to_avoid_promiscuity),
    x52_iii_it_increases_the_chances_of_getting_married = as.factor(x52_iii_it_increases_the_chances_of_getting_married),
    x52_iv_it_decreases_sexual_desire = as.factor(x52_iv_it_decreases_sexual_desire),
    x52_v_to_please_future_husband = as.factor(x52_v_to_please_future_husband),
    x52_vi_to_preserve_her_virginity = as.factor(x52_vi_to_preserve_her_virginity),
    x52_vii_it_is_a_culture_tradition_that_should_be_followed = as.factor(x52_vii_it_is_a_culture_tradition_that_should_be_followed),
    x53_i_it_can_result_medical_complications = as.factor(x53_i_it_can_result_medical_complications),
    x53_ii_it_is_not_supported_by_my_religion = as.factor(x53_ii_it_is_not_supported_by_my_religion),
    x53_iii_it_is_against_the_law_of_nigeria = as.factor(x53_iii_it_is_against_the_law_of_nigeria),
    x53_iv_there_is_no_need_to_do_it = as.factor(x53_iv_there_is_no_need_to_do_it),
    x53_v_my_husband_will_not_accept_it = as.factor(x53_v_my_husband_will_not_accept_it)
  )


# Convert relevant columns to factors for categorical variables
df_scored <- df_scored %>%
  mutate(
    x44_were_you_circumcised = as.factor(x44_were_you_circumcised),
    x45_do_you_have_a_daughter_s = as.factor(x45_do_you_have_a_daughter_s),
    x46_if_yes_to_question_45_how_many_daughters_do_you_have = as.factor(x46_if_yes_to_question_45_how_many_daughters_do_you_have),
    x47_how_many_of_your_daughters_above_are_circumcised = as.factor(x47_how_many_of_your_daughters_above_are_circumcised),
    x48_how_old_was_your_last_daughter_when_she_was_circumcised_in_years_58 = as.factor(x48_how_old_was_your_last_daughter_when_she_was_circumcised_in_years_58),
    x48_how_old_was_your_last_daughter_when_she_was_circumcised_in_years_59 = as.factor(x48_how_old_was_your_last_daughter_when_she_was_circumcised_in_years_59),
    x49_who_performed_the_circumcision = as.factor(x49_who_performed_the_circumcision),
    x50_do_you_plan_to_have_your_daughter_s_circumcised_in_the_future = as.factor(x50_do_you_plan_to_have_your_daughter_s_circumcised_in_the_future),
    x51_if_yes_to_question_50_who_do_you_plan_to_perform_the_circumcision = as.factor(x51_if_yes_to_question_50_who_do_you_plan_to_perform_the_circumcision),
    x52_i_i_plan_to_circumcise_my_daughter_because_it_is_a_practice_that_is_required_by_my_religion = as.factor(x52_i_i_plan_to_circumcise_my_daughter_because_it_is_a_practice_that_is_required_by_my_religion),
    x52_ii_i_plan_to_circumcise_my_daughter_to_avoid_promiscuity = as.factor(x52_ii_i_plan_to_circumcise_my_daughter_to_avoid_promiscuity),
    x52_iii_it_increases_the_chances_of_getting_married = as.factor(x52_iii_it_increases_the_chances_of_getting_married),
    x52_iv_it_decreases_sexual_desire = as.factor(x52_iv_it_decreases_sexual_desire),
    x52_v_to_please_future_husband = as.factor(x52_v_to_please_future_husband),
    x52_vi_to_preserve_her_virginity = as.factor(x52_vi_to_preserve_her_virginity),
    x52_vii_it_is_a_culture_tradition_that_should_be_followed = as.factor(x52_vii_it_is_a_culture_tradition_that_should_be_followed),
    x53_i_it_can_result_medical_complications = as.factor(x53_i_it_can_result_medical_complications),
    x53_ii_it_is_not_supported_by_my_religion = as.factor(x53_ii_it_is_not_supported_by_my_religion),
    x53_iii_it_is_against_the_law_of_nigeria = as.factor(x53_iii_it_is_against_the_law_of_nigeria),
    x53_iv_there_is_no_need_to_do_it = as.factor(x53_iv_there_is_no_need_to_do_it),
    x53_v_my_husband_will_not_accept_it = as.factor(x53_v_my_husband_will_not_accept_it)
  )

# Define labels for variables
labels <- list(
  x44_were_you_circumcised = "Were you circumcised?",
  x45_do_you_have_a_daughter_s = "Do you have a daughter(s)?",
  x46_if_yes_to_question_45_how_many_daughters_do_you_have = "How many daughters do you have?",
  x47_how_many_of_your_daughters_above_are_circumcised = "How many of your daughters are circumcised?",
  x48_how_old_was_your_last_daughter_when_she_was_circumcised_in_years_58 = "Age of last daughter when circumcised (58)",
  x48_how_old_was_your_last_daughter_when_she_was_circumcised_in_years_59 = "Age of last daughter when circumcised (59)",
  x49_who_performed_the_circumcision = "Who performed the circumcision?",
  x50_do_you_plan_to_have_your_daughter_s_circumcised_in_the_future = "Do you plan to circumcise your daughter(s) in the future?",
  x51_if_yes_to_question_50_who_do_you_plan_to_perform_the_circumcision = "Who do you plan to perform the circumcision?",
  x52_i_i_plan_to_circumcise_my_daughter_because_it_is_a_practice_that_is_required_by_my_religion = "Plan to circumcise daughter due to religious requirement",
  x52_ii_i_plan_to_circumcise_my_daughter_to_avoid_promiscuity = "Plan to circumcise daughter to avoid promiscuity",
  x52_iii_it_increases_the_chances_of_getting_married = "Increases chances of getting married",
  x52_iv_it_decreases_sexual_desire = "Decreases sexual desire",
  x52_v_to_please_future_husband = "To please future husband",
  x52_vi_to_preserve_her_virginity = "To preserve her virginity",
  x52_vii_it_is_a_culture_tradition_that_should_be_followed = "It's a cultural tradition that should be followed",
  x53_i_it_can_result_medical_complications = "Can result in medical complications",
  x53_ii_it_is_not_supported_by_my_religion = "Not supported by my religion",
  x53_iii_it_is_against_the_law_of_nigeria = "Against the law in Nigeria",
  x53_iv_there_is_no_need_to_do_it = "There is no need to do it",
  x53_v_my_husband_will_not_accept_it = "My husband will not accept it"
)

# Summarize the variables
summary_table <- df_scored %>%
  select(
    x44_were_you_circumcised,
    x45_do_you_have_a_daughter_s,
    x46_if_yes_to_question_45_how_many_daughters_do_you_have,
    x47_how_many_of_your_daughters_above_are_circumcised,
    x48_how_old_was_your_last_daughter_when_she_was_circumcised_in_years_58,
    x48_how_old_was_your_last_daughter_when_she_was_circumcised_in_years_59,
    x49_who_performed_the_circumcision,
    x50_do_you_plan_to_have_your_daughter_s_circumcised_in_the_future,
    x51_if_yes_to_question_50_who_do_you_plan_to_perform_the_circumcision,
    x52_i_i_plan_to_circumcise_my_daughter_because_it_is_a_practice_that_is_required_by_my_religion,
    x52_ii_i_plan_to_circumcise_my_daughter_to_avoid_promiscuity,
    x52_iii_it_increases_the_chances_of_getting_married,
    x52_iv_it_decreases_sexual_desire,
    x52_v_to_please_future_husband,
    x52_vi_to_preserve_her_virginity,
    x52_vii_it_is_a_culture_tradition_that_should_be_followed,
    x53_i_it_can_result_medical_complications,
    x53_ii_it_is_not_supported_by_my_religion,
    x53_iii_it_is_against_the_law_of_nigeria,
    x53_iv_there_is_no_need_to_do_it,
    x53_v_my_husband_will_not_accept_it
  ) %>%
  tbl_summary(
    label = labels
  ) %>%
  modify_header(label = "**Question**") %>%
  as_gt()

# Print the table
print(summary_table)


# Chi-Square --------------------------------------------------------------
# Define labels for variables
labels <- list(
  x1_age_as_at_last_birthday_in_years = "Age at last birthday (in years)",
  x2_highest_educatonal_level_of_respondent = "Highest educational level of respondent",
  x3_marital_status = "Marital status",
  x4_highest_educational_level_of_respondent = "Highest educational level of respondent's husband/partner",
  x5_geopolitical_zone = "Geopolitical zone",
  x6_religion = "Religion",
  x7_type_of_family = "Type of family",
  x8_occupation = "Occupation",
  x9_estimated_household_monthly_income_n = "Estimated household monthly income"
)

# Summarize socio-demographic characteristics
df_scored %>%
  select(x1_age_as_at_last_birthday_in_years, x2_highest_educatonal_level_of_respondent, x3_marital_status, x4_highest_educational_level_of_respondent,
         x5_geopolitical_zone, x6_religion, x7_type_of_family, x8_occupation, x9_estimated_household_monthly_income_n, knowledge_score, knowledge_cat) %>%
  tbl_summary(
    label = labels,
    statistic = list(all_categorical() ~ "{n} ({p}%)", all_continuous() ~ "{mean} ± {sd}"),
    digits = list(all_continuous() ~ 2, all_categorical() ~ 1), 
    by = knowledge_cat
  ) %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Knowledge Category**") %>%
  add_p() 



df_scored %>%
  select(x1_age_as_at_last_birthday_in_years, x2_highest_educatonal_level_of_respondent, x3_marital_status, x4_highest_educational_level_of_respondent,
         x5_geopolitical_zone, x6_religion, x7_type_of_family, x8_occupation, x9_estimated_household_monthly_income_n, perception_score, perception_cat) %>%
  tbl_summary(
    label = labels,
    statistic = list(all_categorical() ~ "{n} ({p}%)", all_continuous() ~ "{mean} ± {sd}"),
    digits = list(all_continuous() ~ 2, all_categorical() ~ 1), 
    by = perception_cat
  ) %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Perception Category**") %>%
  add_p()

df_scored %>%
  select(x1_age_as_at_last_birthday_in_years, x2_highest_educatonal_level_of_respondent, x3_marital_status, x4_highest_educational_level_of_respondent,
         x5_geopolitical_zone, x6_religion, x7_type_of_family, x8_occupation, x9_estimated_household_monthly_income_n, current_practice) %>%
  tbl_summary(
    label = labels,
    statistic = list(all_categorical() ~ "{n} ({p}%)", all_continuous() ~ "{mean} ± {sd}"),
    digits = list(all_continuous() ~ 2, all_categorical() ~ 1), 
    by = current_practice
  ) %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Intent to Practice**") %>%
  add_p() %>%
  add_overall()

df_scored %>%
  select(x1_age_as_at_last_birthday_in_years, x2_highest_educatonal_level_of_respondent, x3_marital_status, x4_highest_educational_level_of_respondent,
         x5_geopolitical_zone, x6_religion, x7_type_of_family, x8_occupation, x9_estimated_household_monthly_income_n, intending_practice) %>%
  tbl_summary(
    label = labels,
    statistic = list(all_categorical() ~ "{n} ({p}%)", all_continuous() ~ "{mean} ± {sd}"),
    digits = list(all_continuous() ~ 2, all_categorical() ~ 1), 
    by = intending_practice,
    digits = list(all_continuous() ~ 2, all_categorical() ~ 1)
  ) %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Intent to Practice**") %>%
  add_p() %>%
  add_overall()

df_scored %>%
  select(knowledge_score, knowledge_cat, perception_score, perception_cat, current_practice, intending_practice) %>%
  tbl_summary(
    by = intending_practice,
    statistic = list(all_categorical() ~ "{n} ({p}%)", all_continuous() ~ "{mean} ± {sd}"),
    digits = list(all_continuous() ~ 2, all_categorical() ~ 1)
  ) %>%
  modify_header(label ~ "**Variable**") %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Intent to Practice**") %>%
  add_p() %>%
  add_overall()
  
  

# Charts ------------------------------------------------------------------
# Load necessary libraries (if not already installed)
# install.packages(c("likert", "ggplot2"))
library(likert)
library(ggplot2)

# Assuming 'df' is your dataset
# Make sure to replace 'df' with your actual dataset name

# Subset the Likert variables from your dataset
likert_data <- df[, c(
  "x30_female_genital_mutilation_sustains_tradition_and_should_be_continued",
  "x31_female_genital_mutilation_helps_to_promote_a_woman_s_faithfulness_to_her_husband",
  "x32_female_genital_mutilation_causes_reduced_sexual_satisfaction_for_the_woman",
  "x33_female_genital_mutilation_makes_the_female_genital_area_more_attractive",
  "x34_the_practice_of_female_genital_mutilation_protects_a_girl_s_virginity",
  "x35_the_practice_of_female_circumcision_is_an_uncivilized_act_and_should_be_stopped",
  "x36_female_circumcision_improves_a_girl_s_chances_of_getting_married",
  "x37_the_practice_of_female_circumcision_is_supported_by_my_religion",
  "x38_females_who_are_not_circumcised_are_likely_to_be_stigmatized_in_the_society",
  "x39_females_that_are_not_circumcised_are_likely_to_be_promiscuous",
  "x40_female_genital_mutilation_is_beneficial_for_the_female",
  "x41_female_genital_mutilation_improves_the_sexual_hygiene_of_the_female",
  "x42_female_circumcision_brings_honor_to_a_girl_child",
  "x43_men_prefer_to_marry_women_who_have_been_circumcised"
)]

# Convert factor levels to Likert scale labels
likert_data <- data.frame(lapply(likert_data, function(x) {
  factor(x, levels = c("SA", "A", "U", "D", "SD"), 
         labels = c("Strongly Agree", "Agree", "Undecided", "Disagree", "Strongly Disagree"))
}))

# Group "Strongly Agree" and "Agree" as "Agree" and "Strongly Disagree" and "Disagree" as "Disagree"
likert_data <- data.frame(lapply(likert_data, function(x) {
  factor(x, levels = c("Strongly Agree", "Agree", "Undecided", "Disagree", "Strongly Disagree"), 
         labels = c("Agree", "Agree", "Undecided", "Disagree", "Disagree"))
}))

# Define explicit mappings for variable names
variable_mappings <- c(
  "x30_female_genital_mutilation_sustains_tradition_and_should_be_continued" = "FGM sustains tradition",
  "x31_female_genital_mutilation_helps_to_promote_a_woman_s_faithfulness_to_her_husband" = "FGM promotes faithfulness",
  "x32_female_genital_mutilation_causes_reduced_sexual_satisfaction_for_the_woman" = "FGM reduces sexual satisfaction",
  "x33_female_genital_mutilation_makes_the_female_genital_area_more_attractive" = "FGM makes genital area attractive",
  "x34_the_practice_of_female_genital_mutilation_protects_a_girl_s_virginity" = "FGM protects virginity",
  "x35_the_practice_of_female_circumcision_is_an_uncivilized_act_and_should_be_stopped" = "FGM is uncivilized",
  "x36_female_circumcision_improves_a_girl_s_chances_of_getting_married" = "FGM improves marriage chances",
  "x37_the_practice_of_female_circumcision_is_supported_by_my_religion" = "FGM supported by religion",
  "x38_females_who_are_not_circumcised_are_likely_to_be_stigmatized_in_the_society" = "Uncircumcised girls stigmatized",
  "x39_females_that_are_not_circumcised_are_likely_to_be_promiscuous" = "Uncircumcised girls promiscuous",
  "x40_female_genital_mutilation_is_beneficial_for_the_female" = "FGM beneficial for females",
  "x41_female_genital_mutilation_improves_the_sexual_hygiene_of_the_female" = "FGM improves sexual hygiene",
  "x42_female_circumcision_brings_honor_to_a_girl_child" = "FGM brings honor",
  "x43_men_prefer_to_marry_women_who_have_been_circumcised" = "Men prefer circumcised women"
)

# Rename columns using the explicit mapping
colnames(likert_data) <- variable_mappings[colnames(likert_data)]

# Create a Likert plot
likert_plot <- likert.bar.plot(
  likert(likert_data),
  colors = c("#4e79a7", "#f28e2b", "#76b7b2", "#e15759",  "#59a14f")
)

# Customize ggplot appearance
likert_plot <- likert_plot +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(face = "bold", size = 20),  # Use face instead of weight
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14),  # Increase x-axis text size
    axis.text.y = element_text(size = 14),  # Increase y-axis text size
    legend.position = "bottom",  # Maintain legend at the bottom
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 14),  # Adjust legend text size
    plot.title = element_text(size = 16, hjust = 0.5),  # Adjust title size and center
    plot.subtitle = element_text(size = 14)  # Adjust subtitle size
  ) +
  labs(title = "Perceptions on Female Genital Mutilation/Circumcision")

# Display the Likert plot
print(likert_plot)





colnames(df_scored)

# Plot the distribution of perception scores by knowledge category using kernel density plots
ggplot(df_scored, aes(x = perception_score, fill = knowledge_cat)) +
  geom_density(alpha = 0.4) +
  labs(
    title = "Distribution of Perception Scores by Knowledge Category",
    x = "Perception Score",
    y = "Density",
    fill = "Knowledge Category"
  )

# Compare knowledge scores across estimated household monthly income categories
ggbetweenstats(
  data  = df_scored,
  x     = x9_estimated_household_monthly_income_n,
  y     = knowledge_score,
  title = "Knowledge Scores Across Estimated Household Monthly Income Categories",
  xlab  = "Estimated Household Monthly Income",
  ylab  = "Knowledge Score"
)

# Histogram of knowledge scores
gghistostats(
  data       = df_scored,
  x          = knowledge_score,
  title      = "Distribution of Knowledge Scores",
  xlab       = "Knowledge Score",
  ylab       = "Frequency",
  binwidth   = 1
)

# Histogram of perception scores
gghistostats(
  data       = df_scored,
  x          = perception_score,
  title      = "Distribution of Perception Scores",
  xlab       = "Perception Score",
  ylab       = "Frequency",
  binwidth   = 1
)

# Dot plot of knowledge scores across estimated household monthly income categories
ggdotplotstats(
  data       = df_scored,
  y          = x9_estimated_household_monthly_income_n,
  x          = knowledge_score,
  title      = "Knowledge Scores Across Estimated Household Monthly Income Categories",
  xlab       = "Knowledge Score",
  ylab       = "Estimated Household Monthly Income",
  type       = "nonparametric"
)

# Dot plot of knowledge scores across geopolitical zones
ggdotplotstats(
  data       = df_scored,
  y          = x5_geopolitical_zone,
  x          = knowledge_score,
  title      = "Knowledge Scores Across Geopolitical Zones",
  xlab       = "Knowledge Score",
  ylab       = "Geopolitical Zone",
  type       = "parametric",
  centrality.type = "parametric",
  centrality.plotting = TRUE
)




# Regression --------------------------------------------------------------

# Run the regression model
model <- glm(knowledge_score ~ 
              x1_age_as_at_last_birthday_in_years + 
              x2_highest_educatonal_level_of_respondent + 
              x3_marital_status + 
              x4_highest_educational_level_of_respondent + 
              x5_geopolitical_zone + 
              x6_religion + 
              x7_type_of_family + 
              x8_occupation + 
              x9_estimated_household_monthly_income_n, 
            data = df_scored)

# Create a gtsummary table for the regression results
regression_table <- tbl_regression(model, 
                                   label = labels, 
                                   intercept = TRUE)

# Print the regression summary table
print(regression_table)


model <- glm(perception_score ~ 
               x1_age_as_at_last_birthday_in_years + 
               x2_highest_educatonal_level_of_respondent + 
               x3_marital_status + 
               x4_highest_educational_level_of_respondent + 
               x5_geopolitical_zone + 
               x6_religion + 
               x7_type_of_family + 
               x8_occupation + 
               x9_estimated_household_monthly_income_n, 
             data = df_scored)

# Create a gtsummary table for the regression results
regression_table <- tbl_regression(model, 
                                   label = labels, 
                                   intercept = TRUE)

# Print the regression summary table
print(regression_table)





#Regression for intending Practice
# Convert intending_practice to a binary variable
df_scored <- df_scored %>%
  mutate(intending_practice = case_when(
    intending_practice == "Yes" ~ 1,
    intending_practice == "No" ~ 0,
    TRUE ~ NA_real_
  ))

# Remove rows with NA in intending_practice
df_scored <- df_scored %>% filter(!is.na(intending_practice))

# Define the labels
labels <- list(
  x1_age_as_at_last_birthday_in_years = "Age at last birthday (in years)",
  x2_highest_educatonal_level_of_respondent = "Highest educational level of respondent",
  x3_marital_status = "Marital status",
  x4_highest_educational_level_of_respondent = "Highest educational level of respondent's husband/partner",
  x5_geopolitical_zone = "Geopolitical zone",
  x6_religion = "Religion",
  x7_type_of_family = "Type of family",
  x8_occupation = "Occupation",
  x9_estimated_household_monthly_income_n = "Estimated household monthly income"
)

# Run the logistic regression model
model <- glm(intending_practice ~ 
               x1_age_as_at_last_birthday_in_years + 
               x2_highest_educatonal_level_of_respondent + 
               x3_marital_status + 
               x4_highest_educational_level_of_respondent + 
               x5_geopolitical_zone + 
               x6_religion + 
               x7_type_of_family + 
               x8_occupation + 
               x9_estimated_household_monthly_income_n, 
             data = df_scored, 
             family = binomial)

# Create a gtsummary table for the regression results
regression_table <- tbl_regression(model, 
                                   label = labels) %>%
  bold_p(t = 0.050) %>%
  bold_labels() %>%
  italicize_levels()

# Print the regression summary table
print(regression_table)

#Regression for intending Practice
# Convert intending_practice to a binary variable
df_scored <- df_scored %>%
  mutate(intending_practice = case_when(
    intending_practice == "Yes" ~ 1,
    intending_practice == "No" ~ 0,
    TRUE ~ NA_real_
  ))

# Remove rows with NA in intending_practice
df_scored <- df_scored %>% filter(!is.na(intending_practice))

# Define the labels
labels <- list(
  x1_age_as_at_last_birthday_in_years = "Age at last birthday (in years)",
  x2_highest_educatonal_level_of_respondent = "Highest educational level of respondent",
  x3_marital_status = "Marital status",
  x4_highest_educational_level_of_respondent = "Highest educational level of respondent's husband/partner",
  x5_geopolitical_zone = "Geopolitical zone",
  x6_religion = "Religion",
  x7_type_of_family = "Type of family",
  x8_occupation = "Occupation",
  x9_estimated_household_monthly_income_n = "Estimated household monthly income"
)

# Run the logistic regression model
model <- glm(intending_practice ~ 
               x1_age_as_at_last_birthday_in_years + 
               x2_highest_educatonal_level_of_respondent + 
               x3_marital_status + 
               x4_highest_educational_level_of_respondent + 
               x5_geopolitical_zone + 
               x6_religion + 
               x7_type_of_family + 
               x8_occupation + 
               x9_estimated_household_monthly_income_n, 
             data = df_scored, 
             family = binomial)

# Create a gtsummary table for the regression results
regression_table <- tbl_regression(model, 
                                   label = labels) %>%
  bold_p(t = 0.050) %>%
  bold_labels() %>%
  italicize_levels()

# Print the regression summary table
print(regression_table)













#Regression for Current Practice
# Convert intending_practice to a binary variable
df_scored <- df_scored %>%
  mutate(current_practice = case_when(
    intending_practice == "Yes" ~ 1,
    intending_practice == "No" ~ 0,
    TRUE ~ NA_real_
  ))

# Remove rows with NA in intending_practice
df_scored_current <- df_scored %>% filter(!is.na(current_practice))

# Define the labels
labels <- list(
  x1_age_as_at_last_birthday_in_years = "Age at last birthday (in years)",
  x2_highest_educatonal_level_of_respondent = "Highest educational level of respondent",
  x3_marital_status = "Marital status",
  x4_highest_educational_level_of_respondent = "Highest educational level of respondent's husband/partner",
  x5_geopolitical_zone = "Geopolitical zone",
  x6_religion = "Religion",
  x7_type_of_family = "Type of family",
  x8_occupation = "Occupation",
  x9_estimated_household_monthly_income_n = "Estimated household monthly income"
)

# Run the logistic regression model
model <- glm(current_practice ~ 
               x1_age_as_at_last_birthday_in_years + 
               x2_highest_educatonal_level_of_respondent + 
               x3_marital_status + 
               x4_highest_educational_level_of_respondent + 
               x5_geopolitical_zone + 
               x6_religion + 
               x7_type_of_family + 
               x8_occupation + 
               x9_estimated_household_monthly_income_n, 
             data = df_scored_current, 
             family = binomial)

# Create a gtsummary table for the regression results
regression_table <- tbl_regression(model, 
                                   label = labels) %>%
  bold_p(t = 0.050) %>%
  bold_labels() %>%
  italicize_levels()

# Print the regression summary table
print(regression_table)




# Map Chart ---------------------------------------------------------------


# Install and load the required packages
install.packages("naijR")
library(naijR)
library(ggplot2)

# Get all states
all_states <- states()

# Create a sample data frame with values for each state
set.seed(123)  # For reproducibility
values <- runif(length(all_states), min = 0, max = 100)  # Example values for each state

# Create a data frame
data <- data.frame(state = all_states, value = values)

# Create the choropleth map
map_ng(
  region = data$state,
  x = data$value,
  breaks = c(0, 20, 40, 60, 80, 100),
  categories = c("Very Low", "Low", "Medium", "High", "Very High"),
  col = 'YlOrRd',
  show.text = TRUE
)

# Save the map as a PNG file
ggsave("nigeria_state_choropleth_map.png", plot = last_plot(), width = 10, height = 7, dpi = 300)

states(gpz = "ne", all = TRUE) 
states(gpz = "nw", all = TRUE) 
states(gpz = "nc", all = TRUE) 
states(gpz = "se", all = TRUE) 
states(gpz = "ss", all = TRUE) 
states(gpz = "sw", all = TRUE) 


# Define the mapping of regions to states
region_to_states <- list(
  "North East" = states(gpz = "ne", all = TRUE),
  "North West" = states(gpz = "nw", all = TRUE),
  "North Central" = c(states(gpz = "nc", all = TRUE), "Federal Capital Territory"),
  "South East" = states(gpz = "se", all = TRUE),
  "South South" = states(gpz = "ss", all = TRUE),
  "South West" = states(gpz = "sw", all = TRUE)
)

# Sample data frame with values for each region
region_values <- data.frame(
  region = c("North East", "North West", "North Central", "South East", "South South", "South West"),
  value = c(50, 70, 45, 60, 80, 55)
)

# Convert the data frame to long format
long_format <- region_values %>%
  mutate(states = map(region, ~ region_to_states[[.]])) %>%
  unnest(states)

# Rename columns for clarity
long_format <- long_format %>%
  rename(state = states)

# View the resulting long format data frame
print(long_format)

# Create the choropleth map
map_ng(
  region = long_format$state,
  x = long_format$value,
  breaks = c(0, 20, 40, 60, 80, 100),
  categories = c("Very Low", "Low", "Medium", "High", "Very High"),
  col = 'YlOrRd',
  show.text = TRUE
)

# Save the map as a PNG file
ggsave("nigeria_state_choropleth_map.png", plot = last_plot(), width = 10, height = 7, dpi = 300)


colnames(df_scored)


# Convert df_scored to long format
long_format <- df_scored %>%
  mutate(states = map(x5_geopolitical_zone, ~ region_to_states[[.]])) %>%
  unnest(states) %>%
  rename(state = states, value = intending_practice)

# Calculate the proportion of "Yes" for each state
aggregated_data <- long_format %>%
  group_by(state) %>%
  summarize(value = mean(value == "Yes", na.rm = TRUE))

# View the resulting aggregated data frame
print(aggregated_data)

# Create the choropleth map
map_ng(
  region = aggregated_data$state,
  x = aggregated_data$value,
  breaks = c(0, 0.2, 0.4, 0.6, 0.8, 1.0),
  categories = c("Very Low", "Low", "Medium", "High", "Very High"),
  col = 'YlOrRd',
  show.text = TRUE
)

# Save the map as a PNG file
ggsave("nigeria_state_choropleth_map.png", plot = last_plot(), width = 10, height = 7, dpi = 300)



# Convert df_scored to long format for knowledge_score
long_format_knowledge_score <- df_scored %>%
  mutate(states = map(x5_geopolitical_zone, ~ region_to_states[[.]])) %>%
  unnest(states) %>%
  rename(state = states, value = knowledge_score)

# Calculate the average knowledge_score for each state
aggregated_data_knowledge_score <- long_format_knowledge_score %>%
  group_by(state) %>%
  summarize(value = mean(value, na.rm = TRUE))

# View the resulting aggregated data frame for knowledge_score
print(aggregated_data_knowledge_score)

# Create the choropleth map for knowledge_score with specified breaks and categories
map_ng(
  region = aggregated_data_knowledge_score$state,
  x = aggregated_data_knowledge_score$value,
  breaks = c(0, 6, 12),
  categories = c("Poor", "Good"),
  col = c("red"),  
  show.text = TRUE,
  title = "Knowledge of Female Genital Mutilation Across Nigerian Regions",
  leg.title = "Knowledge Level",
  legend.labels = c("Poor (0-6)", "Good (>6)")
)


# Save the map as a PNG file
ggsave("nigeria_state_choropleth_map_knowledge_score.png", plot = last_plot(), width = 10, height = 7, dpi = 300)






# Convert df_scored to long format for perception_cat
long_format_perception_score <- df_scored %>%
  mutate(states = map(x5_geopolitical_zone, ~ region_to_states[[.]])) %>%
  unnest(states) %>%
  rename(state = states, value = perception_score)

# Calculate the proportion of "Yes" for each state for perception_cat
aggregated_data_perception_score <- long_format_perception_score %>%
  group_by(state) %>%
  summarize(value = mean(value, na.rm = TRUE))

# Create the choropleth map for perception_cat with specified breaks and categories
map_ng(
  region = aggregated_data_perception_score$state,
  x = aggregated_data_perception_score$value,
  breaks = c(1, 40, 70),
  categories = c("Negative", "Positive"),
  col = c("red"),  
  show.text = TRUE,
  title = "Perception of Female Genital Mutilation Across Nigerian Regions",
  leg.title = "Perception Level",
  legend.labels = c("Low (0-0.5)", "High (>0.5)")
)

# Save the map as a PNG file
ggsave("nigeria_state_choropleth_map_perception_cat.png", plot = last_plot(), width = 10, height = 7, dpi = 300)
