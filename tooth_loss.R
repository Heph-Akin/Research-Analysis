library(googlesheets4)
library(janitor)
library(tidyr)
library(gtsummary)
library(ggstatsplot)
library(gtsummary)
library(dplyr)
library(tidyr)
library(tools)
library(forcats)


# Load the Google Sheet by providing the URL or the sheet name
df <- read_sheet("https://docs.google.com/spreadsheets/d/1FzGD4oN_EtYMeQIAO1YnCVSxs9G-CXPjpOFN38JMS5E/edit?usp=sharing") %>%
  clean_names()


#Caries Missing
df <- df %>%
  mutate(
    missing_incisors_caries = ifelse(
      missing_teeth_due_to_caries_ul1 == "Yes" | missing_teeth_due_to_caries_ul2 == "Yes" |
        missing_teeth_due_to_caries_ll1 == "Yes" | missing_teeth_due_to_caries_ll2 == "Yes" |
        missing_teeth_due_to_caries_lr1 == "Yes" | missing_teeth_due_to_caries_lr2 == "Yes" |
        missing_teeth_due_to_caries_ur1 == "Yes" | missing_teeth_due_to_caries_ur2 == "Yes",
      "Yes",
      "No"
    ),
    missing_canines_caries = ifelse(
      missing_teeth_due_to_caries_ul3 == "Yes" |
        missing_teeth_due_to_caries_ll3 == "Yes" |
        missing_teeth_due_to_caries_lr3 == "Yes" |
        missing_teeth_due_to_caries_ur3 == "Yes",
      "Yes",
      "No"
    ),
    missing_premolars_caries = ifelse(
      missing_teeth_due_to_caries_ul4 == "Yes" |
        missing_teeth_due_to_caries_ul5 == "Yes" |
        missing_teeth_due_to_caries_ll4 == "Yes" |
        missing_teeth_due_to_caries_ll5 == "Yes" |
        missing_teeth_due_to_caries_lr4 == "Yes" |
        missing_teeth_due_to_caries_lr5 == "Yes" |
        missing_teeth_due_to_caries_ur4 == "Yes" |
        missing_teeth_due_to_caries_ur5 == "Yes",
      "Yes",
      "No"
    ),
    missing_molars_caries = ifelse(
      missing_teeth_due_to_caries_ul6 == "Yes" |
        missing_teeth_due_to_caries_ul7 == "Yes" |
        missing_teeth_due_to_caries_ul8 == "Yes" |
        missing_teeth_due_to_caries_ll6 == "Yes" |
        missing_teeth_due_to_caries_ll7 == "Yes" |
        missing_teeth_due_to_caries_ll8 == "Yes" |
        missing_teeth_due_to_caries_lr6 == "Yes" |
        missing_teeth_due_to_caries_lr7 == "Yes" |
        missing_teeth_due_to_caries_lr8 == "Yes" |
        missing_teeth_due_to_caries_ur6 == "Yes" |
        missing_teeth_due_to_caries_ur7 == "Yes" |
        missing_teeth_due_to_caries_ur8 == "Yes",
      "Yes",
      "No"
    )
  )

#Caries
df <- df %>%
  mutate(
    caries_incisors = ifelse(
      caries_ul1 == "Yes" | caries_ul2 == "Yes" |
        caries_ll1 == "Yes" | caries_ll2 == "Yes" |
        caries_lr1 == "Yes" | caries_lr2 == "Yes" |
        caries_ur1 == "Yes" | caries_ur2 == "Yes",
      "Yes",
      "No"
    ),
    caries_canines = ifelse(
      caries_ul3 == "Yes" |
        caries_ll3 == "Yes" |
        caries_lr3 == "Yes" |
        caries_ur3 == "Yes",
      "Yes",
      "No"
    ),
    caries_premolars = ifelse(
      caries_ul4 == "Yes" |
        caries_ul5 == "Yes" |
        caries_ll4 == "Yes" |
        caries_ll5 == "Yes" |
        caries_lr4 == "Yes" |
        caries_lr5 == "Yes" |
        caries_ur4 == "Yes" |
        caries_ur5 == "Yes",
      "Yes",
      "No"
    ),
    caries_molars = ifelse(
      caries_ul6 == "Yes" |
        caries_ul7 == "Yes" |
        caries_ul8 == "Yes" |
        caries_ll6 == "Yes" |
        caries_ll7 == "Yes" |
        caries_ll8 == "Yes" |
        caries_lr6 == "Yes" |
        caries_lr7 == "Yes" |
        caries_lr8 == "Yes" |
        caries_ur6 == "Yes" |
        caries_ur7 == "Yes" |
        caries_ur8 == "Yes",
      "Yes",
      "No"
    )
  )

#Missing perio
df <- df %>%
  mutate(
    missing_incisors_perio = ifelse(
      missing_teeth_due_to_periodontal_gum_disease_ul1 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_ul2 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_ll1 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_ll2 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_lr1 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_lr2 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_ur1 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_ur2 == "Yes",
      "Yes",
      "No"
    ),
    missing_canines_perio = ifelse(
      missing_teeth_due_to_periodontal_gum_disease_ul3 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_ll3 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_lr3 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_ur3 == "Yes",
      "Yes",
      "No"
    ),
    missing_premolars_perio = ifelse(
      missing_teeth_due_to_periodontal_gum_disease_ul4 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_ul5 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_ll4 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_ll5 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_lr4 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_lr5 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_ur4 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_ur5 == "Yes",
      "Yes",
      "No"
    ),
    missing_molars_perio = ifelse(
      missing_teeth_due_to_periodontal_gum_disease_ul6 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_ul7 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_ul8 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_ll6 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_ll7 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_ll8 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_lr6 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_lr7 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_lr8 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_ur6 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_ur7 == "Yes" |
        missing_teeth_due_to_periodontal_gum_disease_ur8 == "Yes",
      "Yes",
      "No"
    )
  )

#missing fracture
df <- df %>%
  mutate(
    missing_incisors_fracture = ifelse(
      missing_teeth_due_to_fracture_ul1 == "Yes" |
        missing_teeth_due_to_fracture_ul2 == "Yes" |
        missing_teeth_due_to_fracture_ll1 == "Yes" |
        missing_teeth_due_to_fracture_ll2 == "Yes" |
        missing_teeth_due_to_fracture_lr1 == "Yes" |
        missing_teeth_due_to_fracture_lr2 == "Yes" |
        missing_teeth_due_to_fracture_ur1 == "Yes" |
        missing_teeth_due_to_fracture_ur2 == "Yes",
      "Yes",
      "No"
    ),
    missing_canines_fracture = ifelse(
      missing_teeth_due_to_fracture_ul3 == "Yes" |
        missing_teeth_due_to_fracture_ll3 == "Yes" |
        missing_teeth_due_to_fracture_lr3 == "Yes" |
        missing_teeth_due_to_fracture_ur3 == "Yes",
      "Yes",
      "No"
    ),
    missing_premolars_fracture = ifelse(
      missing_teeth_due_to_fracture_ul4 == "Yes" |
        missing_teeth_due_to_fracture_ul5 == "Yes" |
        missing_teeth_due_to_fracture_ll4 == "Yes" |
        missing_teeth_due_to_fracture_ll5 == "Yes" |
        missing_teeth_due_to_fracture_lr4 == "Yes" |
        missing_teeth_due_to_fracture_lr5 == "Yes" |
        missing_teeth_due_to_fracture_ur4 == "Yes" |
        missing_teeth_due_to_fracture_ur5 == "Yes",
      "Yes",
      "No"
    ),
    missing_molars_fracture = ifelse(
      missing_teeth_due_to_fracture_ul6 == "Yes" |
        missing_teeth_due_to_fracture_ul7 == "Yes" |
        missing_teeth_due_to_fracture_ul8 == "Yes" |
        missing_teeth_due_to_fracture_ll6 == "Yes" |
        missing_teeth_due_to_fracture_ll7 == "Yes" |
        missing_teeth_due_to_fracture_ll8 == "Yes" |
        missing_teeth_due_to_fracture_lr6 == "Yes" |
        missing_teeth_due_to_fracture_lr7 == "Yes" |
        missing_teeth_due_to_fracture_lr8 == "Yes" |
        missing_teeth_due_to_fracture_ur6 == "Yes" |
        missing_teeth_due_to_fracture_ur7 == "Yes" |
        missing_teeth_due_to_fracture_ur8 == "Yes",
      "Yes",
      "No"
    )
  )

#Congenital Missing teeth
df <- df %>%
  mutate(
    congenital_missing_incisors = ifelse(
      congenital_missing_teeth_ul1 == "Yes" |
        congenital_missing_teeth_ul2 == "Yes" |
        congenital_missing_teeth_ll1 == "Yes" |
        congenital_missing_teeth_ll2 == "Yes" |
        congenital_missing_teeth_lr1 == "Yes" |
        congenital_missing_teeth_lr2 == "Yes" |
        congenital_missing_teeth_ur1 == "Yes" |
        congenital_missing_teeth_ur2 == "Yes",
      "Yes",
      "No"
    ),
    congenital_missing_canines = ifelse(
      congenital_missing_teeth_ul3 == "Yes" |
        congenital_missing_teeth_ll3 == "Yes" |
        congenital_missing_teeth_lr3 == "Yes" |
        congenital_missing_teeth_ur3 == "Yes",
      "Yes",
      "No"
    ),
    congenital_missing_premolars = ifelse(
      congenital_missing_teeth_ul4 == "Yes" |
        congenital_missing_teeth_ul5 == "Yes" |
        congenital_missing_teeth_ll4 == "Yes" |
        congenital_missing_teeth_ll5 == "Yes" |
        congenital_missing_teeth_lr4 == "Yes" |
        congenital_missing_teeth_lr5 == "Yes" |
        congenital_missing_teeth_ur4 == "Yes" |
        congenital_missing_teeth_ur5 == "Yes",
      "Yes",
      "No"
    ),
    congenital_missing_molars = ifelse(
      congenital_missing_teeth_ul6 == "Yes" |
        congenital_missing_teeth_ul7 == "Yes" |
        congenital_missing_teeth_ul8 == "Yes" |
        congenital_missing_teeth_ll6 == "Yes" |
        congenital_missing_teeth_ll7 == "Yes" |
        congenital_missing_teeth_ll8 == "Yes" |
        congenital_missing_teeth_lr6 == "Yes" |
        congenital_missing_teeth_lr7 == "Yes" |
        congenital_missing_teeth_lr8 == "Yes" |
        congenital_missing_teeth_ur6 == "Yes" |
        congenital_missing_teeth_ur7 == "Yes" |
        congenital_missing_teeth_ur8 == "Yes",
      "Yes",
      "No"
    )
  )

#Mobile teeth
df <- df %>%
  mutate(
    mobile_incisors = ifelse(
      mobile_teeth_ul1 == "Yes" |
        mobile_teeth_ul2 == "Yes" |
        mobile_teeth_ll1 == "Yes" |
        mobile_teeth_ll2 == "Yes" |
        mobile_teeth_lr1 == "Yes" |
        mobile_teeth_lr2 == "Yes" |
        mobile_teeth_ur1 == "Yes" |
        mobile_teeth_ur2 == "Yes",
      "Yes",
      "No"
    ),
    mobile_canines = ifelse(
      mobile_teeth_ul3 == "Yes" |
        mobile_teeth_ll3 == "Yes" |
        mobile_teeth_lr3 == "Yes" |
        mobile_teeth_ur3 == "Yes",
      "Yes",
      "No"
    ),
    mobile_premolars = ifelse(
      mobile_teeth_ul4 == "Yes" |
        mobile_teeth_ul5 == "Yes" |
        mobile_teeth_ll4 == "Yes" |
        mobile_teeth_ll5 == "Yes" |
        mobile_teeth_lr4 == "Yes" |
        mobile_teeth_lr5 == "Yes" |
        mobile_teeth_ur4 == "Yes" |
        mobile_teeth_ur5 == "Yes",
      "Yes",
      "No"
    ),
    mobile_molars = ifelse(
      mobile_teeth_ul6 == "Yes" |
        mobile_teeth_ul7 == "Yes" |
        mobile_teeth_ul8 == "Yes" |
        mobile_teeth_ll6 == "Yes" |
        mobile_teeth_ll7 == "Yes" |
        mobile_teeth_ll8 == "Yes" |
        mobile_teeth_lr6 == "Yes" |
        mobile_teeth_lr7 == "Yes" |
        mobile_teeth_lr8 == "Yes" |
        mobile_teeth_ur6 == "Yes" |
        mobile_teeth_ur7 == "Yes" |
        mobile_teeth_ur8 == "Yes",
      "Yes",
      "No"
    )
  )



library(gtsummary)

# Define labels for variables
labels_A <- list(
  age = "Age",
  gender = "Gender",
  marital_staus = "Marital Status",
  what_is_your_occupation = "Occupation",
  state_of_origin = "State of Origin",
  are_you_nigerian = "Nigerian",
  education_level = "Educational Level",
  family_income = "Family Income"
)

# Summarize Demographic Details
df %>%
  select(age, gender, marital_staus, what_is_your_occupation, state_of_origin, are_you_nigerian, education_level, family_income) %>%
  tbl_summary(
    label = labels_A,
    digits = list(all_continuous() ~ 2, all_categorical() ~ 1)
  ) %>%
  modify_caption("**Table 1. Sociodemographic Distribution of Respondents**")


# Define labels for variables
labels_B <- list(
  missing_incisors_caries = "Missing Incisors (Caries)",
  missing_canines_caries = "Missing Canines (Caries)",
  missing_premolars_caries = "Missing Premolars (Caries)",
  missing_molars_caries = "Missing Molars (Caries)",
  caries_incisors = "Caries Incisors",
  caries_canines = "Caries Canines",
  caries_premolars = "Caries Premolars",
  caries_molars = "Caries Molars",
  missing_incisors_perio = "Missing Incisors (Perio)",
  missing_canines_perio = "Missing Canines (Perio)",
  missing_premolars_perio = "Missing Premolars (Perio)",
  missing_molars_perio = "Missing Molars (Perio)",
  missing_incisors_fracture = "Missing Incisors (Fracture)",
  missing_canines_fracture = "Missing Canines (Fracture)",
  missing_premolars_fracture = "Missing Premolars (Fracture)",
  missing_molars_fracture = "Missing Molars (Fracture)",
  congenital_missing_incisors = "Congenital Missing Incisors",
  congenital_missing_canines = "Congenital Missing Canines",
  congenital_missing_premolars = "Congenital Missing Premolars",
  congenital_missing_molars = "Congenital Missing Molars",
  mobile_incisors = "Mobile Incisors",
  mobile_canines = "Mobile Canines",
  mobile_premolars = "Mobile Premolars",
  mobile_molars = "Mobile Molars"
)

# Summarize Oral Examination
df %>%
  select(missing_incisors_caries, missing_canines_caries, missing_premolars_caries, missing_molars_caries,
         caries_incisors, caries_canines, caries_premolars, caries_molars,
         missing_incisors_perio, missing_canines_perio, missing_premolars_perio, missing_molars_perio,
         missing_incisors_fracture, missing_canines_fracture, missing_premolars_fracture, missing_molars_fracture,
         congenital_missing_incisors, congenital_missing_canines, congenital_missing_premolars, congenital_missing_molars,
         mobile_incisors, mobile_canines, mobile_premolars, mobile_molars) %>%
  tbl_summary(
    label = labels_B,
    digits = list(all_categorical() ~ 1)
  ) %>%
  
  modify_caption("**Table 2: Tooth Mortality Distribution of Respondents**")






# Define labels for variables
labels_C <- list(
  have_you_ever_lost_a_tooth = "Ever Lost a Tooth",
  how_many_teeth_have_you_lost = "Number of Teeth Lost",
  what_was_the_reason_for_the_tooth_loss_pain_from_tooth_decay = "Reason for Tooth Loss: Pain from Tooth Decay",
  what_was_the_reason_for_the_tooth_loss_trauma = "Reason for Tooth Loss: Trauma",
  what_was_the_reason_for_the_tooth_loss_gum_problems = "Reason for Tooth Loss: Gum Problems",
  what_was_the_reason_for_the_tooth_loss_ortho = "Reason for Tooth Loss: Ortho",
  have_you_had_any_dental_treatment_such_as_denture_fabrication_to_replace_missing_tooth = "Dental Treatment to Replace Missing Tooth",
  how_long_have_you_lost_the_tooth_teeth = "Duration of Tooth Loss"
)

# Summarize Oral Health History and Current Status
df %>%
  select(have_you_ever_lost_a_tooth, how_many_teeth_have_you_lost, 
         what_was_the_reason_for_the_tooth_loss_pain_from_tooth_decay, 
         what_was_the_reason_for_the_tooth_loss_trauma, 
         what_was_the_reason_for_the_tooth_loss_gum_problems, 
         what_was_the_reason_for_the_tooth_loss_ortho, 
         have_you_had_any_dental_treatment_such_as_denture_fabrication_to_replace_missing_tooth, 
         how_long_have_you_lost_the_tooth_teeth) %>%
  tbl_summary(
    label = labels_C,
    missing_text = "No Tooth Loss",
    digits = list(all_categorical() ~ 1)
  )


# Define labels for variables
labels_D <- list(
  are_there_any_specific_factors_you_believe_contributed_to_your_tooth_loss_such_as_poor_oral_hygiene = "Factors Contributing to Tooth Loss: Poor Oral Hygiene",
  are_there_any_specific_factors_you_believe_contributed_to_your_tooth_loss_such_as_trauma = "Factors Contributing to Tooth Loss: Trauma",
  are_there_any_specific_factors_you_believe_contributed_to_your_tooth_loss_such_as_genetic_predisposition = "Factors Contributing to Tooth Loss: Genetic Predisposition",
  how_frequently_do_you_visit_a_dentist_for_regular_check_ups_and_oral_health_maintanance = "Frequency of Dental Visits",
  have_you_received_oral_health_education_or_instructions_on_preventive_measures_to_maintain_oral_health_and_prevent_tooth_loss = "Received Oral Health Education",
  are_there_cultural_or_societal_factors_that_influence_oral_health_practices_in_your_community = "Cultural Factors Influencing Oral Health",
  are_there_cultural_or_societal_factors_that_is_against_tooth_loss_in_the_community = "Cultural Factors Against Tooth Loss",
  in_your_opinion_how_accessible_is_dental_care_in_this_community_and_do_you_have_any_barriers_in_accessible_it = "Accessibility of Dental Care",
  do_you_believe_that_there_is_enough_awareness_about_importance_of_oral_hygiene_and_its_role_in_preventing_tooth_loss_among_adult = "Awareness of Oral Hygiene",
  do_you_know_about_gum_disease_before_now = "Knowledge of Gum Disease",
  do_you_believe_if_they_could_contribute_to_tooth_loss = "Belief in Gum Disease Contribution to Tooth Loss",
  do_you_think_dietary_habits_have_influence_on_the_prevalence_of_tooth_loss_in_this_community = "Influence of Dietary Habits on Tooth Loss",
  have_yoy_had_pain_from_your_teeth_before = "Experience of Pain from Teeth",
  do_you_think_parafunctional_habits_have_influence_on_the_prevalence_of_tooth_loss = "Influence of Parafunctional Habits on Tooth Loss"
)

# Summarize Participants Perception of Oral Health
df %>%
  select(are_there_any_specific_factors_you_believe_contributed_to_your_tooth_loss_such_as_poor_oral_hygiene,
         are_there_any_specific_factors_you_believe_contributed_to_your_tooth_loss_such_as_trauma,
         are_there_any_specific_factors_you_believe_contributed_to_your_tooth_loss_such_as_genetic_predisposition,
         how_frequently_do_you_visit_a_dentist_for_regular_check_ups_and_oral_health_maintanance,
         have_you_received_oral_health_education_or_instructions_on_preventive_measures_to_maintain_oral_health_and_prevent_tooth_loss,
         are_there_cultural_or_societal_factors_that_influence_oral_health_practices_in_your_community,
         are_there_cultural_or_societal_factors_that_is_against_tooth_loss_in_the_community,
         in_your_opinion_how_accessible_is_dental_care_in_this_community_and_do_you_have_any_barriers_in_accessible_it,
         do_you_believe_that_there_is_enough_awareness_about_importance_of_oral_hygiene_and_its_role_in_preventing_tooth_loss_among_adult,
         do_you_know_about_gum_disease_before_now,
         do_you_believe_if_they_could_contribute_to_tooth_loss,
         do_you_think_dietary_habits_have_influence_on_the_prevalence_of_tooth_loss_in_this_community,
         have_yoy_had_pain_from_your_teeth_before,
         do_you_think_parafunctional_habits_have_influence_on_the_prevalence_of_tooth_loss) %>%
  tbl_summary(
    label = labels_D,
    digits = list(all_categorical() ~ 1)
  )


# Assuming your dataframe is named 'df'
summary_table <- df %>%
  select(
    have_you_had_trouble_pronouncing_any_word_because_of_your_missing_tooth,
    have_you_felt_that_your_sense_of_taste_has_worsened_because_of_your_missing_tooth_teeth,
    have_you_found_it_uncomfortable_to_eat_any_food_because_of_your_missing_tooth_teeth,
    have_you_been_self_conscious_because_of_your_missing_tooth_teeth,
    have_you_felt_tense_or_ashamed_because_of_your_missing_tooth_teeth,
    have_your_diet_being_unsatisfactory_because_of_your_missing_tooth_teeth,
    have_you_stopped_some_food_or_diet_because_of_your_missing_tooth_teeth,
    has_your_missing_tooth_teeth_given_you_concern_before,
    have_you_been_embarrassed_because_of_problems_with_your_teeth,
    have_you_being_a_bit_irritable_with_other_people_because_of_problems_with_your_teeth,
    have_you_had_difficulty_doing_your_usual_jobs_because_of_your_missing_tooth_teeth,
    have_you_felt_like_life_in_general_was_less_satisfying_because_of_your_missing_tooth_teeth,
    have_you_being_totally_unable_to_function_optimally_at_work_or_at_homebecause_of_your_missing_tooth_teeth,
    are_you_aware_of_preventive_measures_to_avoid_tooth_loss
  ) %>%
  tbl_summary(
    label = list(
      have_you_had_trouble_pronouncing_any_word_because_of_your_missing_tooth ~ "Trouble pronouncing words",
      have_you_felt_that_your_sense_of_taste_has_worsened_because_of_your_missing_tooth_teeth ~ "Sense of taste worsened",
      have_you_found_it_uncomfortable_to_eat_any_food_because_of_your_missing_tooth_teeth ~ "Uncomfortable eating food",
      have_you_been_self_conscious_because_of_your_missing_tooth_teeth ~ "Self-conscious",
      have_you_felt_tense_or_ashamed_because_of_your_missing_tooth_teeth ~ "Tense or ashamed",
      have_your_diet_being_unsatisfactory_because_of_your_missing_tooth_teeth ~ "Diet unsatisfactory",
      have_you_stopped_some_food_or_diet_because_of_your_missing_tooth_teeth ~ "Stopped some food or diet",
      has_your_missing_tooth_teeth_given_you_concern_before ~ "Concern about missing tooth",
      have_you_been_embarrassed_because_of_problems_with_your_teeth ~ "Embarrassed by tooth problems",
      have_you_being_a_bit_irritable_with_other_people_because_of_problems_with_your_teeth ~ "Irritable with others",
      have_you_had_difficulty_doing_your_usual_jobs_because_of_your_missing_tooth_teeth ~ "Difficulty with usual jobs",
      have_you_felt_like_life_in_general_was_less_satisfying_because_of_your_missing_tooth_teeth ~ "Life less satisfying",
      have_you_being_totally_unable_to_function_optimally_at_work_or_at_homebecause_of_your_missing_tooth_teeth ~ "Unable to function optimally",
      are_you_aware_of_preventive_measures_to_avoid_tooth_loss ~ "Aware of preventive measures"
    ),
    type = list(
      everything() ~ "categorical"
    ),
    statistic = list(
      all_categorical() ~ "{n} ({p}%)"
    ),
    missing = "no"
  )

# Print the summary table
summary_table



# Selecting section E variable
selected_vars <- df %>%
  select(
    have_you_had_trouble_pronouncing_any_word_because_of_your_missing_tooth,
    have_you_felt_that_your_sense_of_taste_has_worsened_because_of_your_missing_tooth_teeth,
    have_you_found_it_uncomfortable_to_eat_any_food_because_of_your_missing_tooth_teeth,
    have_you_been_self_conscious_because_of_your_missing_tooth_teeth,
    have_you_felt_tense_or_ashamed_because_of_your_missing_tooth_teeth,
    have_your_diet_being_unsatisfactory_because_of_your_missing_tooth_teeth,
    have_you_stopped_some_food_or_diet_because_of_your_missing_tooth_teeth,
    has_your_missing_tooth_teeth_given_you_concern_before,
    have_you_been_embarrassed_because_of_problems_with_your_teeth,
    have_you_being_a_bit_irritable_with_other_people_because_of_problems_with_your_teeth,
    have_you_had_difficulty_doing_your_usual_jobs_because_of_your_missing_tooth_teeth,
    have_you_felt_like_life_in_general_was_less_satisfying_because_of_your_missing_tooth_teeth,
    have_you_being_totally_unable_to_function_optimally_at_work_or_at_homebecause_of_your_missing_tooth_teeth
  )

# Remapping variable names
selected_vars <- selected_vars %>%
  pivot_longer(cols = everything(), names_to = "Item", values_to = "value") %>%
  mutate(
    Item = case_when(
      Item == "have_you_had_trouble_pronouncing_any_word_because_of_your_missing_tooth" ~ "Trouble Pronouncing Words",
      Item == "have_you_felt_that_your_sense_of_taste_has_worsened_because_of_your_missing_tooth_teeth" ~ "Sense of Taste Worsened",
      Item == "have_you_found_it_uncomfortable_to_eat_any_food_because_of_your_missing_tooth_teeth" ~ "Uncomfortable Eating Food",
      Item == "have_you_been_self_conscious_because_of_your_missing_tooth_teeth" ~ "Self-Conscious",
      Item == "have_you_felt_tense_or_ashamed_because_of_your_missing_tooth_teeth" ~ "Felt Tense or Ashamed",
      Item == "have_your_diet_being_unsatisfactory_because_of_your_missing_tooth_teeth" ~ "Diet Unsatisfactory",
      Item == "have_you_stopped_some_food_or_diet_because_of_your_missing_tooth_teeth" ~ "Stopped Some Foods or Diet",
      Item == "has_your_missing_tooth_teeth_given_you_concern_before" ~ "Missing Teeth Given Concern",
      Item == "have_you_been_embarrassed_because_of_problems_with_your_teeth" ~ "Embarrassed Because of Teeth Problems",
      Item == "have_you_being_a_bit_irritable_with_other_people_because_of_problems_with_your_teeth" ~ "Irritable with Others",
      Item == "have_you_had_difficulty_doing_your_usual_jobs_because_of_your_missing_tooth_teeth" ~ "Difficulty Doing Usual Jobs",
      Item == "have_you_felt_like_life_in_general_was_less_satisfying_because_of_your_missing_tooth_teeth" ~ "Life Less Satisfying",
      Item == "have_you_being_totally_unable_to_function_optimally_at_work_or_at_homebecause_of_your_missing_tooth_teeth" ~ "Unable to Function Optimally",
      TRUE ~ Item
    ),
    value = fct_relevel(value, "Very Often", "Fairly Often", "Occasionally", "Hardly Ever", "Never")
  )

# Create the summary table
selected_vars %>%
  tbl_summary(
    by = value,
    percent = "row"
  ) %>%
  modify_header(label = "**Support for Items**") %>%
  as_gt()



## Association Statistics
# Define labels for missing teeth variables
labels_missing_teeth <- list(
  missing_incisors_caries = "Missing Incisors (Caries)",
  missing_canines_caries = "Missing Canines (Caries)",
  missing_premolars_caries = "Missing Premolars (Caries)",
  missing_molars_caries = "Missing Molars (Caries)",
  missing_incisors_perio = "Missing Incisors (Perio)",
  missing_canines_perio = "Missing Canines (Perio)",
  missing_premolars_perio = "Missing Premolars (Perio)",
  missing_molars_perio = "Missing Molars (Perio)",
  missing_incisors_fracture = "Missing Incisors (Fracture)",
  missing_canines_fracture = "Missing Canines (Fracture)",
  missing_premolars_fracture = "Missing Premolars (Fracture)",
  missing_molars_fracture = "Missing Molars (Fracture)",
  congenital_missing_incisors = "Congenital Missing Incisors",
  congenital_missing_canines = "Congenital Missing Canines",
  congenital_missing_premolars = "Congenital Missing Premolars",
  congenital_missing_molars = "Congenital Missing Molars"
)

# Define labels for caries and mobile teeth variables
labels_caries_mobile <- list(
  caries_incisors = "Caries Incisors",
  caries_canines = "Caries Canines",
  caries_premolars = "Caries Premolars",
  caries_molars = "Caries Molars",
  mobile_incisors = "Mobile Incisors",
  mobile_canines = "Mobile Canines",
  mobile_premolars = "Mobile Premolars",
  mobile_molars = "Mobile Molars"
)

# Summarize missing teeth
df %>%
  select(missing_incisors_caries, missing_canines_caries, missing_premolars_caries, missing_molars_caries,
         missing_incisors_perio, missing_canines_perio, missing_premolars_perio, missing_molars_perio,
         missing_incisors_fracture, missing_canines_fracture, missing_premolars_fracture, missing_molars_fracture,
         congenital_missing_incisors, congenital_missing_canines, congenital_missing_premolars, congenital_missing_molars,
         gender) %>%
  tbl_summary(
    by = gender,
    label = labels_missing_teeth,
    digits = list(all_categorical() ~ 1)
  ) %>%
  add_p() %>%
  add_overall() %>%
  modify_caption("**Table 2: Tooth Mortality Distribution across gender**")

# Summarize caries and mobile teeth
df %>%
  select(caries_incisors, caries_canines, caries_premolars, caries_molars,
         mobile_incisors, mobile_canines, mobile_premolars, mobile_molars,
         gender) %>%
  tbl_summary(
    by = gender,
    label = labels_caries_mobile,
    digits = list(all_categorical() ~ 1)
  ) %>%
  add_p() %>%
  add_overall() %>%
  modify_caption("**Table 3: Caries and Mobile Teeth Distribution across gender**")

# Summarize missing teeth
df %>%
  select(missing_incisors_caries, missing_canines_caries, missing_premolars_caries, missing_molars_caries,
         missing_incisors_perio, missing_canines_perio, missing_premolars_perio, missing_molars_perio,
         missing_incisors_fracture, missing_canines_fracture, missing_premolars_fracture, missing_molars_fracture,
         congenital_missing_incisors, congenital_missing_canines, congenital_missing_premolars, congenital_missing_molars,
         age) %>%
  tbl_summary(
    by = age,
    label = labels_missing_teeth,
    digits = list(all_categorical() ~ 1)
  ) %>%
  add_p() %>%
  add_overall() %>%
  modify_caption("**Table 2: Tooth Mortality Distribution across Age Groups**")

# Summarize caries and mobile teeth
df %>%
  select(caries_incisors, caries_canines, caries_premolars, caries_molars,
         mobile_incisors, mobile_canines, mobile_premolars, mobile_molars,
         age) %>%
  tbl_summary(
    by = age,
    label = labels_caries_mobile,
    digits = list(all_categorical() ~ 1)
  ) %>%
  add_p() %>%
  add_overall() %>%
  modify_caption("**Table 3: Caries and Mobile Teeth Distribution Age Groups**")



# Summarize missing teeth
df %>%
  select(missing_incisors_caries, missing_canines_caries, missing_premolars_caries, missing_molars_caries,
         missing_incisors_perio, missing_canines_perio, missing_premolars_perio, missing_molars_perio,
         missing_incisors_fracture, missing_canines_fracture, missing_premolars_fracture, missing_molars_fracture,
         congenital_missing_incisors, congenital_missing_canines, congenital_missing_premolars, congenital_missing_molars,
         education_level) %>%
  tbl_summary(
    by = education_level,
    label = labels_missing_teeth,
    digits = list(all_categorical() ~ 1),
    percent = "row"
  ) %>%
  add_p() %>%
  modify_caption("**Table 2: Tooth Mortality Distribution across Educational Level**")


### Charts

library(ggplot2)
library(viridis)
library(hrbrthemes)
library(tidyr)
library(dplyr)
library(stringr)



# Convert data to long format
long_df <- df %>%
  select(missing_incisors_caries, missing_canines_caries, missing_premolars_caries, missing_molars_caries,
         missing_incisors_perio, missing_canines_perio, missing_premolars_perio, missing_molars_perio,
         missing_incisors_fracture, missing_canines_fracture, missing_premolars_fracture, missing_molars_fracture,
         congenital_missing_incisors, congenital_missing_canines, congenital_missing_premolars, congenital_missing_molars) %>%
  pivot_longer(cols = everything(), 
               names_to = "variable", 
               values_to = "value") %>%
  mutate(
    reason = case_when(
      str_detect(variable, "caries") ~ "Caries",
      str_detect(variable, "perio") ~ "Periodontitis",
      str_detect(variable, "fracture") ~ "Fracture",
      str_detect(variable, "congenital") ~ "Congenital"
    ),
    tooth_type = case_when(
      str_detect(variable, "incisors") ~ "Incisors",
      str_detect(variable, "canines") ~ "Canines",
      str_detect(variable, "premolars") ~ "Premolars",
      str_detect(variable, "molars") ~ "Molars"
    )
  ) %>%
  filter(value == "Yes") %>%
  count(tooth_type, reason) %>%
  mutate(tooth_type = factor(tooth_type, levels = c("Incisors", "Canines", "Premolars", "Molars")))

# Plot the data
plot <- ggplot(long_df, aes(fill = tooth_type, y = n, x = reason)) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_viridis(discrete = TRUE, name = "Tooth") +
  ggtitle("Frequency of Tooth Mortatlity by Aetiology") +
  theme_ipsum() +
  xlab("Aetiology") +
  ylab("Frequency")

# Save the plot
ggsave("missing_teeth_plot.png", plot, width = 10, height = 6, dpi = 300)





# Convert data to long format
long_df <- df %>%
  select(missing_incisors_caries, missing_canines_caries, missing_premolars_caries, missing_molars_caries,
         missing_incisors_perio, missing_canines_perio, missing_premolars_perio, missing_molars_perio,
         missing_incisors_fracture, missing_canines_fracture, missing_premolars_fracture, missing_molars_fracture,
         congenital_missing_incisors, congenital_missing_canines, congenital_missing_premolars, congenital_missing_molars,
         age) %>%
  pivot_longer(cols = -age, 
               names_to = "variable", 
               values_to = "value") %>%
  mutate(
    reason = case_when(
      str_detect(variable, "caries") ~ "Caries",
      str_detect(variable, "perio") ~ "Periodontitis",
      str_detect(variable, "fracture") ~ "Fracture",
      str_detect(variable, "congenital") ~ "Congenital"
    ),
    tooth_type = case_when(
      str_detect(variable, "incisors") ~ "Incisors",
      str_detect(variable, "canines") ~ "Canines",
      str_detect(variable, "premolars") ~ "Premolars",
      str_detect(variable, "molars") ~ "Molars"
    )
  ) %>%
  filter(value == "Yes") %>%
  filter(reason != "Congenital") %>%
  count(tooth_type, reason, age) %>%
  mutate(tooth_type = factor(tooth_type, levels = c("Incisors", "Canines", "Premolars", "Molars")))


# Convert data to long format
long_df_age <- df %>%
  select(missing_incisors_caries, missing_canines_caries, missing_premolars_caries, missing_molars_caries,
         missing_incisors_perio, missing_canines_perio, missing_premolars_perio, missing_molars_perio,
         missing_incisors_fracture, missing_canines_fracture, missing_premolars_fracture, missing_molars_fracture,
         congenital_missing_incisors, congenital_missing_canines, congenital_missing_premolars, congenital_missing_molars,
         age) %>%
  pivot_longer(cols = -age, 
               names_to = "variable", 
               values_to = "value") %>%
  mutate(
    reason = case_when(
      str_detect(variable, "caries") ~ "Caries",
      str_detect(variable, "perio") ~ "Periodontitis",
      str_detect(variable, "fracture") ~ "Fracture",
      str_detect(variable, "congenital") ~ "Congenital"
    ),
    tooth_type = case_when(
      str_detect(variable, "incisors") ~ "Incisors",
      str_detect(variable, "canines") ~ "Canines",
      str_detect(variable, "premolars") ~ "Premolars",
      str_detect(variable, "molars") ~ "Molars"
    )
  ) %>%
  filter(value == "Yes") %>%
  filter(reason != "Congenital") %>%
  mutate(tooth_type = factor(tooth_type, levels = c("Incisors", "Canines", "Premolars", "Molars")))


grouped_ggbarstats(
  data         = long_df_age,
  x            = age,
  y            = tooth_type,
  grouping.var = reason,
  package      = "wesanderson",
  palette      = "Darjeeling2" # ,
  # ggtheme      = ggthemes::theme_tufte(base_size = 12)
)




# Plot the data with facets in one column, thicker lines, and proper labeling
plot <- ggplot(long_df, aes(x = age, y = n, color = tooth_type, group = tooth_type)) +
  geom_point() +
  geom_line(size = 1.5) +  # Thicker lines
  facet_wrap(vars(reason), ncol = 1) +  # Facet in one column
  labs(title = "Tooth Mortality and Aetilogy across age groups",
       x = "Age",
       y = "Number of Individuals",
       color = "Tooth Type") +  # Labeling the legend
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12),
        axis.text.y = element_text(colour = "grey20", size = 12),
        text = element_text(size = 16))

# Save the plot as a high-resolution PNG
ggsave("missing_teeth_plot_age.png", plot, width = 782, height = 836, units = "px", dpi = 300)


# Convert data to long format for gender
long_df_gender <- df %>%
  select(missing_incisors_caries, missing_canines_caries, missing_premolars_caries, missing_molars_caries,
         missing_incisors_perio, missing_canines_perio, missing_premolars_perio, missing_molars_perio,
         missing_incisors_fracture, missing_canines_fracture, missing_premolars_fracture, missing_molars_fracture,
         congenital_missing_incisors, congenital_missing_canines, congenital_missing_premolars, congenital_missing_molars,
         age, gender) %>%
  pivot_longer(cols = -c(age, gender), 
               names_to = "variable", 
               values_to = "value") %>%
  mutate(
    reason = case_when(
      str_detect(variable, "caries") ~ "Caries",
      str_detect(variable, "perio") ~ "Periodontitis",
      str_detect(variable, "fracture") ~ "Fracture",
      str_detect(variable, "congenital") ~ "Congenital"
    ),
    tooth_type = case_when(
      str_detect(variable, "incisors") ~ "Incisors",
      str_detect(variable, "canines") ~ "Canines",
      str_detect(variable, "premolars") ~ "Premolars",
      str_detect(variable, "molars") ~ "Molars"
    )
  ) %>%
  filter(value == "Yes") %>%
  filter(reason != "Congenital") %>%
  count(tooth_type, reason, age, gender) %>%
  mutate(tooth_type = factor(tooth_type, levels = c("Incisors", "Canines", "Premolars", "Molars")))

# Plot the data with bar charts for gender
plot_gender <- ggplot(long_df_gender, aes(x = gender, y = n, fill = tooth_type)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(vars(reason), ncol = 1) +
  labs(title = "Tooth Mortality and Etiology across Age Groups by Gender",
       x = "Age",
       y = "Number of Individuals",
       fill = "Gender") +
  theme_bw() +
  theme(axis.text.x = element_text(colour = "grey20", size = 12),
        axis.text.y = element_text(colour = "grey20", size = 12),
        text = element_text(size = 16))

# Save the plot as a high-resolution PNG
ggsave("missing_teeth_plot_gender.png", plot_gender, width = 782, height = 836, units = "px", dpi = 300)



# Convert data to long format for education
long_df_edu <- df %>%
  select(missing_incisors_caries, missing_canines_caries, missing_premolars_caries, missing_molars_caries,
         missing_incisors_perio, missing_canines_perio, missing_premolars_perio, missing_molars_perio,
         missing_incisors_fracture, missing_canines_fracture, missing_premolars_fracture, missing_molars_fracture,
         congenital_missing_incisors, congenital_missing_canines, congenital_missing_premolars, congenital_missing_molars,
         education_level) %>%
  pivot_longer(cols = -c(education_level), 
               names_to = "variable", 
               values_to = "value") %>%
  mutate(
    reason = case_when(
      str_detect(variable, "caries") ~ "Caries",
      str_detect(variable, "perio") ~ "Periodontitis",
      str_detect(variable, "fracture") ~ "Fracture",
      str_detect(variable, "congenital") ~ "Congenital"
    ),
    tooth_type = case_when(
      str_detect(variable, "incisors") ~ "Incisors",
      str_detect(variable, "canines") ~ "Canines",
      str_detect(variable, "premolars") ~ "Premolars",
      str_detect(variable, "molars") ~ "Molars"
    )
  ) %>%
  filter(value == "Yes") %>%
  filter(reason != "Congenital") %>%
  mutate(tooth_type = factor(tooth_type, levels = c("Incisors", "Canines", "Premolars", "Molars")))



grouped_ggbarstats(
  data         = long_df_edu,
  x            = education_level,
  y            = tooth_type,
  grouping.var = reason,
  package      = "wesanderson",
  palette      = "Darjeeling2",
  legend.title = "Educational Level",
  xlab = "Tooth Type",
  ylab = "Percentage of Teeth Involved"
) 



