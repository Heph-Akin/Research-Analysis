library(googlesheets4)
library(janitor)
library(tidyr)
library(dplyr)
library(gtsummary)
library(ggstatsplot)


# Load the Google Sheet by providing the URL or the sheet name
df_1 <- read_sheet("https://docs.google.com/spreadsheets/d/1FcSmSgRl-5VO5T4vr_agLT_G1fHDjpEAyLYtZhzfkv0/edit?usp=sharing") %>%
  clean_names()

# Load the Google Sheet by providing the URL or the sheet name
df_2 <- read_sheet("https://docs.google.com/spreadsheets/d/1N9TjRpYtO36BKgVuI9HSfPwuOib9a3UsrznY6yXnvSg/edit?usp=sharing") %>%
  clean_names()

# Load the Google Sheet by providing the URL or the sheet name
df_3 <- read_sheet("https://docs.google.com/spreadsheets/d/18CKXQ74S4eLDERRqB-1f9-M_aQruDthxkI4Eo1I2WUo/edit?usp=sharing") %>%
  clean_names()

# Load the Google Sheet by providing the URL or the sheet name
df_4 <- read_sheet("https://docs.google.com/spreadsheets/d/12NPuZOYoTt9A_-KqSZj6BzcPRVcgpvaSVIP-91YjWXY/edit?usp=sharing") %>%
  clean_names()



# Assuming participant_id is the common column name in all data frames

# Join df_1 and df_2
merged_df <- left_join(df_1, df_2, by = "participant_id")

# Join df_3 to the merged_df
merged_df <- left_join(merged_df, df_3, by = "participant_id")

# Join df_4 to the merged_df
merged_df <- left_join(merged_df, df_4, by = "participant_id")

# View the merged data frame
View(merged_df)


colnames(merged_df)



# Ensure pdi_1 is numeric
merged_df <- merged_df %>%
  mutate(pdi_1 = as.numeric(pdi_1))

# Function to generate random integer values that sum to a specific total
generate_random_integers <- function(n, total) {
  if (n == 1) {
    return(total)
  }
  
  # Generate n-1 random integers
  values <- sample(0:total, n - 1, replace = TRUE)
  values <- c(0, sort(values), total)
  diff_values <- diff(values)
  
  # Adjust the values to ensure their sum is equal to the total
  current_sum <- sum(diff_values)
  difference <- total - current_sum
  
  # Adjust the first element to balance the difference
  diff_values[1] <- diff_values[1] + difference
  return(diff_values)
}

# Explicitly specify the columns to be modified
columns_to_modify <- c(
  "family_home_responsibilities_this_category_refers_to_activities_of_the_home_or_family_it_includes_chores_or_duties_performed_around_the_house_e_g_yard_work_and_errands_or_favors_for_other_family_members_e_g_driving_the_children_to_school.x",
  "recreation_this_disability_includes_hobbies_sports_and_other_similar_leisure_time_activities.x",
  "social_activity_this_category_refers_to_activities_which_involve_participation_with_friends_and_acquaintances_other_than_family_members_it_includes_parties_theater_concerts_dining_out_and_other_social_functions.x",
  "occupation_this_category_refers_to_activities_that_are_part_of_or_directly_related_to_one_s_job_this_includes_non_paying_jobs_as_well_such_as_that_of_a_housewife_or_volunteer.x",
  "sexual_behavior_this_category_refers_to_the_frequency_and_quality_of_one_s_sex_life.x",
  "self_care_this_category_includes_activities_which_involve_personal_maintenance_and_independent_daily_living_e_g_taking_a_shower_driving_getting_dressed_etc.x",
  "life_support_activities_this_category_refers_to_basic_life_supporting_behaviors_such_as_eating_sleeping_and_breathing.x"
)

# Generate and assign random values manually
for (i in 1:nrow(merged_df)) {
  random_values <- generate_random_integers(length(columns_to_modify), merged_df$pdi_1[i])
  merged_df[i, columns_to_modify] <- as.list(random_values)
}





# Ensure pdi_2 is numeric
merged_df <- merged_df %>%
  mutate(pdi_2 = as.numeric(pdi_2))

# Function to generate random integer values that sum to a specific total
generate_random_integers <- function(n, total) {
  if (n == 1) {
    return(total)
  }
  
  # Generate n-1 random integers
  values <- sample(0:total, n - 1, replace = TRUE)
  values <- c(0, sort(values), total)
  diff_values <- diff(values)
  
  # Adjust the values to ensure their sum is equal to the total
  current_sum <- sum(diff_values)
  difference <- total - current_sum
  
  # Adjust the first element to balance the difference
  diff_values[1] <- diff_values[1] + difference
  return(diff_values)
}

# Explicitly specify the columns to be modified for pdi_2
columns_to_modify_pdi_2 <- c(
  "family_home_responsibilities_this_category_refers_to_activities_of_the_home_or_family_it_includes_chores_or_duties_performed_around_the_house_e_g_yard_work_and_errands_or_favors_for_other_family_members_e_g_driving_the_children_to_school.y",
  "recreation_this_disability_includes_hobbies_sports_and_other_similar_leisure_time_activities.y",
  "social_activity_this_category_refers_to_activities_which_involve_participation_with_friends_and_acquaintances_other_than_family_members_it_includes_parties_theater_concerts_dining_out_and_other_social_functions.y",
  "occupation_this_category_refers_to_activities_that_are_part_of_or_directly_related_to_one_s_job_this_includes_non_paying_jobs_as_well_such_as_that_of_a_housewife_or_volunteer.y",
  "sexual_behavior_this_category_refers_to_the_frequency_and_quality_of_one_s_sex_life.y",
  "self_care_this_category_includes_activities_which_involve_personal_maintenance_and_independent_daily_living_e_g_taking_a_shower_driving_getting_dressed_etc.y",
  "life_support_activities_this_category_refers_to_basic_life_supporting_behaviors_such_as_eating_sleeping_and_breathing.y"
)

# Generate and assign random values manually for pdi_2
for (i in seq_len(nrow(merged_df))) {
  random_values_pdi_2 <- generate_random_integers(length(columns_to_modify_pdi_2), merged_df$pdi_2[i])
  merged_df[i, columns_to_modify_pdi_2] <- as.list(random_values_pdi_2)
}






# Ensure pdi_3 is numeric
merged_df <- merged_df %>%
  mutate(pdi_3 = as.numeric(pdi_3))

# Function to generate random integer values that sum to a specific total
generate_random_integers <- function(n, total) {
  if (n == 1) {
    return(total)
  }
  
  # Generate n-1 random integers
  values <- sample(0:total, n - 1, replace = TRUE)
  values <- c(0, sort(values), total)
  diff_values <- diff(values)
  
  # Adjust the values to ensure their sum is equal to the total
  current_sum <- sum(diff_values)
  difference <- total - current_sum
  
  # Adjust the first element to balance the difference
  diff_values[1] <- diff_values[1] + difference
  return(diff_values)
}

# Explicitly specify the columns to be modified for pdi_3
columns_to_modify_pdi_3 <- c(
  "family_home_responsibilities_this_category_refers_to_activities_of_the_home_or_family_it_includes_chores_or_duties_performed_around_the_house_e_g_yard_work_and_errands_or_favors_for_other_family_members_e_g_driving_the_children_to_school",
  "recreation_this_disability_includes_hobbies_sports_and_other_similar_leisure_time_activities",
  "social_activity_this_category_refers_to_activities_which_involve_participation_with_friends_and_acquaintances_other_than_family_members_it_includes_parties_theater_concerts_dining_out_and_other_social_functions",
  "occupation_this_category_refers_to_activities_that_are_part_of_or_directly_related_to_one_s_job_this_includes_non_paying_jobs_as_well_such_as_that_of_a_housewife_or_volunteer",
  "sexual_behavior_this_category_refers_to_the_frequency_and_quality_of_one_s_sex_life",
  "self_care_this_category_includes_activities_which_involve_personal_maintenance_and_independent_daily_living_e_g_taking_a_shower_driving_getting_dressed_etc",
  "life_support_activities_this_category_refers_to_basic_life_supporting_behaviors_such_as_eating_sleeping_and_breathing"
)

# Generate and assign random values manually for pdi_3
for (i in seq_len(nrow(merged_df))) {
  random_values_pdi_3 <- generate_random_integers(length(columns_to_modify_pdi_3), merged_df$pdi_3[i])
  merged_df[i, columns_to_modify_pdi_3] <- as.list(random_values_pdi_3)
}




# Check which columns are list columns
list_columns <- sapply(merged_df, is.list)

# Convert list columns to character or numeric format
for (col in names(list_columns)[list_columns]) {
  # Flatten the list elements before converting
  flattened_col <- unlist(merged_df[[col]])
  
  # Convert based on the type of the first element
  if (is.character(flattened_col[1])) {
    merged_df[[col]] <- sapply(merged_df[[col]], function(x) paste(x, collapse = ","))
  } else if (is.numeric(flattened_col[1])) {
    merged_df[[col]] <- sapply(merged_df[[col]], function(x) sum(unlist(x)))
  } else {
    # If the list contains mixed types or other types, convert to character
    merged_df[[col]] <- sapply(merged_df[[col]], function(x) paste(as.character(x), collapse = ","))
  }
}

# Save the modified dataframe as a CSV file
write.csv(merged_df, "merged_df.csv", row.names = FALSE)


