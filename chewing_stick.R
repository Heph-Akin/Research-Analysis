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
df <- read_sheet("https://docs.google.com/spreadsheets/d/1VuNm_7aQQ4hzSEn9iMWgRzl3K_hlLEQPOfTuzeEGnK4/edit?usp=sharing") %>%
  clean_names()

colnames(df)






df %>%
  filter(round == 1) %>%
  select(
    day_1_vscb, day_1_p_hb, day_1_tscb, day_1_vscim, day_1_p_him,
    day_1_tscim, day_1_percentage_bop, day_1_plaque_index_score,
    day_1_plaque_status, day_1_gingival_index_score, day_1_gingival_status,
    day_7_vsc, day_7_tongue_p_h, day_14_vsc, day_14_tongue_p_h,
    day_21_tongue_coating_score, day_21_vsc, day_21_tongue_p_h,
    day_21_plaque_index_score, day_21_plaque_status, day_21_gingival_index_score,
    day_21_gingival_status, day_21_percentage_bop, group
  ) %>%
  tbl_summary(
    by = group,
    label = list(
      day_1_vscb = "Day 1 VSC (before brushing)",
      day_1_p_hb = "Day 1 Tongue pH (before brushing)",
      day_1_tscb = "Day 1 Tongue Coating Score (before brushing)",
      day_1_vscim = "Day 1 VSC (immediately after brushing)",
      day_1_p_him = "Day 1 Tongue pH (immediately after brushing)",
      day_1_tscim = "Day 1 Tongue Coating Score (immediately after brushing)",
      day_1_percentage_bop = "Day 1 Percentage BOP",
      day_1_plaque_index_score = "Day 1 Plaque Index Score",
      day_1_plaque_status = "Day 1 Plaque Status",
      day_1_gingival_index_score = "Day 1 Gingival Index Score",
      day_1_gingival_status = "Day 1 Gingival Status",
      day_7_vsc = "Day 7 VSC",
      day_7_tongue_p_h = "Day 7 Tongue pH",
      day_14_vsc = "Day 14 VSC",
      day_14_tongue_p_h = "Day 14 Tongue pH",
      day_21_tongue_coating_score = "Day 21 Tongue Coating Score",
      day_21_vsc = "Day 21 VSC",
      day_21_tongue_p_h = "Day 21 Tongue pH",
      day_21_plaque_index_score = "Day 21 Plaque Index Score",
      day_21_plaque_status = "Day 21 Plaque Status",
      day_21_gingival_index_score = "Day 21 Gingival Index Score",
      day_21_gingival_status = "Day 21 Gingival Status",
      day_21_percentage_bop = "Day 21 Percentage BOP",
      group = "Group"
    ),
    type = list(
      day_1_vscb = "continuous",
      day_1_p_hb = "continuous",
      day_1_tscb = "continuous",
      day_1_vscim = "continuous",
      day_1_p_him = "continuous",
      day_1_tscim = "continuous",
      day_1_percentage_bop = "continuous",
      day_1_plaque_index_score = "continuous",
      day_1_plaque_status = "categorical",
      day_1_gingival_index_score = "continuous",
      day_1_gingival_status = "categorical",
      day_7_vsc = "continuous",
      day_7_tongue_p_h = "continuous",
      day_14_vsc = "continuous",
      day_14_tongue_p_h = "continuous",
      day_21_tongue_coating_score = "continuous",
      day_21_vsc = "continuous",
      day_21_tongue_p_h = "continuous",
      day_21_plaque_index_score = "continuous",
      day_21_plaque_status = "categorical",
      day_21_gingival_index_score = "continuous",
      day_21_gingival_status = "categorical",
      day_21_percentage_bop = "continuous"
    )
  ) %>%
  add_p() %>%
  add_overall()




df_selected <- df %>%
  select(day_1_tscb, day_1_vscim, day_1_p_him, day_1_tscim,
         day_1_percentage_bop, day_1_plaque_index_score, day_1_plaque_status,
         day_1_gingival_index_score, day_1_gingival_status, day_7_vsc,
         day_7_tongue_p_h, day_14_vsc, day_14_tongue_p_h,
         day_21_tongue_coating_score, day_21_vsc, day_21_tongue_p_h,
         day_21_plaque_index_score, day_21_plaque_status,
         day_21_gingival_index_score, day_21_gingival_status,
         day_21_percentage_bop) %>%
  mutate(across(everything(), as.character))


# Reshape the selected data frame
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  separate(variable, into = c("day_prefix", "day", "feature"), sep = "_", extra = "merge") %>%
  mutate(day = as.numeric(day)) %>%
  select(-day_prefix)

# Update the feature names
df_long <- df_long %>%
  mutate(feature = case_when(
    feature == "p_him" ~ "tongue_p_h",
    feature == "tscim" ~ "tongue_coating_score",
    feature == "vscim" ~ "vsc",
    TRUE ~ feature
  ))



# Add a unique identifier for each original row
df_long <- df_long %>%
  group_by(day, feature) %>%
  mutate(id = row_number()) %>%
  ungroup()

# Convert back to wide format while keeping the `day` column
df_wide <- df_long %>%
  pivot_wider(names_from = feature, values_from = value)

# View the reshaped wide-format data
head(df_wide)


colnames(df_wide)


df_wide %>%
  tbl_summary(by = day)



