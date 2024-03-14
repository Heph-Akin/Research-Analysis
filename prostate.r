library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)


# Install and load the necessary libraries
library(summarytools)


prostate <- read.csv("/home/akindele/Downloads/prostate_to_clean.csv")
prostate_raw <- read.csv("/home/akindele/Downloads/prostate_to_clean.csv")

colnames(prostate)

# Assuming 'prostate' is your dataframe

# Q1: Have you ever heard of prostate cancer?
prostate$`Have.you.ever.heard.of.prostate.cancer.` <- ifelse(prostate$`Have.you.ever.heard.of.prostate.cancer.` == "Yes", 1, 0)

# Q3: Whom do you think prostate cancer can affect?
prostate$`Whom.do.ou.think.prostate.cancer.can.affect` <- ifelse(prostate$`Whom.do.ou.think.prostate.cancer.can.affect` != "", 1, 0)

# Q4: A man that has prostate cancer can have no symptoms.
prostate$`A.man.that.has.prostate.cancer.can.have.no.symptoms` <- ifelse(prostate$`A.man.that.has.prostate.cancer.can.have.no.symptoms` == "Yes", 1, 0)

# Symptoms of prostate cancer
symptoms <- c("Difficulty.in.passing.urine", "Urinating.frequently..especially.at.night.",
              "Incomplete.emptying.of.the.bladder", "Blood.in.the.urine", "Pain.when.urinating",
              "Pain.in.the.bone", "Loss.of.weight")

# Replace "Yes" with 1 and "No" with 0 for each symptom
prostate[, symptoms] <- ifelse(prostate[, symptoms] == "Yes", 1, 0)

# Factors increasing the risk of prostate cancer
risk_factors <- c("Runs.in.the.family..Hereditary.", "Increasing.age", "Drinking.alcohol",
                  "Smoking.Cigarettes", "Fatty.foods", "Poor.physical.activity", "Consumption.of.red.meat")

# Replace "Yes" with 1 and "No" with 0 for each risk factor
prostate[, risk_factors] <- ifelse(prostate[, risk_factors] == "Yes", 1, 0)

# Q5: Can prostate cancer be prevented?
prostate$`Can.prostate.cancer.be.prevented.` <- ifelse(prostate$`Can.prostate.cancer.be.prevented.` == "Yes", 1, 0)

# Factors reducing the risk of prostate cancer
prevention_factors <- c("Periodic.Medical.check.up", "Diet.control", "Reduced.consumption.of.red.meat",
                        "Exercise", "Avoid.smoking")

# Replace "Yes" with 1 and "No" with 0 for each prevention factor
prostate[, prevention_factors] <- ifelse(prostate[, prevention_factors] == "Yes", 1, 0)

# Q6: Do you know about prostate cancer screening?
prostate$`Do.you.know.about.prostate.cancer.screening` <- ifelse(prostate$`Do.you.know.about.prostate.cancer.screening` == "Yes", 1, 0)

# Q7: If yes, how did you know about it?
prostate$`If.yes..how.did.you.know.about.it.` <- ifelse(prostate$`If.yes..how.did.you.know.about.it.` != "", 1, 0)

# Q8: Which tests do you think can be done to screen for prostate cancer?
prostate$`Which.tests.do.you.think.can.be.done.to.screen.for.prostate.cancer.` <- 
  ifelse(prostate$`Which.tests.do.you.think.can.be.done.to.screen.for.prostate.cancer.` %in% c("", "Don't know"), 0, 1)

# Q9: Prostate cancer is treatable.
prostate$`Prostate.cancer.is.treatable.` <- ifelse(prostate$`Prostate.cancer.is.treatable.` == "Yes", 1, 0)

# Factors for treating prostate cancer
treatment_methods <- c("Surgery", "Radiotherapy..use.of.X.ray.", "Chemtherapy..use.of.drugs.",
                       "Cryosurgery..use.of.extreme.cold.", "Hormonal.therapy..use.of.hormones.",
                       "Don.t.know")

# Replace "Yes" with 1 and "No" with 0 for each treatment method
prostate[, treatment_methods] <- ifelse(prostate[, treatment_methods] != "", 1, 0)

# Q14: Do you know any method used to treat prostate cancer?
prostate$`Do.you.know.any.method.used.to.treat.prostate.cancer.` <- 
  ifelse(prostate$`Do.you.know.any.method.used.to.treat.prostate.cancer.` %in% c("", "Don't know"), 0, 1)

# Display the updated dataframe
head(prostate)

# Calculate knowledge score
prostate$knowledge_score <- rowSums(prostate[, c("Have.you.ever.heard.of.prostate.cancer.",
                                                 "Whom.do.ou.think.prostate.cancer.can.affect",
                                                 "A.man.that.has.prostate.cancer.can.have.no.symptoms",
                                                 symptoms,
                                                 risk_factors,
                                                 "Can.prostate.cancer.be.prevented.",
                                                 prevention_factors,
                                                 "Do.you.know.about.prostate.cancer.screening",
                                                 "If.yes..how.did.you.know.about.it.",
                                                 "Which.tests.do.you.think.can.be.done.to.screen.for.prostate.cancer.",
                                                 "Prostate.cancer.is.treatable.",
                                                 treatment_methods,
                                                 "Do.you.know.any.method.used.to.treat.prostate.cancer.")])

# Calculate knowledge category
prostate$knowledge_category <- ifelse((prostate$knowledge_score / length(c("Have.you.ever.heard.of.prostate.cancer.",
                                                                           "If.yes..how.did.you.hear.about.it",
                                                                           "Whom.do.ou.think.prostate.cancer.can.affect",
                                                                           "A.man.that.has.prostate.cancer.can.have.no.symptoms",
                                                                           symptoms,
                                                                           risk_factors,
                                                                           "Can.prostate.cancer.be.prevented.",
                                                                           prevention_factors,
                                                                           "Do.you.know.about.prostate.cancer.screening",
                                                                           "If.yes..how.did.you.know.about.it.",
                                                                           "Which.tests.do.you.think.can.be.done.to.screen.for.prostate.cancer.",
                                                                           "Prostate.cancer.is.treatable.",
                                                                           treatment_methods,
                                                                           "Do.you.know.any.method.used.to.treat.prostate.cancer."))) * 100 >= 50, "Good", "Bad")


# Likert-type questions
likert_questions <- c("Prostate.cancer.can.kill.a.man",
                      "Prostate.cancer.commonly.affects.white.people",
                      "I.am.too.busy.to.go.for.prostate.cancer.screening",
                      "It.is.important.to.me.to.have.a.prostate.cancer.screening.test",
                      "I.think.men.who.undergo.prostate.cancer.screening.will.have.more.problems.than.men.who.do.not.go.for.screening",
                      "I.am.afraid.that.if.I.have.a.prostate.cancer.screening.test..the.test.result.will.show.that.I.have.prostate.cancer",
                      "I.find.prostate.cancer.screening..DRE..uncomfortable.and.embarrassing",
                      "Prostate.cancer.screening.is.expensive",
                      "Prostate.cancer.can.be.sexually.transmitted",
                      "Prostate.cancer.cannot.make.me.infertile",
                      "Prostate.cancer.is.as.a.result.of.a.curse.or.taboo")

# Map Likert responses to numeric values
likert_mapping <- c("Strongly Disagree" = 1,
                    "Disagree" = 2,
                    "Neutral" = 3,
                    "Agree" = 4,
                    "Strongly Agree" = 5)

# Apply mapping to Likert questions
prostate[, likert_questions] <- lapply(prostate[, likert_questions], function(x) likert_mapping[x])

# Calculate mean score
prostate$likert_mean_score <- rowMeans(prostate[, likert_questions], na.rm = TRUE)

# Categorize attitude based on mean score
prostate$attitude_category <- ifelse(prostate$likert_mean_score < mean(prostate$likert_mean_score, na.rm = TRUE), "Negative", "Positive")



# Practice variables
practice_variables <- c("Have.you.ever.been.screened.for.prostate.cancer.",
                        "If.yes..which.screening.method.did.you.go.for..",
                        "When.was.your.most.recent.test.done.",
                        "What.was.the.reason.you.went.for.prostate.cancer.screening.",
                        "Was.the.prostate.cancer.screening.result.explained.to.you.",
                        "Would.you.go.for.a.screening.in.the.near.future.",
                        "Would.you.encourage.friends.and.relatives.to.go.for.prostate.cancer.screening.")


# Replace "Don't know" and "No" with 0
prostate$Have.you.ever.been.screened.for.prostate.cancer. <- ifelse(prostate$Have.you.ever.been.screened.for.prostate.cancer. %in% c("", "Don't know", "No"), 0, 1)
prostate$If.yes..which.screening.method.did.you.go.for.. <- ifelse(prostate$If.yes..which.screening.method.did.you.go.for.. %in% c("", "Don't know", "No"), 0, 1)
prostate$When.was.your.most.recent.test.done. <- ifelse(prostate$When.was.your.most.recent.test.done. %in% c("", "Don't know", "No"), 0, 1)
prostate$What.was.the.reason.you.went.for.prostate.cancer.screening. <- ifelse(prostate$What.was.the.reason.you.went.for.prostate.cancer.screening. %in% c("", "Don't know", "No"), 0, 1)
prostate$Was.the.prostate.cancer.screening.result.explained.to.you. <- ifelse(prostate$Was.the.prostate.cancer.screening.result.explained.to.you. %in% c("", "Don't know", "No"), 0, 1)
prostate$Would.you.go.for.a.screening.in.the.near.future. <- ifelse(prostate$Would.you.go.for.a.screening.in.the.near.future. %in% c("", "Don't know", "No"), 0, 1)
prostate$Would.you.encourage.friends.and.relatives.to.go.for.prostate.cancer.screening. <- ifelse(prostate$Would.you.encourage.friends.and.relatives.to.go.for.prostate.cancer.screening. %in% c("", "Don't know", "No"), 0, 1)


# Calculate total score for preventive practice
prostate$practice_total_score <- rowSums(prostate[, practice_variables], na.rm = TRUE)

# Categorize preventive practice
prostate$practice_category <- ifelse(prostate$practice_total_score >= 4, "Good", "Poor")

head(prostate)


# Assuming the dataframe is named 'prostate'
write.csv(prostate, "prostate_modified.csv", row.names = FALSE)




library(gtsummary)

prostate %>%
  select(Religion, Ethnic.group, Marital.Status, Level.of.Education, knowledge_category) %>%
  tbl_summary(by = knowledge_category, percent = "row") %>%
  add_overall()  %>%
  add_p()

prostate %>%
  select(knowledge_category, attitude_category) %>%
  tbl_summary(by = attitude_category, percent = "row") %>%
  add_overall() %>%
  add_p()

prostate %>%
  select(knowledge_category, practice_category) %>%
  tbl_summary(by = practice_category, percent = "row") %>%
  add_overall() %>%
  add_p()

##Visualizations
#Chart one KAP
# Load necessary libraries (if not already installed)
library(ggplot2)
library(dplyr)
library(patchwork)

# Function to create a percentage bar chart with consistent colors
create_percentage_chart <- function(data, variable, title, color_scale) {
  percentage_data <- data %>%
    group_by({{ variable }}) %>%
    summarize(count = n()) %>%
    mutate(percentage = count / sum(count) * 100) %>%
    arrange(desc(percentage))  # Arrange in descending order by percentage
  
  chart <- ggplot(percentage_data, aes(x = factor({{ variable }}, levels = unique({{ variable }})), y = percentage, fill = {{ variable }})) +
    geom_bar(stat = "identity", position = "dodge", color = "black") +
    labs(title = title, x = "") +
    scale_x_discrete(labels = function(x) gsub("_", " ", x)) +  # Remove underscores from x-axis labels
    scale_y_continuous(labels = scales::percent_format(scale = 1), limits = c(0, 100)) +  # Set y-axis limits to 0-100%
    scale_fill_manual(values = color_scale) +
    theme_minimal() +
    theme(axis.text.x = element_text(face = "bold"))  # Make x-axis text bold
  
  return(chart)
}

# Define color scales for each category
knowledge_color_scale <- c("Good" = "lightcoral", "Bad" = "firebrick")
attitude_color_scale <- c("Positive" = "lightcoral", "Negative" = "firebrick")
practice_color_scale <- c("Good" = "lightcoral", "Poor" = "firebrick")

# Create percentage charts with consistent colors
knowledge_chart <- create_percentage_chart(prostate, knowledge_category, "Knowledge Category Distribution", knowledge_color_scale)
attitude_chart <- create_percentage_chart(prostate, attitude_category, "Attitude Category Distribution", attitude_color_scale)
practice_chart <- create_percentage_chart(prostate, practice_category, "Practice Category Distribution", practice_color_scale)

# Combine charts into a single chart
combined_chart <- (knowledge_chart | attitude_chart | practice_chart) +
  plot_layout(ncol = 3, guides = 'collect')

# Print the combined chart
print(combined_chart)

#Chart 2

# Replace "Bad" with "Poor"
prostate_cleaned <- prostate %>%
  mutate(knowledge_category = ifelse(knowledge_category == "Bad", "Poor", knowledge_category))

# Calculate percentages manually
percentage_data <- prostate_cleaned %>%
  group_by(Level.of.Education, knowledge_category) %>%
  summarize(count = n()) %>%
  group_by(Level.of.Education) %>%
  mutate(percentage = count / sum(count) * 100)  # Multiply by 100 to convert to percentage

# Create a bar plot with percentages
ggplot(percentage_data, aes(x = knowledge_category, y = percentage, fill = Level.of.Education)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = scales::percent(percentage / 100)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Knowledge Category Distribution", x = "Knowledge Category", y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), name = "Percentage", limits = c(0, 100)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~Level.of.Education, scales = "free_y")

##Chart 3
# Load necessary libraries (if not already installed)
# install.packages(c("likert", "RColorBrewer", "ggplot2"))
library(likert)
library(RColorBrewer)
library(ggplot2)

# Subset the data for Likert questions
likert_data <- prostate_raw[, likert_questions]

# Convert all columns to factors
likert_data_factor <- data.frame(lapply(likert_data, as.factor))

# Define a mapping for variable names
name_mapping <- c(
  "Prostate.cancer.can.kill.a.man" = "Prostate cancer can kill a man",
  "Prostate.cancer.commonly.affects.white.people" = "Prostate cancer commonly affects white people",
  "I.am.too.busy.to.go.for.prostate.cancer.screening" = "I am too busy to go for prostate cancer screening",
  "It.is.important.to.me.to.have.a.prostate.cancer.screening.test" = "It is important to me to have a prostate cancer screening test",
  "I.think.men.who.undergo.prostate.cancer.screening.will.have.more.problems.than.men.who.do.not.go.for.screening" = "I think men who undergo prostate cancer screening will have more problems than men who do not go for screening",
  "I.am.afraid.that.if.I.have.a.prostate.cancer.screening.test..the.test.result.will.show.that.I.have.prostate.cancer" = "I am afraid that if I have a prostate cancer screening test, the test result will show that I have prostate cancer",
  "I.find.prostate.cancer.screening..DRE..uncomfortable.and.embarrassing" = "I find prostate cancer screening (DRE) uncomfortable and embarrassing",
  "Prostate.cancer.screening.is.expensive" = "Prostate cancer screening is expensive",
  "Prostate.cancer.can.be.sexually.transmitted" = "Prostate cancer can be sexually transmitted",
  "Prostate.cancer.cannot.make.me.infertile" = "Prostate cancer cannot make me infertile",
  "Prostate.cancer.is.as.a.result.of.a.curse.or.taboo" = "Prostate cancer is as a result of a curse or taboo"
)

# Rename columns using the mapping
colnames(likert_data_factor) <- name_mapping[colnames(likert_data_factor)]

# Explicitly reorder likert levels
likert_data_factor[] <- lapply(likert_data_factor, function(x) {
  levels(x) <- c("Strongly Agree", "Agree", "Neutral", "Disagree", "Strongly Disagree")
  return(x)
})

# Create a Likert plot
p <- likert(likert_data_factor)
likert_plot <- plot(likert(likert_data_factor))

# Customize the Likert options for colors
likert.options(reverse.order = TRUE, colors = brewer.pal(n = 5, "RdYlBu")[2:4])

# Customize ggplot appearance
# Customize ggplot appearance
likert_plot +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(face = "bold"),  # Use face instead of weight
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",  # Maintain legend at the bottom
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 10)  # Adjust legend text size
  )  


likert.bar.plot(likert(likert_data_factor), legend.position = "right", text.size = 4) +
  theme(text = element_text(size = rel(4)),axis.text.y = element_text(size = rel(2))) +
  theme_update(legend.text = element_text(size = rel(0.7))) +
  theme_classic() +
  theme(
    text = element_text(face = "bold"),  # Use face instead of weight
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",  # Maintain legend at the bottom
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 10)  # Adjust legend text size
  )  
                                 
                                 
                                 
                                 
#Chart 5: 
# Calculate percentages manually for "Does anyone in your family have a history of cancer"
percentage_data_cancer <- prostate %>%
  group_by(`Does.anyone.in.your.family.have.a.history.of.cancer.`) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)  # Multiply by 100 to convert to percentage

# Create a bar plot with percentages for "Does anyone in your family have a history of cancer"
chart_cancer <- ggplot(percentage_data_cancer, aes(x = `Does.anyone.in.your.family.have.a.history.of.cancer.`, y = percentage)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", fill = "skyblue") +
  geom_text(aes(label = scales::percent(percentage / 100)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Family History of Cancer Distribution", x = "Family History of Cancer", y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), name = "Percentage", limits = c(0, 100)) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Calculate percentages manually for "practice_category"
percentage_data_practice <- prostate %>%
  group_by(practice_category) %>%
  summarize(count = n()) %>%
  mutate(percentage = count / sum(count) * 100)  # Multiply by 100 to convert to percentage

# Create a bar plot with percentages for "practice_category"
chart_practice <- ggplot(percentage_data_practice, aes(x = practice_category, y = percentage)) +
  geom_bar(stat = "identity", position = "dodge", color = "black", fill = "lightgreen") +
  geom_text(aes(label = scales::percent(percentage / 100)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Practice Category Distribution", x = "Practice Category", y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), name = "Percentage", limits = c(0, 100)) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display the charts side by side
library(gridExtra)
grid.arrange(chart_cancer, chart_practice, ncol = 2)

#chart 5:
# Replace blanks with "Unemployed" in the Occupation column
prostate$Occupation <- ifelse(prostate$Occupation == "", "Unemployed", prostate$Occupation)

# Calculate percentages manually for "Occupation"
percentage_data_occupation <- prostate %>%
  group_by(Occupation, attitude_category) %>%
  summarize(count = n()) %>%
  group_by(Occupation) %>%
  mutate(percentage = count / sum(count) * 100)  # Multiply by 100 to convert to percentage

# Create a bar plot with percentages for "Occupation"
chart_occupation <- ggplot(percentage_data_occupation, aes(x = attitude_category, y = percentage, fill = attitude_category)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = scales::percent(percentage / 100)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Attitude Category Distribution by Occupation", x = "Attitude Category", y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), name = "Percentage", limits = c(0, 100)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~Occupation, scales = "free_y", ncol = 2)

# Display the chart
print(chart_occupation)


#Chart6:
# Replace "Yes" with "Positive Family History" and "No" with "Negative Family History"
prostate_cleaned <- prostate %>%
  mutate(Does.anyone.in.your.family.have.a.history.of.cancer. = 
           ifelse(Does.anyone.in.your.family.have.a.history.of.cancer. == "Yes", 
                  "Positive Family History", 
                  ifelse(Does.anyone.in.your.family.have.a.history.of.cancer. == "No", 
                         "Negative Family History", 
                         Does.anyone.in.your.family.have.a.history.of.cancer.)))

# Calculate percentages manually for "Does anyone in your family have a history of cancer" and "practice_category"
percentage_data_family_cancer <- prostate_cleaned %>% 
  group_by(Does.anyone.in.your.family.have.a.history.of.cancer., practice_category) %>%
  summarize(count = n()) %>%
  group_by(Does.anyone.in.your.family.have.a.history.of.cancer.) %>%
  mutate(percentage = count / sum(count) * 100)  # Multiply by 100 to convert to percentage

# Create a bar plot with percentages for "Does anyone in your family have a history of cancer" and "practice_category"
chart_family_cancer <- ggplot(percentage_data_family_cancer, aes(x = practice_category, y = percentage, fill = practice_category)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = scales::percent(percentage / 100)), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Practice Category Distribution by Family Cancer History", x = "Practice Category", y = "Percentage") +
  scale_y_continuous(labels = scales::percent_format(scale = 1), name = "Percentage", limits = c(0, 100)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~Does.anyone.in.your.family.have.a.history.of.cancer., scales = "free_y", ncol = 2)

# Display the chart
print(chart_family_cancer)
