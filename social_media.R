library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggthemes)

social_media <- read.csv("/home/akindele/Downloads/social_media.csv")

colnames(social_media)

head(social_media)

library(ggplot2)

# Assuming "Mean.Overall" represents mean intensity
selected_columns <- c("X.Facebook.", "X.Twitter.", "X.Instagram.", "X.WhatsApp.", "X.YouTube.",
                      "X.Snapchat.", "X.TikTok.", "X.LinkedIn.", "X.Telegram.", "Mean.Overall")

# Extract the relevant columns
selected_data <- social_media[, selected_columns]

# Calculate means
means <- colMeans(selected_data, na.rm = TRUE)

# Clean up social media platform names
platform_names <- gsub("X\\.", "", names(means))  # Remove "X."
platform_names <- gsub("\\.$", "", platform_names)  # Remove trailing "."

# Specify the order of the factor levels
platform_order <- c("Facebook", "Twitter", "Instagram", "WhatsApp", "YouTube",
                    "Snapchat", "TikTok", "LinkedIn", "Telegram", "Mean.Overall")

# Create a data frame for ggplot
plot_data <- data.frame(SocialMedia = factor(platform_names, levels = platform_order),
                        MeanIntensity = means)

# Assuming 'plot_data' is your dataset
# Make sure to replace 'plot_data' with your actual dataset name
# Assuming 'plot_data' is your dataset
# Make sure to replace 'plot_data' with your actual dataset name
# Assuming 'plot_data' is your dataset
# Make sure to replace 'plot_data' with your actual dataset name
# Assuming 'plot_data' is your dataset
# Make sure to replace 'plot_data' with your actual dataset name
# Assuming 'plot_data' is your dataset
# Make sure to replace 'plot_data' with your actual dataset name

# Assuming 'plot_data' is your dataset
# Make sure to replace 'plot_data' with your actual dataset name
# Assuming 'plot_data' is your dataset
# Make sure to replace 'plot_data' with your actual dataset name

# Create a bar chart using ggplot
ggplot(plot_data, aes(x = SocialMedia, y = MeanIntensity, fill = SocialMedia)) +
  geom_bar(stat = "identity", color = "black", position = "dodge") +
  labs(title = "Mean Frequency of Social Media Use",
       x = "Social Media Platforms",
       y = "Mean Frequency of Use") +
  scale_x_discrete(labels = function(x) ifelse(x == "Mean.Overall", "Mean Overall", x)) +  # Change label for Mean.Overall
  theme_minimal(base_size = 16) +  # Increase base font size
  theme(legend.text = element_text(size = 14, face = "bold"),  # Adjust legend text size and boldness
        axis.text.x = element_text(size = 14, face = "bold", angle = 45, hjust = 1),  # Adjust x-axis label size, boldness, and rotation
        axis.text.y = element_text(size = 14, face = "bold"),  # Adjust y-axis label size and boldness
        plot.title = element_text(size = 18, face = "bold")) +  # Adjust title size and boldness
  # Add annotations to y-axis (bold and spread out)
  annotate("text", x = 0, y = max(plot_data$MeanIntensity) + 3, label = "0 = Not Used", vjust = 0, hjust = 0, size = 4, fontface = "bold") +
  annotate("text", x = 1, y = max(plot_data$MeanIntensity) + 3, label = "1 = Rarely Used", vjust = 0, hjust = -0.5, size = 4, fontface = "bold") +
  annotate("text", x = 2, y = max(plot_data$MeanIntensity) + 3, label = "2 = Occasionally Used", vjust = 0, hjust = -1, size = 4, fontface = "bold") +
  annotate("text", x = 3, y = max(plot_data$MeanIntensity) + 3, label = "3 = Used Weekly", vjust = 0, hjust = -2.5, size = 4, fontface = "bold") +
  annotate("text", x = 4, y = max(plot_data$MeanIntensity) + 3, label = "4 = Used Daily", vjust = 0, hjust = -4, size = 4, fontface = "bold")

library(ggplot2)


library(ggplot2)

# Extract relevant columns
selected_data <- social_media[, c("X10..How.long.do.you.spend.on.social.media.daily.", "SNUNS_Mean_Score")]

# Rename columns for convenience
colnames(selected_data) <- c("TimeSpentDaily", "SNUNS_Mean_Score")

# Define the order of levels explicitly
time_spent_levels <- c("< 1 hour", "1-2 hours","2-3 hours", "3–4 hours", "4-5 hours", "5-6 hours", "> 6 hours")

# Create a factor with explicitly ordered levels
selected_data$TimeSpentDaily <- factor(selected_data$TimeSpentDaily, levels = time_spent_levels)

# Create a bar chart using ggplot
ggplot(selected_data, aes(x = TimeSpentDaily, y = SNUNS_Mean_Score, fill = TimeSpentDaily)) +
  geom_bar(stat = "identity", color = "black", position = "dodge") +
  labs(title = "SNUNS Mean Score across Time Spent on Social Media Daily",
       x = "Time Spent Daily",
       y = "SNUNS Mean Score") +
  theme_minimal()


# Create a bar chart using ggplot with modern styling
ggplot(selected_data, aes(x = TimeSpentDaily, y = SNUNS_Mean_Score, fill = TimeSpentDaily)) +
  geom_bar(stat = "identity", color = NA, position = "dodge", alpha = 0.8) +  # Set color = NA to remove borders
  labs(title = "SNUNS Score across Time Spent on Social Media Daily",
       x = "Time Spent Daily",
       y = "SNUNS Mean Score") +
  theme_minimal() +
  theme(legend.position = "none",           # Remove legend
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center and bold title
        axis.title.y = element_text(size = 14),  # Adjust y-axis title size
        axis.title.x = element_text(size = 14),  # Adjust x-axis title size
        axis.text = element_text(size = 12),  # Adjust axis text size
        panel.grid.major = element_blank(),   # Remove major gridlines
        panel.grid.minor = element_blank(),   # Remove minor gridlines
        panel.border = element_blank(),       # Remove panel border
        panel.background = element_blank())   # Remove panel background




# Assuming "TimeSpentDaily" is the variable representing time spent on social media daily
# Assuming "GHQ_Total" is the variable for GHQ Total Score
# Assuming "selected_data" is the data frame containing these variables
# Extract relevant columns
selected_data <- social_media[, c("X10..How.long.do.you.spend.on.social.media.daily.", "GHQ_Total")]

# Rename columns for convenience
colnames(selected_data) <- c("TimeSpentDaily", "GHQ_Total")

# Create a bar chart for GHQ_Total across Time Spent Daily using ggplot with modern styling
ggplot(selected_data, aes(x = TimeSpentDaily, y = GHQ_Total, fill = TimeSpentDaily)) +
  geom_bar(stat = "identity", color = NA, position = "dodge", alpha = 0.8) +  # Set color = NA to remove borders
  labs(title = "GHQ Total Score across Time Spent on Social Media Daily",
       x = "Time Spent Daily",
       y = "GHQ Total Score") +
  theme_minimal() +
  theme(legend.position = "none",           # Remove legend
        axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center and bold title
        axis.title.y = element_text(size = 14),  # Adjust y-axis title size
        axis.title.x = element_text(size = 14),  # Adjust x-axis title size
        axis.text = element_text(size = 12),  # Adjust axis text size
        panel.grid.major = element_blank(),   # Remove major gridlines
        panel.grid.minor = element_blank(),   # Remove minor gridlines
        panel.border = element_blank(),       # Remove panel border
        panel.background = element_blank())   # Remove panel background


# Assuming "GHQ_Total" is the variable for GHQ Total Score
# Assuming "SPAUSCIS_Score" is the variable for SPAUSCIS Score
# Assuming "selected_data" is the data frame containing these variables

# Create a scatter plot between GHQ_Total and SPAUSCIS_Score
ggplot(social_media, aes(x = GHQ_Total, y = SPAUSCIS_Score)) +
  geom_point(color = "blue", alpha = 0.7) +  # Set point color and transparency
  labs(title = "Scatter Plot between GHQ Total and SPAUSCIS Score",
       x = "GHQ Total Score",
       y = "SPAUSCIS Score") +
  theme_minimal() +
  theme(legend.position = "none",           # Remove legend
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center and bold title
        axis.title.y = element_text(size = 14),  # Adjust y-axis title size
        axis.title.x = element_text(size = 14),  # Adjust x-axis title size
        axis.text = element_text(size = 12))   # Adjust axis text size




install.packages("corrplot")
library(ggplot2)
library(corrplot)

# Select columns for correlation analysis
correlation_data <- social_media[, c("GHQ_Total", "X.Facebook.", "X.Twitter.", "X.Instagram.",
                                     "X.WhatsApp.", "X.YouTube.", "X.Snapchat.", "X.TikTok.",
                                     "X.LinkedIn.", "X.Telegram.")]

# Calculate correlation matrix
correlation_matrix <- cor(correlation_data, use = "complete.obs")

# Create a heatmap using ggplot2 and corrplot
ggplot(data.frame(x = colnames(correlation_matrix), y = rownames(correlation_matrix),
                  value = as.vector(correlation_matrix)),
       aes(x = x, y = y, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0,
                       limits = c(-1, 1), breaks = seq(-1, 1, by = 0.2)) +
  theme_minimal() +
  labs(title = "Correlation Heatmap between GHQ Total and Social Media Platforms",
       x = "Social Media Platforms",
       y = "Social Media Platforms") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),  # Rotate x-axis labels and set size
        axis.text.y = element_text(size = 10),  # Set y-axis labels size
        legend.text = element_text(size = 10),  # Set legend text size
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  # Center and bold title
        axis.title = element_text(size = 14))  # Adjust font size for axis titles


library(ggplot2)

# Assuming "SNUNS_Mean_Score" represents overall mean score
selected_columns <- c("SNUNS_Diversion", "SNUNS_Cognitive_needs",
                      "SNUNS_Affective_needs", "SNUNS_Social_integrative_needs", "SNUNS_Mean_Score")

# Extract the relevant columns
selected_data <- social_media[, selected_columns]

# Calculate means
means <- colMeans(selected_data, na.rm = TRUE)

# Clean up domain names
domain_names <- gsub("SNUNS_", "", names(means))

# Specify the explicit order of domains
domain_order <- c("Diversion", "Cognitive_needs", "Affective_needs", "Social_integrative_needs", "Mean_Score")

# Rename means based on the cleaned-up domain names
names(means) <- domain_names

# Create a data frame for ggplot
plot_data <- data.frame(Domain = factor(domain_order, levels = domain_order),
                        MeanScore = means)

# Create a bar chart using ggplot
ggplot(plot_data, aes(x = Domain, y = MeanScore, fill = Domain)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Mean Scores for SNUNS Domains",
       x = "SNUNS Domains",
       y = "Mean Score") +
  theme_minimal()



# Assuming 'social_media' is your new dataset
# Make sure to replace 'social_media' with your actual dataset name

# Load necessary libraries (if not already installed)
# install.packages(c("likert", "RColorBrewer", "ggplot2"))
library(likert)
library(RColorBrewer)
library(ggplot2)

# Subset the data for Likert questions
likert_data_social_media <- social_media[, c(
  "X.Social.media.helps.me.relieve.boredom..",
  "X.Social.media.helps.me.feel.less.lonely..",
  "X.Social.media.helps.me.escape.my.worries..",
  "X.Social.media.helps.in.my.studies.and.research..",
  "X.Social.media.helps.to.search.for.jobs..",
  "X.Social.media.helps.to.gain.knowledge..",
  "X.Social.media.allows.me.to.communicate.with.my.friends..",
  "X.Social.media.helps.me.express.my.emotions.to.others.easily..",
  "X.Social.media.allows.me.to.develop.romantic.relationships..",
  "X.I.use.social.media.to.discuss.my.issues.and.seek.help..",
  "X.Social.media.allows.me.to.meet.new.people.and.develop.relationships..",
  "X.Social.Media.allows.me.to.stay.in.touch.with.my.family..",
  "X.I.use.social.media.because.my.friends.use.it.."
)]

# Convert numeric values (1 to 5) to Likert scale labels
likert_data_social_media <- data.frame(lapply(likert_data_social_media, function(x) {
  factor(x, levels = 1:5, labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))
}))

# Define explicit mappings for variable names
variable_mappings <- c(
  "X.Social.media.helps.me.relieve.boredom.." = "Social Media helps me relieve boredom",
  "X.Social.media.helps.me.feel.less.lonely.." = "Social Media helps me feel less lonely",
  "X.Social.media.helps.me.escape.my.worries.." = "Social Media helps me escape my worries",
  "X.Social.media.helps.in.my.studies.and.research.." = "Social Media helps in my studies and research",
  "X.Social.media.helps.to.search.for.jobs.." = "Social Media helps to search for jobs",
  "X.Social.media.helps.to.gain.knowledge.." = "Social Media helps to gain knowledge",
  "X.Social.media.allows.me.to.communicate.with.my.friends.." = "Social Media allows me to communicate with my friends",
  "X.Social.media.helps.me.express.my.emotions.to.others.easily.." = "Social Media helps me express my emotions to others easily",
  "X.Social.media.allows.me.to.develop.romantic.relationships.." = "Social Media allows me to develop romantic relationships",
  "X.I.use.social.media.to.discuss.my.issues.and.seek.help.." = "I use social media to discuss my issues and seek help",
  "X.Social.media.allows.me.to.meet.new.people.and.develop.relationships.." = "Social Media allows me to meet new people and develop relationships",
  "X.Social.Media.allows.me.to.stay.in.touch.with.my.family.." = "Social Media allows me to stay in touch with my family",
  "X.I.use.social.media.because.my.friends.use.it.." = "I use social media because my friends use it"
)

# Rename columns using the explicit mapping
colnames(likert_data_social_media) <- variable_mappings[colnames(likert_data_social_media)]


# Create a Likert plot with custom colors
likert_plot_social_media <- likert.bar.plot(
  likert(likert_data_social_media),
  colors = c("#4e79a7", "#f28e2b", "#76b7b2", "#e15759", "#59a14f"),
  text.size = 4
)


# Customize ggplot appearance
likert_plot_social_media <- likert_plot_social_media +
  theme_minimal(base_size = 12) +
  theme_update(legend.text = element_text(size = rel(0.7))) +
  theme(
    text = element_text(face = "bold"),  # Use face instead of weight
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = rel(1.2)),  # Adjust y-axis text size
    legend.position = "bottom",  # Maintain legend at the bottom
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 10)  # Adjust legend text size
  ) +
  labs(title = "The Social Networking Site Usage and Needs Scale")

# Display the Likert plot
print(likert_plot_social_media)




# Assuming 'social_media' is your dataset
# Make sure to replace 'social_media' with your actual dataset name

# Load necessary libraries (if not already installed)
# install.packages(c("likert", "RColorBrewer", "ggplot2"))
library(likert)
library(RColorBrewer)
library(ggplot2)

# Subset the data for Likert questions
likert_data_mental_health <- social_media[, c(
  "GHQ_.Been.thinking.of.yourself.as.a.worthless.person..",
  "GHQ_.Felt.that.life.is.entirely.hopeless..",
  "GHQ_.Felt.that.life.isnâ..t.worth.living..",
  "GHQ_.Thought.of.the.possibility.that.you.might.end.your.life..",
  "GHQ_.Found.at.times.you.couldnâ..t.do.anything.because.you.were.too.anxious..",
  "GHQ_.Found.yourself.wishing.you.were.gone.and.away.from.it.all..",
  "GHQ_.Found.that.the.idea.of.taking.your.life.kept.coming.into.your.mind.."
)]

# Convert numeric values (1 to 4) to Likert scale labels
likert_data_mental_health <- data.frame(lapply(likert_data_mental_health, function(x) {
  factor(x, levels = 1:4, labels = c("Not At All", "Same As Usual", "Rather More Than Usual", "Much More Than Usual"))
}))

# Define explicit mappings for variable names
variable_mappings_mental_health <- c(
  "GHQ_.Been.thinking.of.yourself.as.a.worthless.person.." = "Been thinking of yourself as a worthless person",
  "GHQ_.Felt.that.life.is.entirely.hopeless.." = "Felt that life is entirely hopeless",
  "GHQ_.Felt.that.life.isnâ..t.worth.living.." = "Felt that life isn't worth living",
  "GHQ_.Thought.of.the.possibility.that.you.might.end.your.life.." = "Thought of the possibility that you might end your life",
  "GHQ_.Found.at.times.you.couldnâ..t.do.anything.because.you.were.too.anxious.." = "Found at times you couldn't do anything because you were too anxious",
  "GHQ_.Found.yourself.wishing.you.were.gone.and.away.from.it.all.." = "Found yourself wishing you were gone and away from it all",
  "GHQ_.Found.that.the.idea.of.taking.your.life.kept.coming.into.your.mind.." = "Found that the idea of taking your life kept coming into your mind"
)

# Rename columns using the explicit mapping
colnames(likert_data_mental_health) <- variable_mappings_mental_health[colnames(likert_data_mental_health)]

# Create a Likert plot
likert_plot_mental_health <- likert.bar.plot(
  likert(likert_data_mental_health),
  colors = c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2")
)

# Customize ggplot appearance
likert_plot_mental_health <- likert_plot_mental_health +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(face = "bold"),  # Use face instead of weight
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = rel(1.2)),  # Adjust y-axis text size
    legend.position = "bottom",  # Maintain legend at the bottom
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 10)  # Adjust legend text size
  ) +
  labs(title = "General Health Questionnaire")

# Display the Likert plot
print(likert_plot_mental_health)




# Load necessary libraries (if not already installed)
# install.packages(c("likert", "RColorBrewer", "ggplot2"))
library(likert)
library(RColorBrewer)
library(ggplot2)

# Subset the data for Likert questions
likert_data_social_impact <- social_media[, c(
  "SPAUSCIS_.It.is.important.to.me.that.my.posts.receive.many.likes.and.or.comments.",
  "SPAUSCIS_.It.is.important.to.me.to.have.many.followers.on.social.media.",
  "SPAUSCIS_.I.delete.posts.on.social.media.that.do.not.receive.enough.likes.and.or.comments..",
  "SPAUSCIS_.I.retouch.images.of.myself.to.look.better.before.I.post.them.on.social.media..",
  "SPAUSCIS_.Itâ..s.easier.to.be.myself.on.social.media..",
  "SPAUSCIS_.The.response.I.get.for.what.I.post..images.status.updates.stories..impacts.how.I.feel.",
  "SPAUSCIS_.What.others.post.on.social.media..images.status.updates.stories..makes.me.feel.less.content.with.myself.and.my.life..",
  "SPAUSCIS_.What.others.post.on.social.media..images.status.updates.stories..makes.desire.a.flamboyant.lifestyle..",
  "SPAUSCIS_.What.others.post.on.social.media..images.status.updates.stories..Puts.pressure.on.me.to.engage.in.acts.you.wouldn.t.usually.engage.in.such.as.drinking..smoking..use.of.illicit.substances..",
  "SPAUSCIS_.I.have.been.bullied.about.my.looks..economic.status...verbal.abuse.or.emotional.abuse..",
  "SPAUSCIS_.I.have.bullied.others.about.their.looks..economic.status...verbal.abuse.or.emotional.abuse..",
  "SPAUSCIS_.I.have.been.put.in.me.harmâ..s.way...Self.harm.or.external.harm..",
  "SPAUSCIS_.My.academic.performance.or.performance.at.work.has.improved.positively..",
  "SPAUSCIS_.My.quality.of.life.has.improved...financially..emotionally..physically..spiritually.."
)]

# Convert numeric values (1 to 4) to Likert scale labels
likert_data_social_impact <- data.frame(lapply(likert_data_social_impact, function(x) {
  factor(x, levels = 1:4, labels = c("Not at All", "Rarely", "Sometimes", "Often"))
}))

# Define explicit mappings for variable names
variable_mappings_social_impact <- c(
  "SPAUSCIS_.It.is.important.to.me.that.my.posts.receive.many.likes.and.or.comments." = "It is important to me that my posts receive many likes and/or comments",
  "SPAUSCIS_.It.is.important.to.me.to.have.many.followers.on.social.media." = "It is important to me to have many followers on social media",
  "SPAUSCIS_.I.delete.posts.on.social.media.that.do.not.receive.enough.likes.and.or.comments.." = "I delete posts on social media that do not receive enough likes and/or comments",
  "SPAUSCIS_.I.retouch.images.of.myself.to.look.better.before.I.post.them.on.social.media.." = "I retouch images of myself before I post them on social media",
  "SPAUSCIS_.Itâ..s.easier.to.be.myself.on.social.media.." = "It's easier to be myself on social media",
  "SPAUSCIS_.The.response.I.get.for.what.I.post..images.status.updates.stories..impacts.how.I.feel." = "The response I get for what I post impacts how I feel",
  "SPAUSCIS_.What.others.post.on.social.media..images.status.updates.stories..makes.me.feel.less.content.with.myself.and.my.life.." = "Social media makes me feel less content with myself and my life",
  "SPAUSCIS_.What.others.post.on.social.media..images.status.updates.stories..makes.desire.a.flamboyant.lifestyle.." = "Social media makes desire a flamboyant lifestyle",
  "SPAUSCIS_.What.others.post.on.social.media..images.status.updates.stories..Puts.pressure.on.me.to.engage.in.acts.you.wouldn.t.usually.engage.in.such.as.drinking..smoking..use.of.illicit.substances.." = "Social media pushes me to engage unusual acts, such as drinking, smoking etc",
  "SPAUSCIS_.I.have.been.bullied.about.my.looks..economic.status...verbal.abuse.or.emotional.abuse.." = "I have been bullied about my looks, economic status",
  "SPAUSCIS_.I.have.bullied.others.about.their.looks..economic.status...verbal.abuse.or.emotional.abuse.." = "I have bullied others about their looks, economic status",
  "SPAUSCIS_.I.have.been.put.in.me.harmâ..s.way...Self.harm.or.external.harm.." = "I have been put in harm's way",
  "SPAUSCIS_.My.academic.performance.or.performance.at.work.has.improved.positively.." = "My academic performance or performance at work has improved positively",
  "SPAUSCIS_.My.quality.of.life.has.improved...financially..emotionally..physically..spiritually.." = "My quality of life has improved"
)

# Rename columns using the explicit mapping
colnames(likert_data_social_impact) <- variable_mappings_social_impact[colnames(likert_data_social_impact)]

# Create a Likert plot
likert_plot_social_impact <- likert.bar.plot(
  likert(likert_data_social_impact),
  colors = c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2")
)

# Customize ggplot appearance
likert_plot_social_impact <- likert_plot_social_impact +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(face = "bold"),  # Use face instead of weight
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = rel(1.2)),  # Adjust y-axis text size
    legend.position = "bottom",  # Maintain legend at the bottom
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 10)  # Adjust legend text size
  ) +
  labs(title = "Self-Presentation and Upward Social Comparison Inclination Scale")

# Display the Likert plot
print(likert_plot_social_impact)



# Assuming 'social_media' is your dataset
# Make sure to replace 'social_media' with your actual dataset name

# Load necessary libraries (if not already installed)
# install.packages(c("likert", "RColorBrewer", "ggplot2"))
library(likert)
library(RColorBrewer)
library(ggplot2)

# Subset the data for Likert questions
likert_data_social_media_use <- social_media[, c(
  "X.I.spend.time.thinking.about.social.media.or.planning.how.to.use.it..",
  "X.My.desire.to.increase.my.use.of.social.media.is.growing..",
  "X.I.use.social.media.to.forget.about.my.personal.problems..",
  "X.I.have.tried.to.reduce.my.use.of.social.media..",
  "X.I.become.restless.or.troubled.if.I.donâ..t.use.social.media..",
  "X.My.prolonged.social.media.use.has.a.negative.impact.on.my.job.academics.."
)]

# Convert numeric values (1 to 4) to Likert scale labels
likert_data_social_media_use <- data.frame(lapply(likert_data_social_media_use, function(x) {
  factor(x, levels = 1:4, labels = c("Not at All", "Rarely", "Sometimes", "Often"))
}))

# Define explicit mappings for variable names
variable_mappings_social_media_use <- c(
  "X.I.spend.time.thinking.about.social.media.or.planning.how.to.use.it.." = "I spend time thinking about social media or planning how to use it",
  "X.My.desire.to.increase.my.use.of.social.media.is.growing.." = "My desire to increase my use of social media is growing",
  "X.I.use.social.media.to.forget.about.my.personal.problems.." = "I use social media to forget about my personal problems",
  "X.I.have.tried.to.reduce.my.use.of.social.media.." = "I have tried to reduce my use of social media",
  "X.I.become.restless.or.troubled.if.I.donâ..t.use.social.media.." = "I become restless or troubled if I don't use social media",
  "X.My.prolonged.social.media.use.has.a.negative.impact.on.my.job.academics.." = "My prolonged social media use has a negative impact on my job/academics"
)

# Rename columns using the explicit mapping
colnames(likert_data_social_media_use) <- variable_mappings_social_media_use[colnames(likert_data_social_media_use)]

# Create a Likert plot
likert_plot_social_media_use <- likert.bar.plot(
  likert(likert_data_social_media_use),
  colors = c("#4e79a7", "#f28e2b", "#e15759", "#76b7b2")
)

# Customize ggplot appearance
likert_plot_social_media_use <- likert_plot_social_media_use +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(face = "bold"),  # Use face instead of weight
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = rel(1.2)),  # Adjust y-axis text size
    legend.position = "bottom",  # Maintain legend at the bottom
    legend.title = element_blank(),  # Remove legend title
    legend.text = element_text(size = 10)  # Adjust legend text size
  ) +
  labs(title = "Bergen Social Media Addiction Scale")

# Display the Likert plot
print(likert_plot_social_media_use)
