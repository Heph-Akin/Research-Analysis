library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggthemes)

steroid <- read.csv("/home/akindele/Downloads/uche_clean.csv")

colnames(steroid)

# Assuming your data frame is named 'steroid'
library(ggplot2)

# Replace 'steroid' with the actual name of your data frame
outcome_data <- steroid$X26...What.was.the.outcome.of.the.treatment.

# Creating a data frame for plotting
outcome_df <- as.data.frame(table(outcome_data))


library(dplyr)
library(ggplot2)

# Assuming 'cleaned_outcome_df' is your data frame with the cleaned data
cleaned_outcome_df <- data.frame(Outcome = cleaned_outcome_data)

# Count occurrences of each outcome and calculate percentages
counted_outcome_df <- cleaned_outcome_df %>% 
  count(Outcome) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))

# Create a ggplot
ggplot(counted_outcome_df, aes(x = "", y = Percentage, fill = Outcome)) +
  geom_col(width = 1, position = "stack") +
  coord_flip() +
  labs(title = "Outcome Distribution",
       x = "Outcome",
       y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_text(face = "bold")) +
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 0.5)) +
  guides(fill = guide_legend(reverse = TRUE))



## CHArt 2
library(dplyr)
library(ggplot2)

# Assuming 'steroid' is your data frame
steroid_cleaned <- steroid %>%
  filter(`X19..Who.recommended.it.` != "") %>%  # Exclude rows where the variable is empty
  mutate(`X19..Who.recommended.it.` = case_when(
    `X19..Who.recommended.it.` %in% c("No", "None", "No Prescription") ~ "Self",
    `X19..Who.recommended.it.` %in% c("Aesthetician") ~ "Beautician",
    `X19..Who.recommended.it.` %in% c("Neighbors") ~ "Family member/Friend",
    `X19..Who.recommended.it.` %in% c("Skin Doctor") ~ "Previous prescription from a dermatologist",
    TRUE ~ trimws(`X19..Who.recommended.it.`)
  )) %>%
  count(`X19..Who.recommended.it.`) %>%
  mutate(Percentage = round(n / sum(n) * 100, 1))

# Create a ggplot
ggplot(steroid_cleaned, aes(x = Percentage, y = reorder(`X19..Who.recommended.it.`, Percentage), fill = `X19..Who.recommended.it.`)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Source of Prescription",
       x = "Percentage",
       y = "Source of Prescription",
       fill = "Source of Prescription") +  # Change legend title
  theme_minimal() +
  theme(legend.position = "top",
        legend.title = element_text(face = "bold", size = 14),  # Adjust legend title size and make it bold
        axis.text = element_text(size = 12, face = "bold"),  # Adjust axis text size and make it bold
        axis.title = element_text(size = 14, face = "bold"),  # Adjust axis title size and make it bold
        plot.title = element_text(size = 16, face = "bold"),  # Adjust plot title size and make it bold
        plot.subtitle = element_text(size = 14, face = "bold"),  # Adjust plot subtitle size and make it bold
        legend.text = element_text(size = 12, face = "bold")) +  # Adjust legend text size and make it bold
  geom_text(aes(label = paste0(Percentage, "%")), position = position_stack(vjust = 1.1), size = 6, fontface = "bold")  # Adjust text size, vjust, and make it bold



#Chart 3
# Assuming 'steroid' is your data frame
steroid_lollipop_data <- steroid %>%
  select(c(64:93)) %>%
  filter(across(everything(), is.numeric)) %>%  # Filter out non-numeric values
  summarise_all(sum) %>%
  pivot_longer(cols = everything(), names_to = "Symptom", values_to = "Count") %>%
  mutate(Symptom = gsub("^X25\\.{2}", "", Symptom)) %>%  # Remove "X25.." prefix
  mutate(Symptom = gsub("^X25\\.{1}", "", Symptom)) %>%  # Remove "X25." prefix
  mutate(Symptom = gsub("\\.", " ", Symptom)) %>%  # Replace dots with spaces
  mutate(Symptom = trimws(Symptom)) %>%  # Trim leading and trailing spaces
  group_by(Symptom) %>%  # Group by symptom name
  summarise(Count = sum(Count)) %>%  # Sum counts
  mutate(Percentage = (Count / sum(Count)) * 100) %>%  # Calculate percentages
  arrange(desc(Count))  # Arrange by count in descending order

# Create a horizontal lollipop chart
ggplot(steroid_lollipop_data, aes(x = Percentage, y = reorder(Symptom, Percentage))) +
  geom_segment(aes(xend = 0, yend = Symptom), color = "skyblue4", size = 1) +
  geom_point(size = 3, color = "skyblue4") +
  labs(title = "Symptoms Treated with Steroids",
       x = "Percentage",
       y = "Symptom") +
  theme_minimal() +
  theme(axis.text = element_text(size = 10),  # Adjust axis text size
        axis.title = element_text(size = 12),  # Adjust axis title size
        plot.title = element_text(size = 14))  # Adjust plot title size


#Chart 4
library(dplyr)
library(wordcloud2)
library(wordcloud)

# Select specific columns using the provided indexes
selected_columns <- steroid[, c(95:127)]

# Reshape the data for the word cloud
wordcloud_data_long <- pivot_longer(selected_columns, everything(), names_to = "word", values_to = "freq") %>%
  mutate(word = gsub("^X16\\.{2}", "", word),  # Remove "X16.." prefix
         word = gsub("^X16\\.{1}", "", word),   # Remove "X16." prefix
         word = gsub("\\.", " ", word),         # Replace dots with spaces
         word = trimws(word))                   # Trim leading and trailing spaces

# Summarize the data
wordcloud_data_summary <- wordcloud_data_long %>%
  group_by(word) %>%
  summarise(freq = sum(freq)) %>%
  mutate(percentage = (freq / sum(freq)) * 100) %>%
  arrange(desc(freq))  # Arrange by frequency in descending order


# Basic plot
wordcloud_plot <- wordcloud2(data = wordcloud_data_summary, size = 1.6)


### Better Cloud:
# Install PhantomJS
library(webshot)  
webshot::install_phantomjs()


# Assuming 'wordcloud_data_summary' is your tibble
library(wordcloud2)

# Make the word cloud
my_wordcloud <- wordcloud2(wordcloud_data_summary)

# Install webshot2
install.packages("webshot2")

# Load the necessary libraries
library(webshot2)
library(htmlwidgets)

# Assuming 'my_wordcloud' is your wordcloud2 plot
# Save the word cloud as an HTML file
htmlwidgets::saveWidget(my_wordcloud, "tmp_wordcloud.html", selfcontained = TRUE)

# Use webshot2 to capture the HTML file and save it as a high-res PNG
webshot2::webshot("tmp_wordcloud.html", "wordcloud_plot.png", delay = 5, vwidth = 1200, vheight = 800)

# Remove the temporary HTML file
unlink("tmp_wordcloud.html")



###
library(dplyr)
library(wordcloud2)

wordcloud_data_summary <- wordcloud_data_summary %>%
  mutate(word = factor(word, levels = wordcloud_data_summary$word))  # Convert 'word' to a factor with appropriate levels

# Set the resolution (dpi)
dpi <- 300

# Create a word cloud using the wordcloud package
wordcloud_plot <- wordcloud(wordcloud_data_summary$word, wordcloud_data_summary$freq, scale=c(3,0.5), min.freq=1, colors=brewer.pal(8, "Dark2"))

# Save the word cloud as a high-res PNG
png("wordcloud.png", width = 480 * dpi / 72, height = 480 * dpi / 72, res = dpi)
wordcloud(wordcloud_data_summary$word, wordcloud_data_summary$freq, scale=c(3,0.5), min.freq=1, colors=brewer.pal(8, "Dark2"))
dev.off()

###


# Install and load necessary packages
install.packages(c("webshot", "htmlwidgets"))
library(webshot)
library(htmlwidgets)

# Assuming 'wordcloud_plot' is your word cloud plot

# Save the HTML widget to a temporary file
tmp_file <- tempfile(fileext = ".html")
htmlwidgets::saveWidget(wordcloud_plot, tmp_file, selfcontained = TRUE)

# Use webshot to capture the HTML file and save it as an image
webshot(tmp_file, file = "wordcloud_plot.png", vwidth = 800, vheight = 600, delay = 5, zoom = 2)

# Remove the temporary HTML file
unlink(tmp_file)


#Chart 5:
# Assuming 'steroid' is your data frame
library(ggplot2)
library(dplyr)

# Filter out missing values
steroid_cleaned <- steroid %>%
  filter(!is.na(`X18..How.long.did.you.use.it.for.`),
         `X18..How.long.did.you.use.it.for.` != "")   # Exclude rows where the variable is empty

# Create test data
data <- steroid_cleaned %>%
  count(`X18..How.long.did.you.use.it.for.`) %>%
  mutate(fraction = n / sum(n),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n = -1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(`X18..How.long.did.you.use.it.for.`, "\n", scales::percent(fraction)))

# Make the plot
ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = `X18..How.long.did.you.use.it.for.`)) +
  geom_rect() +
  geom_text(x = 2, aes(y = labelPosition, label = label, color = `X18..How.long.did.you.use.it.for.`), size = 4) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  coord_polar(theta = "y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")


#Chart 6
# Assuming 'steroid' is your data frame
library(ggplot2)
library(dplyr)

# Filter out missing values
steroid_cleaned <- steroid %>%
  filter(`X14...Do.you.know.what.a.topical.corticosteroid.is.` != "")   # Exclude rows where the variable is missing

# Replace "I don't know" with "No"
steroid_cleaned$X14...Do.you.know.what.a.topical.corticosteroid.is.[steroid_cleaned$X14...Do.you.know.what.a.topical.corticosteroid.is. == "I don't know"] <- "No"

# Create a slimmer bar chart
slimmer_bar_chart_data <- steroid_cleaned %>%
  count(`X14...Do.you.know.what.a.topical.corticosteroid.is.`) %>%
  mutate(percentage = n / sum(n) * 100)

# Plot the slimmer bar chart
ggplot(slimmer_bar_chart_data, aes(x = factor(`X14...Do.you.know.what.a.topical.corticosteroid.is.`), y = percentage, fill = `X14...Do.you.know.what.a.topical.corticosteroid.is.`)) +
  geom_bar(stat = "identity", width = 0.6, position = "dodge") +
  labs(title = "Awareness of Topical Corticosteroids",
       x = "Awareness",
       y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "none")


# Assuming 'steroid' is your data frame
library(ggplot2)
library(dplyr)

# Filter out missing values
steroid_cleaned <- steroid %>%
  filter(!is.na(`X13..Do.you.use.any.medication.for.your.skin.`),
         `X13..Do.you.use.any.medication.for.your.skin.` != "")   # Exclude rows where the variable is empty

# Create test data
data <- steroid_cleaned %>%
  count(`X13..Do.you.use.any.medication.for.your.skin.`) %>%
  mutate(fraction = n / sum(n),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n = -1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(`X13..Do.you.use.any.medication.for.your.skin.`, "\n", scales::percent(fraction)))

# Make the plot
ggplot(data, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = `X13..Do.you.use.any.medication.for.your.skin.`)) +
  geom_rect() +
  geom_text(x = 2, aes(y = labelPosition, label = label, color = `X13..Do.you.use.any.medication.for.your.skin.`), size = 4) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  coord_polar(theta = "y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")

####################
# Assuming 'steroid' is your data frame
library(ggplot2)
library(dplyr)

# Filter out missing values for the first chart
steroid_cleaned1 <- steroid %>%
  filter(`X14...Do.you.know.what.a.topical.corticosteroid.is.` != "")   # Exclude rows where the variable is missing

# Replace "I don't know" with "No"
steroid_cleaned1$X14...Do.you.know.what.a.topical.corticosteroid.is.[steroid_cleaned1$X14...Do.you.know.what.a.topical.corticosteroid.is. == "I don't know"] <- "No"

# Create a slimmer bar chart
slimmer_bar_chart_data <- steroid_cleaned1 %>%
  count(`X14...Do.you.know.what.a.topical.corticosteroid.is.`) %>%
  mutate(percentage = n / sum(n) * 100)

# Plot the slimmer bar chart with facet_wrap
ggplot(slimmer_bar_chart_data, aes(x = factor(`X14...Do.you.know.what.a.topical.corticosteroid.is.`), y = percentage, fill = `X14...Do.you.know.what.a.topical.corticosteroid.is.`)) +
  geom_bar(stat = "identity", width = 0.6, position = "dodge") +
  labs(title = "Awareness of Topical Corticosteroids",
       x = "Response",
       y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "none") +
  facet_wrap(~ facet_variable)  # Replace facet_variable with the variable you want to facet by


# Assuming 'steroid' is your data frame
# Filter out missing values for the second chart
steroid_cleaned2 <- steroid %>%
  filter(!is.na(`X13..Do.you.use.any.medication.for.your.skin.`),
         `X13..Do.you.use.any.medication.for.your.skin.` != "")   # Exclude rows where the variable is empty

# Create test data for the donut chart
donut_chart_data <- steroid_cleaned2 %>%
  count(`X13..Do.you.use.any.medication.for.your.skin.`) %>%
  mutate(Percentage = n / sum(n) * 100,
         label = paste0(`X13..Do.you.use.any.medication.for.your.skin.`, "\n", scales::percent(Percentage)))

###
###
# Assuming 'steroid' is your data frame
library(ggplot2)
library(dplyr)

# Filter out missing values
steroid_cleaned3 <- steroid %>%
  filter(!is.na(`X28..Do.you.know.that.topical.corticosteroids.have.side.effects.`),
         `X28..Do.you.know.that.topical.corticosteroids.have.side.effects.` != "")   # Exclude rows where the variable is empty

# Replace "I don't know" with "No"
steroid_cleaned3$X28..Do.you.know.that.topical.corticosteroids.have.side.effects.[steroid_cleaned3$X28..Do.you.know.that.topical.corticosteroids.have.side.effects. == "I don't know"] <- "No"

# Create test data for the donut chart
donut_chart_data2 <- steroid_cleaned3 %>%
  count(`X28..Do.you.know.that.topical.corticosteroids.have.side.effects.`) %>%
  mutate(fraction = n / sum(n),
         ymax = cumsum(fraction),
         ymin = c(0, head(ymax, n = -1)),
         labelPosition = (ymax + ymin) / 2,
         label = paste0(`X28..Do.you.know.that.topical.corticosteroids.have.side.effects.`, "\n", scales::percent(fraction)))

# Make the donut chart
side_effects<- ggplot(donut_chart_data2, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = `X28..Do.you.know.that.topical.corticosteroids.have.side.effects.`)) +
  geom_rect() +
  geom_text(x = 2, aes(y = labelPosition, label = label, color = `X28..Do.you.know.that.topical.corticosteroids.have.side.effects.`), size = 4) +
  scale_fill_brewer(palette = "Dark2") +
  scale_color_brewer(palette = "Dark2") +
  coord_polar(theta = "y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")



steroid_2 <- read.csv("/home/akindele/Downloads/Clean_Final.csv")

colnames(steroid_2)

# Assuming 'steroid_2' is your data frame
library(ggplot2)
library(dplyr)

# Filter out missing values
steroid_2_cleaned <- steroid_2 %>%
  filter(!is.na(haveyouusedanytopicalcorticosteroidbefore),
         haveyouusedanytopicalcorticosteroidbefore != "")   # Exclude rows where the variable is empty

# Replace "I don't know" with "No"
steroid_2_cleaned$haveyouusedanytopicalcorticosteroidbefore[steroid_2_cleaned$haveyouusedanytopicalcorticosteroidbefore == "I don't know"] <- "No"

# Convert 1 to "No" and 2 to "Yes"
steroid_2_cleaned$haveyouusedanytopicalcorticosteroidbefore <- ifelse(steroid_2_cleaned$haveyouusedanytopicalcorticosteroidbefore == "1", "No", "Yes")

# Create a slimmer bar chart
slimmer_bar_chart_data <- steroid_2_cleaned %>%
  count(haveyouusedanytopicalcorticosteroidbefore) %>%
  mutate(percentage = n / sum(n) * 100)

# Plot the slimmer bar chart
ggplot(slimmer_bar_chart_data, aes(x = factor(haveyouusedanytopicalcorticosteroidbefore), y = percentage, fill = haveyouusedanytopicalcorticosteroidbefore)) +
  geom_bar(stat = "identity", width = 0.4, position = "dodge") +  # Adjust width here
  labs(title = "Previous Use of Topical Corticosteroids",
       x = "Response",
       y = "Percentage") +
  theme_minimal() +
  theme(legend.position = "none")
