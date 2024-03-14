library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(ggthemes)
library(gtsummary)
library(dplyr)


DAI <- read.csv("/home/akindele/Downloads/Ayenakin.csv")
bacteria <- read.csv("/home/akindele/Downloads/Bacteria.csv")


colnames(DAI)
colnames(bacteria)


# Assuming your dataset is named DAI
DAI$DAI.Category <- cut(DAI$DAI.Score, breaks = c(-Inf, 25, 30, 35, Inf),
                        labels = c("<=25", "26-30", "31-35", ">=36"),
                        include.lowest = TRUE)


library(dplyr)

bacteria <- bacteria %>%
  mutate(
    Visit = case_when(
      Visit == "1st visit" ~ "Baseline (T0)",
      Visit == "2nd visit" ~ "1 month (T1)",
      Visit == "3rd visit" ~ "2 month (T2)",
      TRUE ~ Visit
    )
  )

bacteria %>%
  filter(Adult.Adolescent == "Adolescent") %>%
  select(Total.Bacteria.Count, Visit) %>%
  tbl_summary(by = Visit) 

bacteria %>%
  filter(Adult.Adolescent == "Adult") %>%
  select(Total.Bacteria.Count, Visit) %>%
  tbl_summary(by = Visit) 

bacteria %>%
  select(Total.Bacteria.Count, Visit) %>%
  tbl_summary(by = Visit) 


library(ggplot2)

# Create a factor for Visit with the desired order
bacteria$Visit <- factor(bacteria$Visit, levels = c("Baseline (T0)", "1 month (T1)", "2 month (T2)"))




# Filter the data
bacteria_filtered <- bacteria %>%
  filter(Adult.Adolescent %in% c("Adolescent", "Adult"))

# Create boxplot
p <- ggplot(bacteria_filtered, aes(x = Visit, y = Total.Bacteria.Count, fill = Visit)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#F8766D", "#00BFC4", "#FF61C3"), breaks = visit_order) +
  labs(title = "Total Bacteria Count by Visit", x = "Visit", y = "Total Bacteria Count") +
  facet_wrap(~Adult.Adolescent) +
  theme_minimal()

# Calculate the summary stats for each group
summary_stats <- bacteria_filtered %>%
  group_by(Visit, Adult.Adolescent) %>%
  summarize(median = median(Total.Bacteria.Count), 
            lower_quartile = quantile(Total.Bacteria.Count, 0.25),
            upper_quartile = quantile(Total.Bacteria.Count, 0.75),
            n = n())

# Add labels to the plot
p + geom_text(data = summary_stats, aes(label = paste("n =", n, "\n", "Median =", median, "\n", "IQR =", lower_quartile, "-", upper_quartile),
                                        x = Visit, y = upper_quartile + 0.5), 
              vjust = -2, hjust = 0.5, size = 3, color = "black")

# Save the plot as a high-resolution PNG file
ggsave("boxplot_filtered.png", p, width = 10, height = 6, units = "in", dpi = 300)


# Calculate the summary stats for adults
summary_stats_adults <- bacteria_adults %>%
  group_by(Visit) %>%
  summarize(median = median(Total.Bacteria.Count), 
            lower_quartile = quantile(Total.Bacteria.Count, 0.25),
            upper_quartile = quantile(Total.Bacteria.Count, 0.75),
            n = n())

# Calculate the summary stats for adolescents
summary_stats_adolescents <- bacteria_adolescents %>%
  group_by(Visit) %>%
  summarize(median = median(Total.Bacteria.Count), 
            lower_quartile = quantile(Total.Bacteria.Count, 0.25),
            upper_quartile = quantile(Total.Bacteria.Count, 0.75),
            n = n())

# Create boxplot for adults
p_adults <- ggplot(bacteria_adults, aes(x = Visit, y = Total.Bacteria.Count, fill = Visit)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#F8766D", "#00BFC4", "#FF61C3"), breaks = visit_order) +
  labs(title = "Total Bacteria Count by Visit (Adults)", x = "Visit", y = "Total Bacteria Count") +
  theme_minimal() +
  geom_text(data = summary_stats_adults, aes(label = paste("n =", n, "\n", "Median =", median, "\n", "IQR =", lower_quartile, "-", upper_quartile),
                                             x = Visit, y = upper_quartile + 0.5), 
            vjust = -2, hjust = 0.5, size = 3, color = "black")

# Create boxplot for adolescents
p_adolescents <- ggplot(bacteria_adolescents, aes(x = Visit, y = Total.Bacteria.Count, fill = Visit)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_manual(values = c("#F8766D", "#00BFC4", "#FF61C3"), breaks = visit_order) +
  labs(title = "Total Bacteria Count by Visit (Adolescents)", x = "Visit", y = "Total Bacteria Count") +
  theme_minimal() +
  geom_text(data = summary_stats_adolescents, aes(label = paste("n =", n, "\n", "Median =", median, "\n", "IQR =", lower_quartile, "-", upper_quartile),
                                                  x = Visit, y = upper_quartile + 0.5), 
            vjust = -2, hjust = 0.5, size = 3, color = "black")

# Save the plots as high-resolution PNG files
ggsave("boxplot_adults.png", p_adults, width = 10, height = 6, units = "in", dpi = 300)
ggsave("boxplot_adolescents.png", p_adolescents, width = 10, height = 6, units = "in", dpi = 300)


DAI %>%
  distinct(Patient.Number, .keep_all = TRUE) %>%
  select(Adult.Adolescent, DAI.Score, DAI.Category) %>%
  tbl_summary(
    by = Adult.Adolescent,
    label = list(DAI.Score ~ "DAI Score"),
    digits = list(DAI.Score ~ 2),
    statistic = list(DAI.Score ~ "{mean} ± {sd}"),
    type = list(DAI.Score ~ "continuous")
  ) %>%
  add_p(
    test = DAI.Score ~ "t.test",
    test.args = DAI.Score ~ list(var.equal = TRUE)
  )



