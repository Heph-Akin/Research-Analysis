library(googlesheets4)
library(janitor)
library(tidyr)
library(gtsummary)
library(ggstatsplot)
library(gtsummary)
library(stringr)
library(dplyr)
library(tidyr)
library(tools)
library(ggplot2)
library(forcats)
library(remotes)
library(ggpattern)   


# Load the Google Sheet by providing the URL or the sheet name
df <- read_sheet("https://docs.google.com/spreadsheets/d/1WyAWd2VYgmmsGKMdE4sNW_4ZTRRw1oowSUIeysc9udQ/edit?usp=sharing") %>%
  clean_names()


# Prepare PDI data
pdi_data <- df %>%
  select(group, pdi_1, pdi_2, pdi_3) %>%
  pivot_longer(cols = starts_with("pdi"), names_to = "time", values_to = "value") %>%
  mutate(time = recode(time, "pdi_1" = "T1", "pdi_2" = "T2", "pdi_3" = "T3"))

# Create PDI chart
pdi_plot <- ggplot(pdi_data, aes(x = time, y = value, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", na.rm = TRUE) +
  labs(title = "Grouped Bar Chart for PDI", x = "Time Period", y = "Value") +
  theme_minimal()

# Save PDI chart
ggsave("PDI_chart.png", plot = pdi_plot, width = 8, height = 6)


# Prepare REBA data
reba_data <- df %>%
  select(group, reba_1, reba_2, reba_3) %>%
  pivot_longer(cols = starts_with("reba"), names_to = "time", values_to = "value") %>%
  mutate(time = recode(time, "reba_1" = "T1", "reba_2" = "T2", "reba_3" = "T3"))

# Create REBA chart
reba_plot <- ggplot(reba_data, aes(x = time, y = value, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", na.rm = TRUE) +
  labs(title = "Grouped Bar Chart for REBA", x = "Time Period", y = "Value") +
  theme_minimal()

# Save REBA chart
ggsave("REBA_chart.png", plot = reba_plot, width = 8, height = 6)


# Prepare SF12_PS data
sf12_ps_data <- df %>%
  select(group, sf12_1_ps, sf12_2_ps, sf12_3_ps) %>%
  pivot_longer(cols = starts_with("sf12_"), names_to = "time", values_to = "value") %>%
  filter(str_detect(time, "_ps")) %>%
  mutate(time = recode(time, "sf12_1_ps" = "T1", "sf12_2_ps" = "T2", "sf12_3_ps" = "T3"))

# Create SF12_PS chart
sf12_ps_plot <- ggplot(sf12_ps_data, aes(x = time, y = value, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", na.rm = TRUE) +
  labs(title = "Grouped Bar Chart for SF12_PS", x = "Time Period", y = "Value") +
  theme_minimal()

# Save SF12_PS chart
ggsave("SF12_PS_chart.png", plot = sf12_ps_plot, width = 8, height = 6)


# Prepare SF12_MS data
sf12_ms_data <- df %>%
  select(group, sf12_1_ms, sf12_2_ms, sf12_3_ms) %>%
  pivot_longer(cols = starts_with("sf12_"), names_to = "time", values_to = "value") %>%
  filter(str_detect(time, "_ms")) %>%
  mutate(time = recode(time, "sf12_1_ms" = "T1", "sf12_2_ms" = "T2", "sf12_3_ms" = "T3"))

# Create SF12_MS chart
sf12_ms_plot <- ggplot(sf12_ms_data, aes(x = time, y = value, fill = group)) +
  geom_bar(stat = "identity", position = "dodge", na.rm = TRUE) +
  labs(title = "Grouped Bar Chart for SF12_MS", x = "Time Period", y = "Value") +
  theme_minimal()

# Save SF12_MS chart
ggsave("SF12_MS_chart.png", plot = sf12_ms_plot, width = 8, height = 6)