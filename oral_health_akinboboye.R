library(googlesheets4)
library(janitor)
library(tidyr)
library(gtsummary)
library(ggstatsplot)
library(gt)
library(knitr)
library(flextable)
library(ggplot2)
library(dplyr)
library(forcats)
library(tibble)
library(purrr)
library(tools)
library(viridis)
library(hrbrthemes)
library(stringr)


# Load the Google Sheet by providing the URL or the sheet name
df <- read_sheet("https://docs.google.com/spreadsheets/d/1pxXvm8f21RFUiYcVMaOSrqwS-bQ4CaAea6rmu_FIQdg/edit?usp=sharing") %>%
  clean_names()

colnames(df)