#Libraries
library(dplyr)
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(tools)
library(summarytools)
library(flextable)
library(knitr)
library(kableExtra)




df <- read.csv("/home/akindele/Documents/Data_Science_Projects/R/Research Analysis/prostate_scored.csv")

df <- data.frame(lapply(df, trimws))

colnames(df)

library(dplyr)