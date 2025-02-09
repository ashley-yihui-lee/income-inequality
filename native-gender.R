# Load required libraries
require("readr")
require("ggplot2")
library(tidyr)
library(dplyr)
library(showtext)

# Set working directory
setwd('~/Desktop/native-income/income-inequality')

# Read data
df <- read.csv("native-gender.csv")

# Add custom font
font_add(family = "TenorSans", regular = "TenorSans-Regular.ttf")
showtext_auto()

#clean data
df_long <- df %>%
  pivot_longer(cols = starts_with("pctl"), names_to = "percentile", values_to = "value")

df_long <- df_long %>%
  rename(gender = group_var_val)

df_long <- df_long %>%
  filter(percentile != "pctl95_adj") %>%
  mutate(percentile = case_when(
    percentile == "pctl10_adj" ~ "10th",
    percentile == "pctl25_adj" ~ "25th",
    percentile == "pctl50_adj" ~ "50th",
    percentile == "pctl75_adj" ~ "75th",
    percentile == "pctl90_adj" ~ "90th",
    percentile == "pctl98_adj" ~ "98th"
  ))

# Plot
df_long %>% 
  ggplot(aes(x = year, y = value, group = gender, color = gender)) + 
  geom_line() + 
  facet_wrap(~percentile) + 
  theme_minimal()