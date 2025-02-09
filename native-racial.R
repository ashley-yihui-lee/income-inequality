# Load required libraries
require("readr")
require("ggplot2")
library(tidyr)
library(dplyr)
library(showtext)
library(scales)
library(stringr)

# Set working directory
setwd('~/Desktop/native-income/income-inequality')

# Read data
df <- read.csv("native-racial.csv")

# Add custom font
font_add(family = "TenorSans", regular = "TenorSans-Regular.ttf")
showtext_auto()

# Clean data
df_long <- df %>% 
  pivot_longer(cols = starts_with("X"), names_to = "percentile", values_to = "value") %>%
  mutate(percentile = str_remove(percentile, "^X"))

# Reshape data for shading
df_gap <- df_long %>%
  pivot_wider(names_from = group, values_from = value) %>%  
  rename(Native = `Native American`, NonNative = `Non-Native`) %>%  # Rename columns
  filter(!is.na(Native) & !is.na(NonNative))  # Ensure no missing data

# Define custom colors
custom_colors <- c("Non-Native" = "#004643", "Native American" = "#e56b70")

ggplot() +  
  # Shaded gap area with legend  
  geom_ribbon(data = df_gap, aes(x = year, ymin = Native, ymax = NonNative, fill = "Income Gap"),  
              alpha = 0.3) +  # Light shading  
  
  # Income trend lines  
  geom_line(data = df_long, aes(x = year, y = value, group = group, color = group), size = 1.5) +  
  
  # Facet by percentile  
  facet_wrap(~percentile, nrow = 2, scales = "free_y") +  
  
  # Custom colors for lines and shading  
  scale_color_manual(values = custom_colors, labels = c("Native American Female", "Non-Native Female")) +  # Adjust the order here
  scale_fill_manual(values = c("Income Gap" = "#fdf0e8")) +  # Add shading color to legend  
  
  # X-axis formatting  
  scale_x_continuous(breaks = c(2005, 2012, 2019), guide = guide_axis()) +  
  
  # Y-axis formatting  
  scale_y_continuous(  
    labels = scales::label_number(scale = 1e-3, suffix = "k"),  
    expand = expansion(mult = c(0.05, 0.1))  
  ) +  
  
  # Minimal theme  
  theme_minimal() +  
  theme(  
    axis.text.x = element_text(family = "TenorSans", color = "#000022", size = 12),  
    axis.text.y = element_text(family = "TenorSans", color = "#000022", size = 12),  
    axis.ticks = element_blank(),  
    axis.line = element_blank(),  
    strip.text.x = element_text(size = 20, hjust = 0.5, family = "TenorSans", color = "#000022"),  
    strip.background = element_blank(),  
    strip.placement = "outside",  
    axis.title.x = element_blank(),  
    axis.title.y = element_blank(),  
    plot.title = element_text(family = "TenorSans", color = "#000022"),  
    legend.title = element_blank(),  # Remove legend title  
    legend.text = element_text(family = "TenorSans", color = "#000022", size = 15),  
    panel.spacing = unit(1, "lines")  
  ) +  
  
  # Ensure the income gap appears in the legend  
  guides(fill = guide_legend(order = 2), color = guide_legend(order = 1))  

ggsave("native-racial-plot.png", width = 10, height = 6, dpi = 300)

