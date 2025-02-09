library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(showtext)

# Load custom font
font_add(family = "TenorSans", regular = "TenorSans-Regular.ttf")
showtext_auto()

# Data
data <- data.frame(
  Year = 2019,
  Group = c("AIANNH_Female", "AIANNH_Male", "Not_AIANNH_Female", "Not_AIANNH_Male"),
  Pctl25 = c(7617, 9103, 10230, 16320),
  Pctl50 = c(21450, 26220, 24640, 37390),
  Pctl75 = c(37590, 47290, 42680, 64200),
  Pctl98 = c(91920, 124500, 116200, 191500)
)

# Convert to long format
data_long <- data %>%
  pivot_longer(cols = starts_with("Pctl"), names_to = "Percentile", values_to = "Income") %>%
  mutate(
    Percentile = recode(Percentile, 
                        "Pctl25" = "25th", 
                        "Pctl50" = "50th", 
                        "Pctl75" = "75th", 
                        "Pctl98" = "98th"),
    Group = case_when(
      Group == "AIANNH_Female" ~ "Native Female",
      Group == "Not_AIANNH_Female" ~ "Non-Native Female",
      Group == "AIANNH_Male" ~ "Native Male",
      Group == "Not_AIANNH_Male" ~ "Non-Native Male",
      TRUE ~ Group
    ),
    BaseIncome = ave(Income, Percentile, FUN = min),  # Find lowest income per percentile
    BubbleSize = Income - BaseIncome  # Bubble size = Difference from lowest value
  )

data_long <- data_long %>%
  mutate(
    Group = factor(Group, levels = c("Native Female", "Non-Native Female", "Native Male", "Non-Native Male"))
  )

# Bubble Chart
ggplot(data_long, aes(x = Income, y = Percentile, size = BubbleSize, color = Group)) +
  geom_point(alpha = 0.7) +  # Transparent bubbles
  scale_size(range = c(7, 20), guide = "none") +  # Remove size legend
  scale_x_continuous(labels = scales::dollar_format(scale = 1e-3, suffix = "K")) +  # Format income as 50K, 100K
  scale_color_manual(values = c(
    "Non-Native Female" = "#e28413",
    "Native Female" = "#e56b70",
    "Native Male" = "#1a939c",
    "Non-Native Male" = "#004643"
  )) +  
  theme_minimal(base_family = "TenorSans") +
  labs(
    x = "Income",
    y = "Income Percentile",
    size = "Difference from\nNative Women's Income",
    color = "Group"
  ) +
  guides(color = guide_legend(override.aes = list(size = 6))) +  # Make legend dots bigger
  theme(
    legend.position = "right",
    text = element_text(color = "#000022"),  # All text in #000022
    axis.text = element_text(color = "#000022", size = 12),
    axis.title = element_text(color = "#000022", size = 12),
    legend.text = element_text(color = "#000022",size = 12),
    legend.title = element_blank()
  )

ggsave("native-four-bubbles.png", width = 10, height = 6, dpi = 300)