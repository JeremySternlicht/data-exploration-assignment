# Data Exploration Assignment Part 2
# Regression Models and Graphs

# Load necessary libraries
library(tidyverse)
library(rio)
library(ggplot2)

# Import the cleaned data from the "Processed Data" directory
final_filtered_data <- rio::import(file.path("../Processed Data", "final_filtered_data.csv"))

# Regression model with interaction between LOCALE and time_period
model_combined <- lm(standardized_index ~ time_period * factor(LOCALE), data = final_filtered_data)
summary(model_combined)
  
# Filter for Low Earnings colleges
low_earnings_data <- final_filtered_data %>%
  filter(earnings_category == "Low Earnings")

# Regression model with interaction between LOCALE and time_period for Low Earnings colleges
model_low_earnings_locale_interaction <- lm(standardized_index ~ time_period * factor(LOCALE), data = low_earnings_data)
summary(model_low_earnings_locale_interaction)

# Filter for High Earnings colleges
high_earnings_data <- final_filtered_data %>%
  filter(earnings_category == "High Earnings")

# Regression model with interaction between LOCALE and time_period for High Earnings colleges
model_high_earnings_locale_interaction <- lm(standardized_index ~ time_period * factor(LOCALE), data = high_earnings_data)
summary(model_high_earnings_locale_interaction)

# Graphing
# Scatter plot with regression lines for combined dataset
ggplot(final_filtered_data, aes(x = date, y = standardized_index, color = factor(LOCALE))) +
  geom_point(alpha = 0.5) + # Adjust alpha for point transparency if needed
  geom_smooth(method = "lm", se = FALSE) + # Adds a linear regression line without confidence interval
  scale_color_viridis_d() + # Optional: Use a different color scale for clarity
  labs(x = "Date", y = "Standardized Index", color = "Locale",
       title = "Search Interest Over Time by Locale") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Filter for Low Earnings colleges
low_earnings_data <- final_filtered_data %>%
  filter(earnings_category == "Low Earnings")

# Scatter plot with regression lines for Low Earnings colleges
ggplot(low_earnings_data, aes(x = date, y = standardized_index, color = factor(LOCALE))) +
  geom_point(alpha = 0.5) + # Adjust transparency with alpha
  geom_smooth(method = "lm", se = FALSE, aes(group = LOCALE)) + # Add regression line for each LOCALE
  scale_color_viridis_d() + # Use a discrete color scale
  labs(x = "Date", y = "Standardized Index", color = "Locale",
       title = "Search Interest Over Time by Locale for Low Earnings Colleges") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right")

# Filter for High Earnings colleges
high_earnings_data <- final_filtered_data %>%
  filter(earnings_category == "High Earnings")

# Scatter plot with regression lines for High Earnings colleges
ggplot(high_earnings_data, aes(x = date, y = standardized_index, color = factor(LOCALE))) +
  geom_point(alpha = 0.5) + # Adjust transparency with alpha
  geom_smooth(method = "lm", se = FALSE, aes(group = LOCALE)) + # Add regression line for each LOCALE
  scale_color_viridis_d() + # Use a discrete color scale
  labs(x = "Date", y = "Standardized Index", color = "Locale",
       title = "Search Interest Over Time by Locale for High Earnings Colleges") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "right")