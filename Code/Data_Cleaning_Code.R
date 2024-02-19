# Data Exploration Assignment Part 1
# Cleaning and Merging the Data

# Load necessary libraries
library(tidyverse)
library(rio)
library(lubridate)

# Define the path to the folder containing the raw data files
data_path <- "../Raw Data/Data_Exploration_Rawdata/Lab3_Rawdata"

# Create a vector of filenames for all Google Trends data files
google_trends_files <- list.files(path = "../Raw Data/Data_Exploration_Rawdata/Lab3_Rawdata", pattern = "trends_up_to_.*\\.csv", full.names = TRUE)

# Use import_list() from rio to read and combine all Google Trends CSV files
google_trends_data <- import_list(google_trends_files, rbind = TRUE)

# View the combined Google Trends data
head(google_trends_data)

# Convert monthorweek to actual date
google_trends_data <- google_trends_data %>%
  mutate(date = ymd(str_sub(monthorweek, 1, 10))) # Convert string to date

# Standardize index by school name and keyword
google_trends_data_processed <- google_trends_data %>%
  mutate(date = ymd(str_sub(monthorweek, 1, 10))) %>%
  group_by(schname, keyword) %>%
  mutate(
    index_standardized = (index - mean(index, na.rm = TRUE)) / sd(index, na.rm = TRUE)
  ) %>%
  ungroup()

# Aggregate the standardized index to the school-week level
google_trends_data_aggregated <- google_trends_data_processed %>%
  group_by(schname, date) %>%
  summarize(
    standardized_index = mean(index_standardized, na.rm = TRUE),
    .groups = 'drop'
  )

# Create binary variable to represent time period before (0) or after (1) Scorecard data release in 2015
google_trends_data_aggregated <- google_trends_data_aggregated %>%
  mutate(time_period = if_else(date >= as.Date("2015-09-01"), 1, 0))

# Check the processed and aggregated Google Trends data
head(google_trends_data_aggregated)

# Read the College Scorecard data using the relative path
scorecard_data <- import(file.path(data_path, "Most+Recent+Cohorts+(Scorecard+Elements).csv"))

# Filter Scorecard data for colleges predominantly granting bachelor's degrees
scorecard_data_filtered <- scorecard_data %>%
  filter(PREDDEG == 3)

# Extract and process median earnings data
# Change to numeric values so 'null' becomes 'NA' and can be properly filtered and calculated
scorecard_data_filtered <- scorecard_data_filtered %>%
  mutate(md_earn_wne_p10 = as.numeric(`md_earn_wne_p10-REPORTED-EARNINGS`))

# Calculate mean and standard deviation of median earnings
earnings_mean <- mean(scorecard_data_filtered$md_earn_wne_p10, na.rm = TRUE)
earnings_sd <- sd(scorecard_data_filtered$md_earn_wne_p10, na.rm = TRUE)

# Define high and low earnings based on mean and standard deviation
scorecard_data_filtered <- scorecard_data_filtered %>%
  mutate(earnings_category = case_when(
    md_earn_wne_p10 >= earnings_mean ~ "High Earnings",
    TRUE ~ "Low Earnings"
  ))

# Read the id_name_link file using the relative path
id_name_link <- import(file.path(data_path, "id_name_link.csv"))

# Filter out duplicates in id_name_link based on school name
id_name_link_filtered <- id_name_link %>%
  group_by(schname) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  ungroup()

# Check the filtered linking data
head(id_name_link_filtered)

# Merge Google Trends data with the filtered linking data
merged_data <- google_trends_data_aggregated %>%
  inner_join(id_name_link_filtered, by = "schname")

# Merge the above with the Scorecard data using inner_join to ensure matches
final_merged_data <- merged_data %>%
  inner_join(scorecard_data_filtered, by = c("unitid" = "UNITID"))

# Select only the relevant columns of data
final_filtered_data <- final_merged_data %>%
  select(schname, date, standardized_index, unitid, opeid, md_earn_wne_p10, earnings_category, time_period, LOCALE)

# Check the final merged dataset ready for analysis
head(final_filtered_data)

# Export the final_filtered_data dataframe to a CSV file in the "Processed Data" directory
rio::export(final_filtered_data, file.path("../Processed Data", "final_filtered_data.csv"))