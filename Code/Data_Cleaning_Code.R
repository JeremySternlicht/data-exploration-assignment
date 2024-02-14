# Part 1 - cleaning and merging the data

# Load necessary libraries
library(tidyverse)
library(rio)
library(lubridate)

# Set working directory to location of data files
setwd("~/School/OMSBA/OMSBA 5300/Data Exploration Assignment/data-exploration-assignment/Raw Data/Data_Exploration_Rawdata/Lab3_Rawdata")

# Create a vector of filenames for all Google Trends data files
google_trends_files <- list.files(pattern = "trends_up_to_.*\\.csv", full.names = TRUE)

# Use import_list() from rio to read and combine all Google Trends CSV files
google_trends_data <- import_list(google_trends_files, rbind = TRUE)

# View the combined Google Trends data
head(google_trends_data)

# Convert monthorweek to actual date and standardize index
google_trends_data_processed <- google_trends_data %>%
  mutate(date = ymd(str_sub(monthorweek, 1, 10)), # Convert string to date
         # Standardize index by school name and keyword
         index_standardized = (index - mean(index, na.rm = TRUE)) / sd(index, na.rm = TRUE)) %>%
  group_by(schname, date) %>%
  summarize(standardized_index = mean(index_standardized, na.rm = TRUE))

# Check the processed and aggregated Google Trends data
head(google_trends_data_processed)

# Read the College Scorecard data
scorecard_data <- import("Most+Recent+Cohorts+(Scorecard+Elements).csv")

# Filter Scorecard data for colleges predominantly granting bachelor's degrees
scorecard_data_filtered <- scorecard_data %>%
  filter(PREDDEG == 3)

# Extract and process median earnings data
# Change to numeric values so 'null' becomes 'NA' and can be properly filtered and calculated
scorecard_data_filtered <- scorecard_data_filtered %>%
  mutate(md_earn_wne_p10 = as.numeric(`md_earn_wne_p10-REPORTED-EARNINGS`))

# Define high and low earnings based on mean and standard deviation
scorecard_data_filtered <- scorecard_data_filtered %>%
  mutate(earnings_category = case_when(
    md_earn_wne_p10 >= earnings_mean ~ "High Earnings",
    TRUE ~ "Low Earnings"
  ))

# Calculate mean and standard deviation of median earnings
earnings_mean <- mean(scorecard_data_filtered$md_earn_wne_p10, na.rm = TRUE)
earnings_sd <- sd(scorecard_data_filtered$md_earn_wne_p10, na.rm = TRUE)

# Read the id_name_link file
id_name_link <- import("id_name_link.csv")

# Filter out duplicates in id_name_link based on school name
id_name_link_filtered <- id_name_link %>%
  group_by(schname) %>%
  mutate(n = n()) %>%
  filter(n == 1) %>%
  ungroup()

# Check the filtered linking data
head(id_name_link_filtered)

# Merge Google Trends data with the filtered linking data
merged_data <- google_trends_data_processed %>%
  inner_join(id_name_link_filtered, by = "schname")

# Merge the above with the Scorecard data using inner_join to ensure matches
final_merged_data <- merged_data %>%
  inner_join(scorecard_data_filtered, by = c("unitid" = "UNITID"))

# Select only the relevant columns of data
final_filtered_data <- final_merged_data %>%
  select(schname, date, standardized_index, unitid, opeid, md_earn_wne_p10, earnings_category)

# Check the final merged dataset ready for analysis
head(final_filtered_data)
