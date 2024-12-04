#### Preamble ####
# Purpose: Tests the structure and validity of the analysis School Shooting dataset.
# Author: Rohan Alexander
# Date: 3 December 2024
# Contact: ruizi.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - The `dplyr` package and `readr` package must be installed and loaded
# Any other information needed? Make sure you are in the `school_shooting_analysis` rproj



#### Workspace setup ####
# Load necessary libraries
library(dplyr)
library(readr)

# Load the dataset
analysis_data <- read_csv(here::here("data/02-analysis_data/analysis_data.csv"))

# Test 1: Casualties Check - Ensure killed + injured equals casualties
casualties_check <- analysis_data %>%
  mutate(total_casualties = killed + injured) %>%
  filter(total_casualties != casualties)

if (nrow(casualties_check) > 0) {
  print("Casualties check failed: Killed + Injured does not match Casualties.")
  print(casualties_check)
} else {
  print("Casualties check passed: Killed + Injured matches Casualties.")
}

# Test 2: Demographic Proportions - Ensure all proportions are between 0 and 1
proportion_columns <- c("white_proportion", "black_proportion", "hispanic_proportion",
                        "asian_proportion", "american_indian_alaska_native_proportion",
                        "hawaiian_native_pacific_islander_proportion", "two_or_more_proportion")

proportion_check <- analysis_data %>%
  filter(across(all_of(proportion_columns), ~ . < 0 | . > 1))

if (nrow(proportion_check) > 0) {
  print("Proportion check failed: Some proportions are out of the valid range (0-1).")
  print(proportion_check)
} else {
  print("Proportion check passed: All proportions are within the valid range.")
}

# Test 3: Top Race Validation - Verify that `top_1_races` is consistent with demographic proportions
top_race_validation <- analysis_data %>%
  mutate(max_proportion = pmax(white_proportion, black_proportion, hispanic_proportion,
                               asian_proportion, american_indian_alaska_native_proportion,
                               hawaiian_native_pacific_islander_proportion, two_or_more_proportion),
         inferred_race = case_when(
           max_proportion == white_proportion ~ "white",
           max_proportion == black_proportion ~ "black",
           max_proportion == hispanic_proportion ~ "hispanic",
           max_proportion == asian_proportion ~ "asian",
           max_proportion == american_indian_alaska_native_proportion ~ "american_indian_alaska_native",
           max_proportion == hawaiian_native_pacific_islander_proportion ~ "hawaiian_native_pacific_islander",
           max_proportion == two_or_more_proportion ~ "two_or_more",
           TRUE ~ "unknown"
         )) %>%
  filter(top_1_races != inferred_race)

if (nrow(top_race_validation) > 0) {
  print("Top race validation failed: Inconsistent top race inference.")
  print(top_race_validation)
} else {
  print("Top race validation passed: Top race matches proportions.")
}

# Test 4: Unique Identifiers - Check if a combination of year, state, and school_type uniquely identifies rows
unique_identifier_check <- analysis_data %>%
  group_by(year, state, school_type) %>%
  filter(n() > 1)

if (nrow(unique_identifier_check) > 0) {
  print("Unique identifier check failed: Duplicate rows detected for (year, state, school_type).")
  print(unique_identifier_check)
} else {
  print("Unique identifier check passed: No duplicate rows.")
}

# Test 5: Locale Descriptions - Validate ulocale_desc values
valid_locale_desc <- c("Suburb: Large", "City: Mid-size", "City: Large", "Town: Small", "Other") # Example valid values
locale_check <- analysis_data %>%
  filter(!ulocale_desc %in% valid_locale_desc)

if (nrow(locale_check) > 0) {
  print("Locale validation failed: Invalid ulocale_desc values.")
  print(locale_check)
} else {
  print("Locale validation passed: All ulocale_desc values are valid.")
}

