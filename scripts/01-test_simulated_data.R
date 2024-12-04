#### Preamble ####
# Purpose: Tests the structure and validity of the simulated School Shooting dataset.
# Author: Rohan Alexander
# Date: 3 December 2024
# Contact: ruizi.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
  # - The `dplyr` package and `readr` package must be installed and loaded
  # - 00-simulate_data.R must have been run
# Any other information needed? Make sure you are in the `school_shooting_analysis` rproj


#### Workspace setup ####
# Load necessary libraries
library(dplyr)
library(readr)

# Load the dataset
data <- read_csv(here::here("data/00-simulated_data/simulated_data.csv"))

# Test 1: Logical Integrity - Ensure killed + injured == casualties
logical_integrity <- data %>%
  mutate(total_casualties = killed + injured) %>%
  filter(total_casualties != casualties)

if (nrow(logical_integrity) > 0) {
  print("Logical integrity test failed: Mismatch in casualties.")
  print(logical_integrity)
} else {
  print("Logical integrity test passed: Casualties match.")
}

# Test 2: Demographic Validation - Sum of demographic groups equals enrollment
demographic_validation <- data %>%
  mutate(demo_sum = white + black + hispanic + asian) %>%
  filter(demo_sum != enrollment)

if (nrow(demographic_validation) > 0) {
  print("Demographic validation test failed: Enrollment does not match demographic sum.")
  print(demographic_validation)
} else {
  print("Demographic validation test passed: Enrollment matches demographic sum.")
}

# Test 3: Proportions - Validate proportions are between 0 and 1
proportion_validation <- data %>%
  filter(white_proportion < 0 | white_proportion > 1 |
           black_proportion < 0 | black_proportion > 1 |
           hispanic_proportion < 0 | hispanic_proportion > 1)

if (nrow(proportion_validation) > 0) {
  print("Proportion test failed: Proportions out of bounds.")
  print(proportion_validation)
} else {
  print("Proportion test passed: All proportions are valid.")
}

# Test 4: Duplicates - Check for duplicate rows
duplicate_rows <- data %>%
  group_by_all() %>%
  filter(n() > 1)

if (nrow(duplicate_rows) > 0) {
  print("Duplicate test failed: Duplicate rows found.")
  print(duplicate_rows)
} else {
  print("Duplicate test passed: No duplicate rows.")
}

# Test 5: Locale Consistency - Check for valid values in ulocale_desc
valid_locales <- c("Town: Distant", "City: Small", "City: Large", "Other") # Example valid values
locale_validation <- data %>%
  filter(!ulocale_desc %in% valid_locales)

if (nrow(locale_validation) > 0) {
  print("Locale validation test failed: Invalid ulocale_desc values found.")
  print(locale_validation)
} else {
  print("Locale validation test passed: All ulocale_desc values are valid.")
}

