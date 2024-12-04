#### Preamble ####
# Purpose: Cleans the raw plane data recorded, simulate a synthetic data for non shooting data
# Author: Ruizi Liu
# Date: 2 December 2024
# Contact: ruizi.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: Run the file 02-download_data.R
# Any other information needed? None

#### Workspace setup ####
library(dplyr)
library(tidyr)
library(maps)
library(ggplot2)
library(scales)

#### Load the dataset ####
raw_data <- read.csv(here::here("data/01-raw_data/raw_data.csv"))


#### Define needed functions ####
latlong_to_state <- function(lat, long) {
  # Ensure the `maps` package is loaded
  if (!requireNamespace("maps", quietly = TRUE)) {
    stop("The 'maps' package is required but not installed. Please install it with install.packages('maps').")
  }
  
  points <- data.frame(x = long, y = lat)
  
  # Use `map.where` to find the state corresponding to each lat/long pair
  state_names <- apply(points, 1, function(p) {
    region <- maps::map.where(database = "state", x = p[1], y = p[2])
    if (is.na(region)) {
      return(NA)  # If no state is found, return NA
    }
    region <- gsub(":.*", "", region)  # Remove any extra text after a colon
    region <- tools::toTitleCase(tolower(region))  # Convert to title case for consistency
    return(region)
  })
  
  return(state_names)
}

clean_numeric <- function(column) {
  as.numeric(gsub("[^0-9.]", "", as.character(column)))
}

# List of standard state names and abbreviations
state_abbreviations <- data.frame(
  state_standard = c("Alabama", "Alaska", "Arizona", "Arkansas", "California", "Colorado", 
                     "Connecticut", "Delaware", "Florida", "Georgia", "Hawaii", "Idaho", 
                     "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", 
                     "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", 
                     "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", 
                     "New Hampshire", "New Jersey", "New Mexico", "New York", 
                     "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", 
                     "Pennsylvania", "Rhode Island", "South Carolina", "South Dakota", 
                     "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", 
                     "West Virginia", "Wisconsin", "Wyoming", "District of Columbia"),
  abbreviation = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA", 
                   "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", 
                   "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", 
                   "NM", "NY", "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", 
                   "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY", "DC")
)



#### Clean raw data ####

# Clean and standardize raw data
cleaned_data <- raw_data |> 
  mutate(date = as.Date(date, format = "%m/%d/%Y")) |> 
  mutate(
    white = ifelse(is.na(clean_numeric(white)), 0, clean_numeric(white)),
    black = ifelse(is.na(clean_numeric(black)), 0, clean_numeric(black)),
    hispanic = ifelse(is.na(clean_numeric(hispanic)), 0, clean_numeric(hispanic)),
    asian = ifelse(is.na(clean_numeric(asian)), 0, clean_numeric(asian)),
    american_indian_alaska_native = ifelse(is.na(clean_numeric(american_indian_alaska_native)), 0, clean_numeric(american_indian_alaska_native)),
    hawaiian_native_pacific_islander = ifelse(is.na(clean_numeric(hawaiian_native_pacific_islander)), 0, clean_numeric(hawaiian_native_pacific_islander)),
    two_or_more = ifelse(is.na(clean_numeric(two_or_more)), 0, clean_numeric(two_or_more)),
    enrollment = ifelse(is.na(clean_numeric(enrollment)), 0, clean_numeric(enrollment))
  ) |> 
  mutate(enrollment = ifelse(enrollment == 0, NA, enrollment)) |> 
  mutate(
    white_proportion = ifelse(is.na(enrollment), 0, white / enrollment),
    black_proportion = ifelse(is.na(enrollment), 0, black / enrollment),
    hispanic_proportion = ifelse(is.na(enrollment), 0, hispanic / enrollment),
    asian_proportion = ifelse(is.na(enrollment), 0, asian / enrollment),
    american_indian_alaska_native_proportion = ifelse(is.na(enrollment), 0, american_indian_alaska_native / enrollment),
    hawaiian_native_pacific_islander_proportion = ifelse(is.na(enrollment), 0, hawaiian_native_pacific_islander / enrollment),
    two_or_more_proportion = ifelse(is.na(enrollment), 0, two_or_more / enrollment)
  ) |> 
  mutate(
    white_proportion = sprintf("%.2f%%", white_proportion * 100),
    black_proportion = sprintf("%.2f%%", black_proportion * 100),
    hispanic_proportion = sprintf("%.2f%%", hispanic_proportion * 100),
    asian_proportion = sprintf("%.2f%%", asian_proportion * 100),
    american_indian_alaska_native_proportion = sprintf("%.2f%%", american_indian_alaska_native_proportion * 100),
    hawaiian_native_pacific_islander_proportion = sprintf("%.2f%%", hawaiian_native_pacific_islander_proportion * 100),
    two_or_more_proportion = sprintf("%.2f%%", two_or_more_proportion * 100)
  ) |> 
  left_join(state_abbreviations, by = c("state" = "state_standard")) |> 
  mutate(state = ifelse(is.na(abbreviation), state, abbreviation)) |> 
  select(-abbreviation) |> 
  mutate(lat = as.numeric(lat), long = as.numeric(long)) |> 
  filter(!is.na(lat) & !is.na(long)) |> 
  mutate(
    school_shooting = 1,  # Shooting occurred
    data_source = "raw"
  )

# Replace NAs in `staffing` with the average of the column
cleaned_data$staffing[is.na(cleaned_data$staffing)] <- mean(cleaned_data$staffing, na.rm = TRUE)

# Replace NAs in `lunch` with the average of the column
cleaned_data$lunch[is.na(cleaned_data$lunch)] <- mean(cleaned_data$lunch, na.rm = TRUE)


#### Simulate non-shooting data ####
non_shooting_data <- cleaned_data
non_shooting_data$data_source <- "synthetic"
non_shooting_data$school_shooting <- 0  # Non-shooting data

set.seed(123)
non_shooting_data$lat <- runif(nrow(non_shooting_data), 24.5, 48.5)
non_shooting_data$long <- runif(nrow(non_shooting_data), -124.5, -67.0)

#### Function to Map lat/long to States ####
latlong_to_state <- function(lat, long) {
  points <- data.frame(x = long, y = lat)
  state_names <- apply(points, 1, function(p) {
    region <- maps::map.where(database = "state", x = p[1], y = p[2])
    if (is.na(region)) {
      return(NA)  # If no state is found, return NA
    }
    region <- gsub(":.*", "", region)  # Remove any extra text after colon
    region <- tools::toTitleCase(tolower(region))  # Normalize to title case
    return(region)
  })
  return(state_names)
}

non_shooting_data$state <- latlong_to_state(non_shooting_data$lat, non_shooting_data$long)

non_shooting_data <- non_shooting_data %>%
  filter(!is.na(state)) %>%
  left_join(state_abbreviations, by = c("state" = "state_standard")) %>%
  mutate(state = ifelse(is.na(abbreviation), state, abbreviation)) %>%
  select(-abbreviation)

set.seed(456)
non_shooting_data$year <- sample(2000:2023, nrow(non_shooting_data), replace = TRUE)





# Simulate valid `ulocale` values
set.seed(42)  # For reproducibility
n <- nrow(non_shooting_data)  # Number of rows in your dataset
valid_ulocale <- c(11, 12, 13, 21, 22, 23, 31, 32, 33, 41, 42, 43)  # Valid ulocale values
non_shooting_data$ulocale <- sample(valid_ulocale, size = n, replace = TRUE)  # Generate valid ulocale codes

# Map `ulocale` to descriptive categories
ulocale_map <- c(
  "11" = "City: Large",
  "12" = "City: Mid-size",
  "13" = "City: Small",
  "21" = "Suburb: Large",
  "22" = "Suburb: Mid-size",
  "23" = "Suburb: Small",
  "31" = "Town: Fringe",
  "32" = "Town: Distant",
  "33" = "Town: Remote",
  "41" = "Rural: Fringe",
  "42" = "Rural: Distant",
  "43" = "Rural: Remote"
)






# Extract min and max for `staffing` and `lunch` from the raw dataset
staffing_min <- min(cleaned_data$staffing, na.rm = TRUE)
staffing_max <- max(cleaned_data$staffing, na.rm = TRUE)
lunch_min <- min(cleaned_data$lunch, na.rm = TRUE)
lunch_max <- max(cleaned_data$lunch, na.rm = TRUE)

# Generate synthetic `staffing` within the observed range
set.seed(42)  # For reproducibility
data$staffing <- round(runif(nrow(data), min = staffing_min, max = staffing_max), 1)

# Generate synthetic `lunch` within the observed range
data$lunch <- round(runif(nrow(data), min = lunch_min, max = lunch_max), 1)




#### Fix Imbalance via Upsampling ####
set.seed(123)
non_shooting_data_upsampled <- non_shooting_data %>%
  slice_sample(n = nrow(cleaned_data), replace = TRUE)

#### Combine Data ####
balanced_data <- bind_rows(cleaned_data, non_shooting_data_upsampled)

# Create a new column with mapped categories
balanced_data <- balanced_data %>%
  mutate(ulocale_desc = ulocale_map[as.character(ulocale)])

#### Remove Unnecessary Columns ####
columns_to_remove <- c(
  "uid", "nces_school_id", "school_name", "nces_district_id", "district_name",
  "date", "school_year", "time", "day_of_week", "city", "shooting_type",
  "age_shooter1", "gender_shooter1", "race_ethnicity_shooter1",
  "shooter_relationship1", "shooter_deceased1", "deceased_notes1",
  "age_shooter2", "gender_shooter2", "race_ethnicity_shooter2",
  "shooter_relationship2", "shooter_deceased2", "deceased_notes2",
  "weapon", "weapon_source", "county", "state_fips", "county_fips", 
  "low_grade", "high_grade", "resource_officer"
)

balanced_data <- balanced_data %>%
  select(-all_of(columns_to_remove))

balanced_data <- balanced_data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# Convert race proportions from percentages to numeric values
proportion_columns <- grep("proportion", names(data), value = TRUE)

balanced_data[proportion_columns] <- balanced_data[proportion_columns] %>%
  lapply(function(x) as.numeric(gsub("%", "", x)) / 100) %>%
  as.data.frame()

# Step 1: Ensure all proportion columns are numeric by removing '%' and converting
race_columns <- grep("_proportion$", colnames(balanced_data), value = TRUE)
balanced_data[race_columns] <- lapply(balanced_data[race_columns], function(col) {
  as.numeric(gsub("%", "", col))  # Remove % and convert to numeric
})

# Step 2: Function to find the top 1 races for each row (without "_proportion" in names)
top_1_races <- function(row) {
  # Sort the proportions in descending order
  sorted <- sort(row, decreasing = TRUE)
  # Get the names of the top 1 proportions
  top_1 <- names(sorted)[1]
  # Remove the "_proportion" suffix from the names
  top_1_cleaned <- gsub("_proportion", "", top_1)
  # Combine the names into a single string
  paste(top_1_cleaned, collapse = ", ")
}

# Step 3: Apply the function row-wise and create a new column
balanced_data$top_1_races <- apply(balanced_data[race_columns], 1, top_1_races)


#### Save Updated balanced_data ####
write.csv(balanced_data, here::here("data/02-analysis_data/analysis_data.csv"), row.names = FALSE)


