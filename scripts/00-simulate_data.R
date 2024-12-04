#### Preamble ####
# Purpose: Simulates a dataset of School Shooting
# Author: Ruizi Liu
# Date: 3 December 2024
# Contact: ruizi.liur@mail.utoronto.ca
# License: MIT
# Pre-requisites: The `dplyr` package must be installed
# Any other information needed? Make sure you are in the `school_shooting_analysis` rproj


#### Workspace setup ####
library(dplyr)

#### Simulate the Data ####

# Define the number of rows for the simulated dataset
n_simulated <- 830

# Simulate the dataset
set.seed(123) # Ensure reproducibility
simulated_data <- data.frame(
  year = sample(1990:2025, n_simulated, replace = TRUE),
  state = sample(unique(data$state), n_simulated, replace = TRUE),
  school_type = sample(c("public", "private"), n_simulated, replace = TRUE, prob = c(0.85, 0.15)),
  enrollment = sample(50:5000, n_simulated, replace = TRUE),
  killed = rpois(n_simulated, 1),
  injured = rpois(n_simulated, 2),
  white = sample(0:5000, n_simulated, replace = TRUE),
  black = sample(0:5000, n_simulated, replace = TRUE),
  hispanic = sample(0:5000, n_simulated, replace = TRUE),
  asian = sample(0:500, n_simulated, replace = TRUE),
  american_indian_alaska_native = sample(0:100, n_simulated, replace = TRUE),
  hawaiian_native_pacific_islander = sample(0:50, n_simulated, replace = TRUE),
  two_or_more = sample(0:300, n_simulated, replace = TRUE),
  resource_officer = sample(c(0, 1), n_simulated, replace = TRUE, prob = c(0.6, 0.4)),
  lat = runif(n_simulated, 24, 50),
  long = runif(n_simulated, -125, -66),
  staffing = runif(n_simulated, 10, 200),
  lunch = sample(c("free", "reduced", "none"), n_simulated, replace = TRUE, prob = c(0.5, 0.3, 0.2)),
  ulocale = sample(1:12, n_simulated, replace = TRUE),
  school_shooting = sample(c(0, 1), n_simulated, replace = TRUE, prob = c(0.8, 0.2)),
  data_source = "simulated",
  ulocale_desc = sample(unique(data$ulocale_desc[!is.na(data$ulocale_desc)]), n_simulated, replace = TRUE),
  top_1_races = sample(unique(data$top_1_races), n_simulated, replace = TRUE)
)

# Add proportion columns
simulated_data <- simulated_data %>%
  mutate(
    white_proportion = pmin(white / enrollment, 1),
    black_proportion = pmin(black / enrollment, 1),
    hispanic_proportion = pmin(hispanic / enrollment, 1)
  )

# Compute casualties as the sum of killed and injured
simulated_data <- simulated_data %>%
  mutate(casualties = killed + injured)

#### Save data ####
write_csv(simulated_data, here::here("data/00-simulated_data/simulated_data.csv"))

