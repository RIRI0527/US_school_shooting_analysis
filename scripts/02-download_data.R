#### Preamble ####
# Purpose: Downloads and saves the data from <https://github.com/washingtonpost/data-school-shootings?tab=readme-ov-file>
# Author: Ruizi Liu
# Date: 21 Novermber 2024
# Contact: ruizi.liu@mail.utoronto.ca
# License: MIT
# Pre-requisites: None
# Any other information needed? None


#### Workspace setup ####
library(tidyverse)
library(here)

#### Download data ####
# Read the data
raw_data <- read.csv("https://raw.githubusercontent.com/washingtonpost/data-school-shootings/master/school-shootings-data.csv")

# Define the destination path
dest_path <- here::here("data/01-raw_data", "raw_data.csv")

# Download the file from the URL and save it to the specified path
write_csv(raw_data, here::here(dest_path))

         
