# Setup for data download and analysis

library(Rnssp)
library(rsatscan)
library(dplyr)
library(purrr)
library(stringr)
library(setmeup)

# Load Essence profile object, needed for `Rnssp::get_api_data()`
load("data/prep/myProfile.rda")

# Import data
geodata <- readRDS("data/dashboard/geographic_data.rds")

ansi <- readRDS("data/dashboard/ansi_state_codes.rds")

# Create a directory in `data/prep/` for storing output
dir_data <- paste0("data/prep/analysis-", end_date, "/")

unlink(dir_data, recursive = TRUE, force = TRUE)

dir.create(dir_data)

