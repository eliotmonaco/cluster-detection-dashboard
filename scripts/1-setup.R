# Setup for data download and analysis

library(Rnssp)
library(rsatscan)
library(dplyr)
library(purrr)
library(setmeup)

# Load Essence profile object, needed for `Rnssp::get_api_data()`
load("data/prep/myProfile.rda")

# Import data
geodata <- readRDS("data/dashboard/geographic_data.rds")

ansi <- readRDS("data/dashboard/ansi_state_codes.rds")

# Assign the end of the date range for Essence data download
end_date <- Sys.Date() - 1

# Create a directory in `data/prep/` for storing output
dir_data <- paste0("data/prep/an-", end_date, "/")

unlink(dir_data, recursive = TRUE, force = TRUE)

dir.create(dir_data)

