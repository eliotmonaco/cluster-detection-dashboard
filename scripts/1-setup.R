# Setup for data download and analysis

library(Rnssp)
library(rsatscan)
library(dplyr)
library(purrr)
library(stringr)
library(setmeup)
library(openxlsx2)

# Load Essence profile object, needed for `Rnssp::get_api_data()`
load("data/prep/myProfile.rda")

# Import data
geodata <- readRDS("data/dashboard/geographic_data.rds")

ansi <- readRDS("data/dashboard/ansi_state_codes.rds")

res <- readRDS("data/prep/residence_data.rds")

# Create a directory in `data/prep/` for storing output
dir_data <- paste0("data/prep/analyses/analysis-", end_date, "/")
# dir_data <- "data/prep/test"

unlink(dir_data, recursive = TRUE, force = TRUE)

dir.create(dir_data)

