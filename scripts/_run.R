# Create dashboard data

# Assign the end of the date range for Essence data download
end_date <- Sys.Date() - 1

source("scripts/fn-prep.R")

files <- list.files("scripts", pattern = "^\\d-", full.names = TRUE)

lapply(files, source)

