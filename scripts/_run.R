# Create dashboard data

# Assign the end of the date range for Essence data download
end_date <- Sys.Date() - 1

files <- list.files("scripts", pattern = "^\\d-", full.names = TRUE)

source("scripts/fn-prep.R")

lapply(files, source)

