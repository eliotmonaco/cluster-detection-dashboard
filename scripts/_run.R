# Create dashboard data

# Assign the end of the date range for Essence data download
end_date <- Sys.Date() - 1

files <- list.files("scripts", pattern = "^\\d-", full.names = TRUE)

source("scripts/fn-prep.R")

lapply(files, source)




# files <- list.files("scripts", pattern = "^\\d-", full.names = TRUE)
#
# for (i in 3:1) {
#   end_date <- Sys.Date() - 1 - i
#
#   print(end_date)
#
#   source("scripts/fn-prep.R")
#
#   lapply(files, source)
# }

