# Create dashboard data

# Assign the end of the date range for Essence data download
end_date <- Sys.Date() - 1

source("scripts/fn-analysis.R")
source("scripts/1-setup.R")
source("scripts/2-get-ess.R")
source("scripts/3-satscan.R")
source("scripts/4-save.R")




for (i in 7:0) {
  end_date <- Sys.Date() - 1 - i

  print(end_date)

  source("scripts/fn-analysis.R")
  source("scripts/1-setup.R")
  source("scripts/2-get-ess.R")
  source("scripts/3-satscan.R")
  source("scripts/4-save.R")
}




