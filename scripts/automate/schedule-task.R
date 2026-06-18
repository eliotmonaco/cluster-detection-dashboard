# Schedule Windows task to run scripts for updating dashboard data

dir <- "C:/Users/emonaco01/Documents/" # no spaces allowed in path

# SCHEDULE TASKS ----------------------------------------------------------

# Data update
taskscheduleR::taskscheduler_create(
  rscript = paste0(dir, "update-acdd-data.R"),
  schedule = "DAILY",
  starttime = "07:00",
  startdate = format(Sys.Date(), "%m/%d/%Y")
)

# Publish dashboard
taskscheduleR::taskscheduler_create(
  rscript = paste0(dir, "update-acdd-dashboard.R"),
  schedule = "DAILY",
  starttime = "07:30",
  startdate = format(Sys.Date(), "%m/%d/%Y")
)

# Copy log
taskscheduleR::taskscheduler_create(
  rscript = paste0(dir, "update-acdd-log.R"),
  schedule = "DAILY",
  starttime = "07:45",
  startdate = format(Sys.Date(), "%m/%d/%Y")
)

# DELETE TASKS ------------------------------------------------------------

# Delete tasks
taskscheduleR::taskscheduler_delete("update-acdd-data.R")

taskscheduleR::taskscheduler_delete("update-acdd-dashboard.R")

taskscheduleR::taskscheduler_delete("update-acdd-log.R")

