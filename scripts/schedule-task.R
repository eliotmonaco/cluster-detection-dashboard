# Schedule Windows task to run scripts for updating dashboard data

dir <- "C:/Users/emonaco01/Documents/" # no spaces allowed in path

# Schedule task 1
file <- "update-cluster-dashboard-data.R"

taskscheduleR::taskscheduler_create(
  rscript = paste0(dir, file),
  schedule = "DAILY",
  starttime = "07:00",
  startdate = format(Sys.Date() + 1, "%m/%d/%Y")
  # starttime = format(Sys.time() + 62, "%H:%M"),
  # startdate = format(Sys.Date(), "%m/%d/%Y")
)

# Schedule task 2
file <- "update-log.R"

taskscheduleR::taskscheduler_create(
  rscript = paste0(dir, file),
  schedule = "DAILY",
  starttime = "08:00",
  startdate = format(Sys.Date() + 1, "%m/%d/%Y")
  # starttime = format(Sys.time() + 122, "%H:%M"),
  # startdate = format(Sys.Date(), "%m/%d/%Y")
)

# Delete tasks
file <- "update-cluster-dashboard-data.R"
taskscheduleR::taskscheduler_delete(file)

file <- "update-log.R"
taskscheduleR::taskscheduler_delete(file)

