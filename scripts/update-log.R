dir_task <- "C:/Users/emonaco01/Documents/"

dir_proj <- "C:/Users/emonaco01/OneDrive - City of Kansas City/Documents/projects/cluster-detection-dashboard/"

logfile <- "update-cluster-dashboard-data"

file.copy(
  paste0(dir_task, logfile, ".log"),
  paste0(dir_proj, "extras/", logfile, "-", Sys.Date(), ".log"),
  overwrite = TRUE
)

file.remove(paste0(dir_task, logfile, ".log"))

