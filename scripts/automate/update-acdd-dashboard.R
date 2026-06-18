# # Initial setup
# rsconnect::connectCloudUser()
#
# # Confirm connection
# rsconnect::accounts()
#
# # Record app dependencies
# renv::snapshot(exclude = c("taskscheduleR", "rsconnect"))
#
# # Create manifest with app files and dependencies
# rsconnect::writeManifest(appFiles = c(
#   "app.R", "renv.lock", "data/dashboard", "R", "www"
# ))

# Publish app
dir_proj <- "C:/Users/emonaco01/OneDrive - City of Kansas City/Documents/projects/cluster-detection-dashboard/"

setwd(dir_proj)

rsconnect::deployApp(
  manifestPath = "manifest.json",
  # appName = "cluster-detection-dashboard",
  appId = "019e7492-fd92-2354-1cba-e1939ddc651f"
)

