# Create dashboard data (TEST)

library(Rnssp)
library(tidyverse)
library(setmeup)
library(kcData)
library(rsatscan)
library(sf)

# Load Essence profile object, needed for `get_api_data()`
load("data/myProfile.rda")

# Import data
geo <- readRDS("data/geographic_data.rds")
ansi <- readRDS("data/ansi_state_codes.rds")

# Assign the end of the date range for Essence data download
end_date <- Sys.Date()

# Create a directory in `data/` for storing output
# dir_data <- paste0("data/an-", end_date, "/")
dir_data <- "data/test/"
unlink(dir_data, recursive = TRUE, force = TRUE)
dir.create(dir_data)

source("R/analysis-fns.R")
source("scripts/syndromes.R")
syn <- syn[c(1, 22)]
source("scripts/get-ess.R")
source("scripts/satscan.R")





# Update ssenv parameters to account for the multiple dataset logic.

ssenv$.ss.params <- ssenv$.ss.params[-c(67, 68, 69)] # TEST
ssenv$.ss.params <- append(
  ssenv$.ss.params, # TEST
  c(
    "[Multiple Data Sets]", # TEST
    "; multiple data sets purpose type (0=Multivariate, 1=Adjustment)", # TEST
    "MultipleDataSetsPurposeType=0", # TEST
    "; case data filename (additional data set 2)", # TEST
    "CaseFile2=", # TEST
    "; case data filename (additional data set 3)", # TEST
    "CaseFile3="
  ), # TEST
  after = 66
) # TEST





inputsyn <- "resp"

# inputdtrng <- daterng1$`One year`

data <- get_db_data(dbdata, max(dt))

# # Data details
# dd <- dbdata |>
#   get_db_data(max(dt), "dd") |>
#   config_dd_table_data(inputsyn, inputdtrng)

# Filter Satscan data
ss <- data$ss

# Filter cluster data
clustdata <- config_syndrome_data(ss, inputsyn, TRUE)

# Filter cluster locations
clustbound <- list(
  patient = get_cluster_boundaries(
    clustdata$patient,
    geo = geo$zctas,
    var = "GEOID20"
  ),
  hospital = get_cluster_boundaries(
    clustdata$hospital,
    geo = clustdata$hospital$shapeclust,
    var = "loc_id"
  )
)

# Cluster map (by patient)
cluster_map(
  cluster_boundaries = clustbound$patient,
  location_boundaries = geo$zctas,
  kc_boundary = geo$city,
  gp = gp$patient,
  zoom_level = 9
)

# Cluster map (by hospital)
cluster_map(
  cluster_boundaries = clustbound$hospital,
  cluster_points = clustdata$hospital$shapegis,
  location_boundaries = geo$counties,
  kc_boundary = geo$city,
  hospital_locations = geo$hosp,
  gp = gp$hospital,
  zoom_level = 9
)

# Cluster count table
ssresults |>
  significant_clusters_by_syndrome(syndrome = syn) |>
  clustcount_table()

# Cluster data table (by patient)
cluster_table(clustdata$patient$shapeclust)

# Location data table (by patient)
location_table(clustdata$patient$gis, id = 1, type = "patient")
location_table(clustdata$hospital$gis, id = 1, type = "hospital")






# inputsyn <- "resp"

df <- assemble_dd_summaries(
  data_details = filter_data_details(data$dd, "patient", inputsyn),
  cluster_data = clustdata,
  var = "sex",
  source = "patient"
)

dd_table(df, "sex")





