library(shiny)
library(bslib)
library(tidyverse)
library(setmeup)
library(sf)
library(highcharter)
library(reactable)
library(leaflet)

source("scripts/syndromes.R")
source("R/mod-inputs.R")
source("R/mod-clust.R")
source("R/mod-dd.R")
source("R/mod-ts.R")
source("R/fn-inputs.R")
source("R/fn-clust-map.R")
source("R/fn-tbls.R")
source("R/fn-dd.R")
source("R/fn-ts.R")

# Import dashboard data
dbdata <- readRDS("data/dashboard_data.rds")

# Import spatial data
geo <- readRDS("data/geographic_data.rds")

# Import ANSI codes
ansi <- readRDS("data/ansi_state_codes.rds")

# Date input choices
dirs <- list.dirs("data/", full.names = TRUE, recursive = FALSE)

dirs <- dirs[grepl("^data/an-", dirs)]

date_input_choices <- as.Date(sub("^data/an-", "", dirs))

# Syndrome input choices (initial)
syn_input_choices <- dbdata |>
  get_db_data(max(date_input_choices), "syndromes") |>
  syn_select_list()

# Time series input choices
ts_input_choices <- list(
  "Two weeks" = "14",
  "30 days" = "30",
  "90 days" = "90",
  "180 days" = "180",
  "One year" = "365"
)

# Recurrence interval input choices
ri_input_choices <- list(
  "Very weak (< 100 days)" = 1,
  "Weak (≥ 100 days)" = 2,
  "Moderate (≥ 1 year)" = 3,
  "Strong (≥ 5 years)" = 4,
  "Very strong (≥ 100 years)" = 5
)

# UI text
uitext <- list(
  ts = list(
    pat = list(
      hd = "ER visits by patient location",
      ft = paste(
        "This dataset consists of ER visit records for patients residing in",
        "Kansas City ZIP codes."
      )
    ),
    hosp = list(
      hd = "ER visits by hospital location",
      ft = paste(
        "This dataset consists of ER visit records from hospitals in Cass,",
        "Clay, Jackson, and Platte Counties."
      )
    )
  )
)

# Graphical parameters for cluster map shapes and markers
gp <- list(
  patient = list(
    study = list(
      name = "Study area (ZCTA)",
      clr = "#aaa",
      fill = "#aaa",
      wt = 2,
      opac1 = 1,
      opac2 = .1,
      shp = "square"
    ),
    kc = list(
      name = "KC boundary",
      clr = "#024cbf",
      fill = "#024cbf",
      wt = 2,
      opac1 = 1,
      opac2 = 0,
      shp = "square"
    ),
    clust = list(
      name = "Syndrome cluster",
      clr = "red",
      fill = "red",
      wt = 2,
      opac1 = .5,
      opac2 = .1,
      shp = "square"
    )
  ),
  hospital = list(
    study = list(
      name = "Study area (county)",
      clr = "#aaa",
      fill = "#aaa",
      wt = 2,
      opac1 = 1,
      opac2 = .1,
      shp = "square"
    ),
    kc = list(
      name = "KC boundary",
      clr = "#024cbf",
      fill = "#024cbf",
      wt = 2,
      opac1 = 1,
      opac2 = 0,
      shp = "square"
    ),
    hosp = list(
      name = "Hospital",
      class = "plus-legend"
    ),
    clust = list(
      name = "Syndrome cluster",
      clr = "red",
      fill = "red",
      wt = 2,
      opac1 = .5,
      opac2 = .1,
      shp = "circle"
    )
  )
)

