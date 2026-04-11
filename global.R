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
  "Weak (100 days to < 1 year)" = 2,
  "Moderate (1 year to < 5 years)" = 3,
  "Strong (5 years to < 100 years)" = 4,
  "Very strong (≥ 100 years)" = 5
)

# Recurrence interval colors
ri_bg_color <- adjustcolor(
  colorRampPalette(c("yellow", "orange", "red"))(5),
  green.f = .85, blue.f = .85
)

ri_text_color <- c(rep("black", 3), rep("white", 2))

# Syndrome input choices (initial)
syn_input_choices <- dbdata |>
  get_db_data(max(date_input_choices), "syndromes") |>
  get_syn_choices()

syn_strength <- dbdata |>
  get_db_data(max(date_input_choices), "satscan_results") |>
  get_syn_cluster_strength()

syn_input_choices <- add_ri_icons(
  syn = syn_input_choices,
  str = syn_strength,
  colors = ri_bg_color
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

