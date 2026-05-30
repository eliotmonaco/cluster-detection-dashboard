# Configure and save final datasets for dashboard

# Create auxiliary data
auxdata <- list()

# Date of update
auxdata$date <- max(procdata$date_range)

# Time series input choices
auxdata$ts <- list(
  "Two weeks" = "14",
  "30 days" = "30",
  "90 days" = "90",
  "180 days" = "180",
  "One year" = "365"
)

# Recurrence interval input choices
auxdata$ri_levels <- list(
  "Very weak (< 100 days)" = 1,
  "Weak (100 days to < 1 year)" = 2,
  "Moderate (1 year to < 5 years)" = 3,
  "Strong (5 years to < 100 years)" = 4,
  "Very strong (≥ 100 years)" = 5
)

# Recurrence interval colors
auxdata$ri_bg <- viridisLite::turbo(5, begin = .3, end = .9)

auxdata$ri_text <- setmeup::contrast_color(auxdata$ri_bg)

# Syndrome input choices (initial)
auxdata$syn <- get_syn_choices(
  df = procdata$syndromes,
  ls = procdata$satscan_results,
  colors = auxdata$ri_bg
)

# UI text
auxdata$uitext <- list(
  update = paste(
    "Data last updated on",
    format(Sys.Date(), "%b %d, %Y")
  ),
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
auxdata$graph <- list(
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

saveRDS(auxdata, "data/dashboard/aux_data.rds")
saveRDS(procdata, "data/dashboard/analysis_data.rds")

