# Create aux data

auxdata <- list()

# Date of update
auxdata$date_updated <- end_date + 1

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

auxdata$ri_text <- contrast_color(auxdata$ri_bg)

# Syndrome input choices (initial)
auxdata$syn <- get_syn_choices(
  df = syndata$syndromes,
  ls = syndata$satscan_results,
  colors = auxdata$ri_bg
)

# Time series plot text
auxdata$tstext <- list(
  pat = "ER visits by patient location (residents of KC ZIP codes)",
  hosp = paste(
    "ER visits by hospital location (Cass, Clay, Jackson, and Platte",
    "Counties)"
  )
)

# Graphical parameters for cluster map shapes and markers
gp <- list(
  study = list(
    name = "Study area",
    clr = "#aaa", fill = "#aaa", wt = 2,
    opac1 = 1, opac2 = .1, shp = "square"
  ),
  kc = list(
    name = "KC boundary",
    clr = "#024cbf", fill = "#024cbf", wt = 2,
    opac1 = 1, opac2 = 0, shp = "square"
  ),
  clust = list(
    name = "Syndrome cluster",
    clr = "red", fill = "red", wt = 2,
    opac1 = 1, opac2 = .1, shp = "square"
  ),
  hosp = list(
    name = "Hospital",
    class = "hosp-icon-legend"
  ),
  wc = list(
    name = "World Cup site",
    class = "wc-icon-legend"
  )
)

auxdata$graph <- list(
  patient = list(
    study = replace(gp$study, 1, "Study area (ZCTA)"),
    kc = gp$kc,
    worldcup = gp$wc,
    clust = gp$clust
  ),
  hospital = list(
    study = replace(gp$study, 1, "Study area (county)"),
    kc = gp$kc,
    hosp = gp$hosp,
    worldcup = gp$wc,
    clust = replace(gp$clust, which(names(gp$clust) == "shp"), "circle")
  )
)

saveRDS(auxdata, "data/dashboard/aux_data.rds")

