# GLOBAL ------------------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(purrr)
library(stringr)
library(setmeup)
library(sf)
library(highcharter)
library(reactable)
library(leaflet)
library(markdown)

files <- list.files("R", pattern = "^(fn|mod)-", full.names = TRUE)

lapply(files, source)

# Import dashboard data
syndata <- readRDS("data/dashboard/syndrome_data.rds")

# Import aux data
auxdata <- readRDS("data/dashboard/aux_data.rds")

# Import spatial data
geodata <- readRDS("data/dashboard/geographic_data.rds")

# Import ANSI codes
ansi <- readRDS("data/dashboard/ansi_state_codes.rds")

# UI ----------------------------------------------------------------------

ui <- page_navbar(

  title = "KC Syndromic Cluster Detection",
  id = "nav",
  theme = bs_theme("navbar-bg" = "#0d3769") |>
    bs_add_rules(sass::sass_file("www/sass/custom.scss")),
  header = useBusyIndicators(),

  nav_panel(
    "Active clusters summary",
    layout_sidebar(
      sidebar = sidebar(
        date = auxdata$date_updated,
        ri_select_ui("smry", auxdata$ri_levels),
        compact_toggle_ui("smry")
      ),
      md_text_ui("smry"),
      cluster_summary_ui("smry")
    )
  ),

  nav_panel(
    "Clusters detail",
    layout_sidebar(
      sidebar = sidebar(
        date = auxdata$date_updated,
        syn_select_ui("synclust", auxdata$syn),
        ri_select_ui("synclust", auxdata$ri_levels),
        zoom_select_ui("synclust")
      ),
      navset_tab(
        nav_panel(
          "Clusters by patient location",
          syn_heading_ui("pat"),
          layout_column_wrap(
            cluster_map_ui("pat"),
            location_table_ui("pat")
          ),
          cluster_table_ui("pat")
        ),
        nav_panel(
          "Clusters by hospital location",
          syn_heading_ui("hosp"),
          layout_column_wrap(
            cluster_map_ui("hosp"),
            location_table_ui("hosp")
          ),
          cluster_table_ui("hosp")
        )
      )
    )
  ),

  nav_panel(
    "Cluster timeline",
    md_text_ui("tmln"),
    navset_tab(
      nav_panel(
        "Clusters by patient location",
        cluster_timeline_ui("pat")
      ),
      nav_panel(
        "Clusters by hospital location",
        cluster_timeline_ui("hosp")
      )
    )
  ),

  nav_panel(
    "Data details",
    layout_sidebar(
      sidebar = sidebar(
        date = auxdata$date_updated,
        syn_select_ui("dd", auxdata$syn),
        ri_select_ui("dd", auxdata$ri_levels)
      ),
      md_text_ui("dd"),
      navset_tab(
        nav_panel(
          "Data by patient location",
          dd_ui("pat-res", "Residence"),
          dd_ui("pat-travel", "Travel"),
          dd_ui("pat-sex", "Sex"),
          dd_ui("pat-age", "Age group")
        ),
        nav_panel(
          "Data by hospital location",
          dd_ui("hosp-res", "Residence"),
          dd_ui("hosp-travel", "Travel"),
          dd_ui("hosp-sex", "Sex"),
          dd_ui("hosp-age", "Age group")
        )
      )
    )
  ),

  nav_panel(
    "Time series",
    layout_sidebar(
      sidebar = sidebar(
        date = auxdata$date_updated,
        syn_select_ui("ts", auxdata$syn),
        days_select_ui("ts", auxdata$ts)
      ),
      ts_plot_ui("pat", auxdata$tstext$pat),
      ts_plot_ui("hosp", auxdata$tstext$hosp)
    )
  ),

  nav_panel(
    "Syndromes",
    syn_info_table_ui("syn")
  ),

  nav_panel(
    "About",
    shiny::tags$div(
      shiny::includeMarkdown("R/text-about.md"),
      style = "width:980px; margin:auto"
    )
  )

)

# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {

  rv <- reactiveValues()

  # INPUTS

  observe({
    rv$data <- syndata
    rv$geo <- geodata
    rv$ansi <- ansi
    rv$aux <- auxdata
  })

  # Syndrome selection
  syn_select_server("synclust", rv, auxdata$ri_bg)
  syn_select_server("dd", rv, auxdata$ri_bg)
  syn_select_server("ts", rv, auxdata$ri_bg)

  # Recurrence interval selection
  ri_select_server("smry", rv)
  ri_select_server("synclust", rv)
  ri_select_server("dd", rv)

  # Map zoom level selection
  zoom_select_server("synclust", rv)

  # Days selection
  days_select_server("ts", rv)

  # Compact table toggle
  compact_toggle_server("smry", rv)

  # TEXT

  syn_heading_server("pat", rv)
  syn_heading_server("hosp", rv)

  md_text_server("smry", file = "R/text.md", delim = "[smry]")
  md_text_server("tmln", file = "R/text.md", delim = "[tmln]")
  md_text_server("dd", file = "R/text.md", delim = "[dd]")

  # PLOTS

  # Cluster timeline
  cluster_timeline_server("pat", rv, "patient")
  cluster_timeline_server("hosp", rv, "hospital")

  # Time series
  ts_plot_server("pat", rv, "patient")
  ts_plot_server("hosp", rv, "hospital")

  # Cluster maps
  cluster_map_server(
    "pat", rv, src = "patient",
    var = "GEOID20", loc_bnd = geodata$zctas,
    hosp_loc = NULL, gp = auxdata$graph$patient
  )
  cluster_map_server(
    "hosp", rv, src = "hospital",
    var = "loc_id", loc_bnd = geodata$counties,
    hosp_loc = geodata$hosp, gp = auxdata$graph$hospital
  )

  # TABLES

  # Clusters summary
  cluster_summary_server("smry", rv)

  # Clusters by syndrome
  cluster_table_server("pat", rv, "patient")
  cluster_table_server("hosp", rv, "hospital")
  location_table_server("pat", rv, "patient")
  location_table_server("hosp", rv, "hospital")

  # Data details
  dd_server("pat-res", rv, "patient", "residence")
  dd_server("pat-travel", rv, "patient", "travel")
  dd_server("pat-sex", rv, "patient", "sex")
  dd_server("pat-age", rv, "patient", "age_group")
  dd_server("hosp-res", rv, "hospital", "residence")
  dd_server("hosp-travel", rv, "hospital", "travel")
  dd_server("hosp-sex", rv, "hospital", "sex")
  dd_server("hosp-age", rv, "hospital", "age_group")

  # Syndromes
  syn_info_table_server("syn", rv)

}

# RUN ---------------------------------------------------------------------

shinyApp(ui = ui, server = server)
