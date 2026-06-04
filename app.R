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

source("R/mod-inputs.R")
source("R/mod-clust.R")
source("R/mod-dd.R")
source("R/mod-ts.R")
source("R/mod-syn.R")
source("R/fn-inputs.R")
source("R/fn-clust-map.R")
source("R/fn-tbls.R")
source("R/fn-dd.R")
source("R/fn-ts.R")

# Import dashboard data
dbdata <- readRDS("data/dashboard/analysis_data.rds")

# Import spatial data
geo <- readRDS("data/dashboard/geographic_data.rds")

# Import ANSI codes
ansi <- readRDS("data/dashboard/ansi_state_codes.rds")

# Import aux data
auxdata <- readRDS("data/dashboard/aux_data.rds")

# UI ----------------------------------------------------------------------

ui <- page_navbar(

  title = "KC Syndromic Cluster Detection Dashboard",
  id = "nav",
  theme = bs_theme("navbar-bg" = "#0d3769") |>
    bs_add_rules(sass::sass_file("www/sass/custom.scss")),
  header = useBusyIndicators(),

  nav_panel(
    "Clusters",
    layout_sidebar(
      sidebar = sidebar(
        shiny::tags$p(auxdata$uitext$update),
        # data_select_ui("clust", auxdata$date),
        syn_select_ui("clust", auxdata$syn),
        ri_select_ui("clust", auxdata$ri_levels),
        zoom_select_ui("clust")
      ),
      navset_tab(
        nav_panel(
          "Overview (table)",
          cluster_overview_ui("clust")
        ),
        nav_panel(
          "Clusters by patient location (map + tables)",
          syn_heading_ui("pat"),
          layout_column_wrap(
            cluster_map_ui("pat"),
            location_table_ui("pat")
          ),
          # card(htmlOutput("ptxt"), height = "100px"), # check map & table IDs
          cluster_table_ui("pat")
        ),
        nav_panel(
          "Clusters by hospital location (map + tables)",
          syn_heading_ui("hosp"),
          layout_column_wrap(
            cluster_map_ui("hosp"),
            location_table_ui("hosp")
          ),
          # card(htmlOutput("htxt"), height = "100px"), # check map & table IDs
          cluster_table_ui("hosp")
        )
      )
    )
  ),

  nav_panel(
    "Data details",
    layout_sidebar(
      sidebar = sidebar(
        shiny::tags$p(auxdata$uitext$update),
        # data_select_ui("dd", auxdata$date),
        syn_select_ui("dd", auxdata$syn),
        ri_select_ui("dd", auxdata$ri_levels)
      ),
      navset_tab(
        nav_panel(
          "Data by patient location",
          card(shiny::markdown(readLines("R/text-dd.md"))),
          dd_ui("pat-sex", "Sex"),
          dd_ui("pat-age", "Age group")
        ),
        nav_panel(
          "Data by hospital location",
          card(shiny::markdown(readLines("R/text-dd.md"))),
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
        shiny::tags$p(auxdata$uitext$update),
        # data_select_ui("ts", auxdata$date),
        syn_select_ui("ts", auxdata$syn),
        days_select_ui("ts", auxdata$ts)
      ),
      ts_plot_ui("pat", auxdata$uitext$ts$pat$hd, auxdata$uitext$ts$pat$ft),
      ts_plot_ui("hosp", auxdata$uitext$ts$hosp$hd, auxdata$uitext$ts$hosp$ft)
    )
  ),

  nav_panel(
    "Syndromes",
    # layout_sidebar(
    #   sidebar = sidebar(
    #     data_select_ui("syn", auxdata$date)
    #   ),
    #   syn_info_table_ui("syn")
    # )
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

  # # Data selection
  # data_select_server("clust", rv, dbdata)
  # data_select_server("dd", rv, dbdata)
  # data_select_server("ts", rv, dbdata)
  # data_select_server("syn", rv, dbdata)

  observe({
    rv$data <- dbdata
    rv$geo <- geo
    rv$ansi <- ansi
    rv$auxdata <- auxdata
  })

  # Syndrome selection
  syn_select_server("clust", rv, auxdata$ri_bg)
  syn_select_server("dd", rv, auxdata$ri_bg)
  syn_select_server("ts", rv, auxdata$ri_bg)

  # Recurrence interval selection
  ri_select_server("clust", rv)
  ri_select_server("dd", rv)

  # Map zoom level selection
  zoom_select_server("clust", rv)

  # Days selection
  days_select_server("ts", rv)

  # TEXT

  syn_heading_server("pat", rv)
  syn_heading_server("hosp", rv)

  # PLOTS

  # Time series
  ts_plot_server("pat", rv, "patient")
  ts_plot_server("hosp", rv, "hospital")

  # Cluster maps
  cluster_map_server(
    "pat", rv, src = "patient", loc = geo$zctas,
    var = "GEOID20", loc_bnd = geo$zctas,
    hosp_loc = NULL, gp = rv$auxdata$graph$patient
  )
  cluster_map_server(
    "hosp", rv, src = "hospital",
    loc = rv$clustdata_hospital$shapeclust,
    var = "loc_id", loc_bnd = geo$counties,
    hosp_loc = geo$hosp, gp = rv$auxdata$graph$hospital
  )

  # TABLES

  # Clusters
  cluster_overview_server("clust", rv)
  cluster_table_server("pat", rv, "patient")
  cluster_table_server("hosp", rv, "hospital")
  location_table_server("pat", rv, "patient")
  location_table_server("hosp", rv, "hospital")

  # Data details
  dd_server("pat-sex", rv, "patient", "sex")
  dd_server("pat-age", rv, "patient", "age_group")
  dd_server("hosp-sex", rv, "hospital", "sex")
  dd_server("hosp-age", rv, "hospital", "age_group")

  # Syndromes
  syn_info_table_server("syn", rv)

  # TESTING

  # output$ptxt <- renderUI({
  #   HTML(paste(
  #     "Map ID:", rv$map_id_patient, "<br>",
  #     "Table ID:", rv$tbl_id_patient
  #   ))
  # })
  #
  # output$htxt <- renderUI({
  #   HTML(paste(
  #     "Map ID:", rv$map_id_hospital, "<br>",
  #     "Table ID:", rv$tbl_id_hospital
  #   ))
  # })

}

# RUN ---------------------------------------------------------------------

shinyApp(ui = ui, server = server)
