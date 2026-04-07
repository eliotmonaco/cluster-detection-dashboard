# Modules for cluster tables and maps

# Cluster count table on overview tab
cluster_overview_ui <- function(id) {
  card(
    p(
      "Number of clusters detected for each syndrome",
      style = "text-align:center;font-size:1.4rem;"
    ),
    p(
      "Clusters are grouped by the strength of the recurrence interval",
      style = "text-align:center;font-size:1.2rem;margin-bottom:16px;"
    ),
    reactableOutput(NS(id, "clustct")),
    class = "overview-tbl"
  )
}

cluster_overview_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    # Cluster counts
    clust_counts <- reactive({
      summarize_syndrome_clusters(
        rv$data$satscan_results,
        syndromes = rv$data$syndromes,
        ri_min = rv$ri
      )
    })

    # Cluster count table
    output$clustct <- renderReactable({
      cluster_count_table(clust_counts())
    })
  })
}

# Cluster table
cluster_table_ui <- function(id) {
  card(
    card_header("Clusters"),
    reactableOutput(NS(id, "clusttbl")),
    class = "clust-row-2"
  )
}

cluster_table_server <- function(id, rv, src) {
  moduleServer(id, function(input, output, session) {
    map_id <- paste0("map_id_", src)

    # Cluster data for maps and data details
    clustdata <- reactive({
      ls <- filter_cluster_data(
        rv$data$satscan_results[[src]][[rv$syn]],
        ri_min = rv$ri
      )

      if (src == "hospital") {
        expand_point_clusters(ls)
      } else {
        ls
      }
    })

    observe({
      rv[[paste0("clustdata_", src)]] <- clustdata()
    })

    # Cluster table
    output$clusttbl <- renderReactable({
      validate(need(
        clustdata()$shapeclust,
        "No clusters detected"
      ))

      cluster_table(clustdata()$shapeclust)
    })

    # When cluster table row is selected, update map cluster ID
    observeEvent(getReactableState("clusttbl"), {
      rv[[map_id]] <- getReactableState("clusttbl", name = "selected")
    })
  })
}

# Cluster locations table
location_table_ui <- function(id) {
  card(
    card_header("Locations in cluster"),
    reactableOutput(NS(id, "loctbl")),
    class = "clust-row-1"
  )
}

location_table_server <- function(id, rv, src) {
  moduleServer(id, function(input, output, session) {
    map_id <- paste0("map_id_", src)

    # Location table
    output$loctbl <- renderReactable({
      validate(need(
        rv[[map_id]],
        paste(
          "Select a cluster on the map or cluster table",
          "to see location details"
        )
      ))

      location_table(
        rv[[paste0("clustdata_", src)]]$gis,
        id = rv[[map_id]],
        src = src
      )
    })
  })
}

# Cluster map
cluster_map_ui <- function(id) {
  card(
    leafletOutput(NS(id, "clustmap")),
    full_screen = TRUE,
    class = "clust-row-1"
  )
}

cluster_map_server <- function(id, rv, src, loc, var, loc_bnd, hosp_loc, gp) {
  moduleServer(id, function(input, output, session) {
    tbl_id <- paste0("tbl_id_", src)
    map_id <- paste0("map_id_", src)

    # Cluster boundary data for maps
    clustbound <- reactive({
      get_cluster_boundaries(
        rv[[paste0("clustdata_", src)]],
        locations = loc,
        var = var
      )
    })

    # Cluster map
    output$clustmap <- renderLeaflet({
      cluster_map(
        cluster_boundaries = clustbound(),
        location_boundaries = loc_bnd,
        kc_boundary = geo$city,
        hospital_locations = hosp_loc,
        gp = gp,
        zoom_level = rv$zoom
      )
    })

    # Set map cluster ID to NULL when a new syndrome is selected
    observeEvent(rv$syn, {
      rv[[map_id]] <- NULL
    })

    # On map click update map cluster ID
    observeEvent(input$clustmap_shape_click, {
      rv[[map_id]] <- input$clustmap_shape_click$id
    })

    # When map cluster ID updates, update table ID, update table, and add
    # cluster outline to map
    observeEvent(rv[[map_id]], ignoreNULL = FALSE, {
      rv[[tbl_id]] <- update_cluster_table_id(rv[[map_id]])

      updateReactable("clusttbl", selected = rv[[tbl_id]])

      add_cluster_outline(
        map_id = "clustmap",
        data = clustbound(),
        shape_id = rv[[map_id]]
      )
    })
  })
}

# Syndrome heading
syn_heading_ui <- function(id) {
  htmlOutput(NS(id, "synheader"))
}

syn_heading_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    # Syndrome heading on clusters page
    output$synheader <- renderUI({
      syndrome_title_tag(rv$syn, rv$synselect)
    })
  })
}

