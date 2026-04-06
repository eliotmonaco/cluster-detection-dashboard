# Modules for cluster tables and maps

cluster_overview_ui <- function(id) {
  card(
    p(HTML(paste(
      "Spatiotemporal clusters are detected using SaTScan software.",
      "This table shows the number of clusters where p&nbsp;<&nbsp;0.05 for",
      "each syndrome."
    ))),
    reactableOutput(NS(id, "clustct")),
    class = "overview-tbl"
  )
}

cluster_overview_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    # Cluster count table data
    clust_count_table <- reactive({
      significant_clusters_by_syndrome(
        rv$data$satscan_results,
        syndromes = rv$data$syndromes
      )
    })

    # Cluster count table
    output$clustct <- renderReactable({
      clustcount_table(clust_count_table())
    })
  })
}

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
      config_syndrome_data(
        rv$data$satscan_results,
        syndrome = rv$syn,
        sig_pval = rv$pval
      )
    })

    observe({
      rv$clustdata <- clustdata()
    })

    # Cluster table
    output$clusttbl <- renderReactable({
      validate(need(
        rv$clustdata[[src]]$shapeclust,
        "No clusters detected"
      ))

      cluster_table(rv$clustdata[[src]]$shapeclust)
    })

    # When cluster table row is selected, update map cluster ID
    observeEvent(getReactableState("clusttbl"), {
      rv[[map_id]] <- getReactableState("clusttbl", name = "selected")
    })
  })
}

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
        rv$clustdata[[src]]$gis,
        id = rv[[map_id]],
        type = src
      )
    })
  })
}

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
        rv$clustdata[[src]],
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

