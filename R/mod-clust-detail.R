# Modules for cluster maps and detail tables

# Cluster map
cluster_map_ui <- function(id) {
  card(
    leaflet::leafletOutput(NS(id, "clustmap")),
    full_screen = TRUE,
    height = "500px"
  )
}

cluster_map_server <- function(id, rv, src, var, loc_bnd, hosp_loc, gp) {
  moduleServer(id, function(input, output, session) {
    tbl_id <- paste0("tbl_id_", src)
    map_id <- paste0("map_id_", src)

    # Cluster boundary data for maps
    clustbound <- reactive({
      if (src == "patient") {
        loc <- rv$geo$zctas
      } else if (src == "hospital") {
        loc <- rv$clustdata_hospital$shapeclust
      }

      get_cluster_boundaries(
        rv[[paste0("clustdata_", src)]],
        locations = loc,
        var = var
      )
    })

    # Cluster map
    output$clustmap <- leaflet::renderLeaflet({
      cluster_map(
        cluster_boundaries = clustbound(),
        location_boundaries = loc_bnd,
        kc_boundary = rv$geo$city,
        hospital_locations = hosp_loc,
        worldcup_sites = rv$geo$worldcup,
        gp = gp,
        zoom_level = rv$zoom
      )
    })

    # Set map cluster ID to NULL when a new syndrome is selected
    observeEvent(rv$syn, {
      rv[[map_id]] <- NULL
    })

    # Set map cluster ID to NULL when a new RI minimum is selected
    observeEvent(rv$ri, {
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

      reactable::updateReactable("clusttbl", selected = rv[[tbl_id]])

      add_cluster_outline(
        map_id = "clustmap",
        data = clustbound(),
        shape_id = rv[[map_id]]
      )
    })
  })
}

# Cluster locations table
location_table_ui <- function(id, suppr, output_name = "loc_tbl_ft") {
  card(
    card_header("Locations in cluster"),
    reactable::reactableOutput(NS(id, "loctbl")),
    tbl_suppr_footnote(id, output_name, suppr),
    full_screen = TRUE,
    height = "500px"
  )
}

location_table_server <- function(id, rv, src, suppr) {
  moduleServer(id, function(input, output, session) {
    map_id <- paste0("map_id_", src)

    # Location table
    output$loctbl <- reactable::renderReactable({
      validate(need(rv[[map_id]], paste(
        "Select a cluster on the map or a row in the cluster table to see",
        "location details"
      )))

      location_table(
        rv[[paste0("clustdata_", src)]]$gis,
        id = rv[[map_id]],
        src = src,
        suppr = suppr
      )
    })

    # Logical output to trigger conditionalPanel()
    output$loc_tbl_ft <- reactive({
      !is.null(rv[[map_id]])
    })

    outputOptions(output, "loc_tbl_ft", suspendWhenHidden = FALSE)
  })
}

# Cluster table
cluster_table_ui <- function(id, suppr, output_name = "clust_tbl_ft") {
  card(
    card_header("Clusters"),
    reactable::reactableOutput(NS(id, "clusttbl")),
    tbl_suppr_footnote(id, output_name, suppr),
    full_screen = TRUE,
    min_height = "200px"
  )
}

cluster_table_server <- function(id, rv, src, suppr) {
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
    output$clusttbl <- reactable::renderReactable({
      validate(need(clustdata()$shapeclust, "No clusters detected"))

      cluster_table(
        clustdata()$shapeclust,
        bg_color = rv$aux$ri_bg,
        text_color = rv$aux$ri_text,
        suppr = suppr
      )
    })

    # Logical output to trigger conditionalPanel()
    output$clust_tbl_ft <- reactive({
      !is.null(rv[[paste0("clustdata_", src)]]$shapeclust)
    })

    outputOptions(output, "clust_tbl_ft", suspendWhenHidden = FALSE)

    # When cluster table row is selected, update map cluster ID
    observeEvent(reactable::getReactableState("clusttbl"), {
      rv[[map_id]] <- reactable::getReactableState(
        "clusttbl", name = "selected"
      )
    })
  })
}

