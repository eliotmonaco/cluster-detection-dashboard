function(input, output, session) {

  # INPUTS ------------------------------------------------------------------

  # Data selection
  data_select_server("clust", rv)
  data_select_server("dd", rv)
  data_select_server("ts", rv)
  data_select_server("syn", rv)

  # Syndrome selection
  syn_select_server("clust", rv)
  syn_select_server("dd", rv)
  syn_select_server("ts", rv)

  # P-value selection
  pval_select_server("clust", rv)

  # Map zoom level selection
  zoom_select_server("clust", rv)

  # Days selection
  days_select_server("ts", rv)

  # REACTIVES ---------------------------------------------------------------

  rv <- reactiveValues()

  # Initialize map cluster IDs as NULL for validation message to appear in
  # cluster location table even when p-value checkbox is not selected

  # rv <- reactiveValues(
  #   # data = get_db_data(dbdata, max(date_input_choices)),
  #   # syn = syn_input_choices[[1]],
  #   # synselect = get_db_data(dbdata, max(date_input_choices), "syndromes") |>
  #   #   syn_select_list(),
  #   map_id_patient = NULL,
  #   map_id_hospital = NULL,
  #   tbl_id_patient = NA,
  #   tbl_id_hospital = NA
  # )

  # TEXT --------------------------------------------------------------------

  output$titlesyn1 <- renderUI({
    syndrome_title_tag(rv$syn, rv$synselect)
  })

  output$titlesyn2 <- renderUI({
    syndrome_title_tag(rv$syn, rv$synselect)
  })

  # output$ptxt <- renderUI({
  #   HTML(paste(
  #     "Input value:", input$clust_map_shape_click$id, "<br>",
  #     "Map ID:", rv$map_id_patient, "<br>",
  #     "Table ID:", rv$tbl_id_patient
  #   ))
  # })
  #
  # output$htxt <- renderUI({
  #   HTML(paste(
  #     "Input value:", input$clust_map_shape_click$id, "<br>",
  #     "Map ID:", rv$map_id_hospital, "<br>",
  #     "Table ID:", rv$tbl_id_hospital
  #   ))
  # })

  # PLOTS -------------------------------------------------------------------

  # Time series
  ts_plot_server("pat", rv, "patient")
  ts_plot_server("hosp", rv, "hospital")

  # Cluster maps
  cluster_map_server(
    "pat", rv, src = "patient", loc = geo$zctas,
    var = "GEOID20", loc_bnd = geo$zctas,
    hosp_loc = NULL, gp = gp$patient
  )
  cluster_map_server(
    "hosp", rv, src = "hospital", loc = rv$clustdata$hospital$shapeclust,
    var = "loc_id", loc_bnd = geo$counties,
    hosp_loc = geo$hosp, gp = gp$hospital
  )

  # TABLES ------------------------------------------------------------------

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
  output$syntbl <- renderReactable({
    req(rv$data$syn)
    syndrome_table(rv$data$syn)
  })

}
