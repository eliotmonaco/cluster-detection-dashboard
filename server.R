function(input, output, session) {

  rv <- reactiveValues()

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

  # Recurrence interval selection
  ri_select_server("clust", rv)

  # Map zoom level selection
  zoom_select_server("clust", rv)

  # Days selection
  days_select_server("ts", rv)

  # TEXT --------------------------------------------------------------------

  syn_heading_server("pat", rv)
  syn_heading_server("hosp", rv)

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
    "hosp", rv, src = "hospital", loc = rv$clustdata_hospital$shapeclust,
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
  syn_info_table_server("syn", rv)

  # TESTING -----------------------------------------------------------------

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
