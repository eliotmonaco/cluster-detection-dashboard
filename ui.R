page_navbar(
  title = "KC Active Cluster Detection Dashboard",
  id = "nav",
  theme = bs_theme("navbar-bg" = "#0d3769") |>
    bs_add_rules(sass::sass_file("www/sass/custom.scss")),
  header = useBusyIndicators(),

  nav_panel(
    "Clusters",
    layout_sidebar(
      sidebar = sidebar(
        data_select_ui("clust", date_input_choices),
        syn_select_ui("clust", syn_input_choices),
        ri_select_ui("clust", ri_input_choices),
        zoom_select_ui("clust")
      ),
      navset_tab(
        nav_panel(
          "Overview",
          cluster_overview_ui("clust")
        ),
        nav_panel(
          "Clusters by patient location",
          syn_heading_ui("pat"),
          layout_column_wrap(
            cluster_map_ui("pat"),
            location_table_ui("pat")
          ),
          # card(htmlOutput("ptxt"), height = "100px"), # check map & table IDs
          cluster_table_ui("pat")
        ),
        nav_panel(
          "Clusters by hospital location",
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
        data_select_ui("dd", date_input_choices),
        syn_select_ui("dd", syn_input_choices),
        ri_select_ui("dd", ri_input_choices)
      ),
      navset_tab(
        nav_panel(
          "Data by patient location",
          card(markdown(readLines("scripts/dd.md"))),
          dd_ui("pat-sex", "Sex"),
          dd_ui("pat-age", "Age group")
        ),
        nav_panel(
          "Data by hospital location",
          card(markdown(readLines("scripts/dd.md"))),
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
        data_select_ui("ts", date_input_choices),
        syn_select_ui("ts", syn_input_choices),
        days_select_ui("ts", ts_input_choices)
      ),
      ts_plot_ui("pat", uitext$ts$pat$hd, uitext$ts$pat$ft),
      ts_plot_ui("hosp", uitext$ts$hosp$hd, uitext$ts$hosp$ft)
    )
  ),

  nav_panel(
    "Syndromes",
    layout_sidebar(
      sidebar = sidebar(
        data_select_ui("syn", date_input_choices)
      ),
      syn_info_table_ui("syn")
    )
  ),

  nav_panel(
    "About",
    tags$div(
      includeMarkdown("scripts/about.md"),
      style = "width:980px; margin:auto"
    )
  )

)
