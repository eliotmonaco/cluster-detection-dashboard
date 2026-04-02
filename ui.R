page_navbar(
  title = "Kansas City Syndrome Tracker",
  id = "nav",
  theme = bs_theme("navbar-bg" = "#0d3769") |>
    bs_add_rules(sass::sass_file("www/sass/custom.scss")),

  nav_panel(
    "Time series",
    layout_sidebar(
      sidebar = sidebar(
        date_input_analysis("date1", dt),
        select_input_syndrome("syn1", synselect1),
        radio_buttons_daterange("dtrng1", daterng1)
      ),
      card(
        card_header("ER visits by patient location"),
        highchartOutput("tspat"),
        tags$div(uitext$tspat)
      ),
      card(
        card_header("ER visits by hospital location"),
        highchartOutput("tshosp"),
        tags$div(uitext$tshosp)
      )
    )
  ),

  nav_panel(
    "Clusters",
    layout_sidebar(
      sidebar = sidebar(
        date_input_analysis("date2", dt),
        select_input_syndrome("syn2", synselect1),
        checkboxInput(
          inputId = "sigp",
          label = uitext$sigp,
          value = TRUE
        ),
        numericInput(
          inputId = "zoom",
          label = "Default map zoom level",
          value = 9,
          min = 0
        )
      ),
      navset_tab(
        nav_panel(
          "Overview",
          card(
            p(uitext$cctbl),
            reactableOutput("clustct"),
            class = "overview-tbl"
          )
        ),
        nav_panel(
          "Clusters by patient location",
          htmlOutput("titlesyn1"),
          layout_column_wrap(
            card(
              leafletOutput("pmap"),
              full_screen = TRUE,
              class = "map-zip-row"
            ),
            card(
              card_header("Locations in cluster"),
              reactableOutput("ploc"),
              class = "map-zip-row"
            )
          ),
          # card(htmlOutput("ptxt")), # for checking map & table IDs
          card(
            card_header("Clusters"),
            reactableOutput("pclust"),
            class = "clust-tbl-row"
          )
        ),
        nav_panel(
          "Clusters by hospital location",
          htmlOutput("titlesyn2"),
          layout_column_wrap(
            card(
              leafletOutput("hmap"),
              full_screen = TRUE,
              class = "map-zip-row"
            ),
            card(
              card_header("Locations in cluster"),
              reactableOutput("hloc"),
              class = "map-zip-row"
            )
          ),
          card(
            # card(htmlOutput("htxt")), # for checking map & table IDs
            card_header("Clusters"),
            reactableOutput("hclust"),
            class = "clust-tbl-row"
          )
        )
      )
    )
  ),

  nav_panel(
    "Data details",
    layout_sidebar(
      sidebar = sidebar(
        date_input_analysis("date3", dt),
        select_input_syndrome("syn3", synselect1),
      ),
      navset_tab(
        nav_panel(
          "Data by patient location",
          card_dc(reactableOutput("ddpsex")),
          card_dc(reactableOutput("ddpage"))
        ),
        nav_panel(
          "Data by hospital location",
          card_dc(reactableOutput("ddhsex")),
          card_dc(reactableOutput("ddhage"))
        )
      )
    )
  ),

  nav_panel(
    "Syndromes",
    layout_sidebar(
      sidebar = date_input_analysis("date4", dt),
      card(reactableOutput("syntbl"))
    )
  ),

  nav_panel(
    "About",
    card(markdown(readLines("scripts/about.md")))
  )

)
