# Modules for data details tables

dd_ui <- function(id, header) {
  card(
    card_header(header),
    reactableOutput(NS(id, "ddtable"))
  )
}

dd_server <- function(id, rv, src, var) {
  moduleServer(id, function(input, output, session) {
    # Data details table data
    data <- reactive({
      rv$data$data_details |>
        filter_data_details(
          src = src,
          syndrome = rv$syn
        ) |>
        assemble_dd_summaries(
          cluster_data = rv[[paste0("clustdata_", src)]],
          var = var,
          src = src
        )
    })

    # Data details table
    output$ddtable <- renderReactable({
      dd_table(data(), var = var)
    })
  })
}

