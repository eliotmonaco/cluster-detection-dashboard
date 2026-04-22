# Modules for data details tables

dd_ui <- function(id, header) {
  card(
    card_header(header),
    reactable::reactableOutput(NS(id, "ddtable")),
    min_height = "100px"
  )
}

dd_server <- function(id, rv, src, var) {
  moduleServer(id, function(input, output, session) {
    # Data details table data
    datadetails <- reactive({
      rv$data$data_details |>
        filter_data_details(
          src = src,
          syn = rv$syn
        ) |>
        assemble_dd_summaries(
          cluster_data = rv[[paste0("clustdata_", src)]],
          var = var,
          src = src
        )
    })

    # Data details table
    output$ddtable <- reactable::renderReactable({
      validate(need(
        datadetails(),
        "No data available"
      ))

      dd_table(datadetails(), var = var)
    })
  })
}

