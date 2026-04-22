# Modules for the syndrome info table

syn_info_table_ui <- function(id) {
  card(reactable::reactableOutput(NS(id, "syntbl")))
}

syn_info_table_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    # Syndrome info table
    output$syntbl <- reactable::renderReactable({
      syndrome_table(rv$data$syndromes)
    })
  })
}

