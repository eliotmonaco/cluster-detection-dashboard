# Modules for time series plot

ts_plot_ui <- function(id, header, footer) {
  card(
    card_header(header),
    highchartOutput(NS(id, "tsplot")),
    p(footer)
  )
}

ts_plot_server <- function(id, rv, src) {
  moduleServer(id, function(input, output, session) {
    # Time series plot data
    data <- reactive({
      config_ts_plot_data(
        rv$data$time_series,
        source = src,
        syndrome = rv$syn,
        n_days = rv$days
      )
    })

    # Time series plot
    output$tsplot <- renderHighchart({
      ts_plot(
        data(),
        title = rv$data$syndromes[[rv$syn]]$name1
      )
    })
  })
}

