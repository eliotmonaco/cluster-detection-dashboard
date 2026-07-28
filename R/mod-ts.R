# Modules for time series plot

ts_plot_ui <- function(id, header) {
  card(
    card_header(header),
    plotly::plotlyOutput(NS(id, "tsplot"))
  )
}

ts_plot_server <- function(id, rv, src) {
  moduleServer(id, function(input, output, session) {
    # Time series plot data
    data <- reactive({
      config_ts_plot_data(
        rv$data$time_series,
        src = src,
        syndrome = rv$syn,
        n_days = rv$days
      )
    })

    # Time series plot
    output$tsplot <- plotly::renderPlotly({
      kcPopsci:::ess_plot_timeseries(
        data(),
        title = rv$data$syndromes[[rv$syn]]$name1
      )
    })
  })
}

