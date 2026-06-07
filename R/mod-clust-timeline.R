# Modules for cluster timeline

cluster_timeline_ui <- function(id) {
  card(
    plotOutput(NS(id, "clusttimeline")),
    height = "5000px"
  )
}

cluster_timeline_server <- function(id, rv, src) {
  moduleServer(id, function(input, output, session) {
    # Cluster timeline plot
    output$clusttimeline <- renderPlot({
      rv$data$cluster_timeline[[src]]
    })
  })
}

