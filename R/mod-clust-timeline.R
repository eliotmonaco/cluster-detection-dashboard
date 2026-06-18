# Modules for cluster timeline

cluster_timeline_ui <- function(id) {
  card(
    plotOutput(NS(id, "clusttimeline"), height = "7000px"),
    class = "clust-timeline"
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

