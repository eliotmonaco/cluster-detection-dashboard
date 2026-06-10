# Modules for cluster summary table

# Cluster summary table
cluster_summary_ui <- function(id) {
  card(
    h3(
      "Active clusters grouped by recurrence interval (RI)",
      style = "text-align:center;"
    ),
    reactable::reactableOutput(NS(id, "clustsmry")),
    class = "clust-smry-tbl"
  )
}

cluster_summary_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    # Cluster summary
    clust_smry <- reactive({
      filter_cluster_summary(
        rv$data$cluster_summary,
        ri_min = rv$ri
      )
    })

    # Cluster summary table
    output$clustsmry <- reactable::renderReactable({
      cluster_summary_table(
        clust_smry(),
        bg_color = rv$aux$ri_bg,
        text_color = rv$aux$ri_text
      )
    })
  })
}

