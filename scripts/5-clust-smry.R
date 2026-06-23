# Create cluster summary and timeline

# Cluster summary
clust_smry <- summarize_clusters(ssresults, syn)

syndata$cluster_summary = clust_smry

saveRDS(
  clust_smry,
  paste0("data/prep/cluster-summaries/clust-smry-", end_date, ".rds")
)

# Cluster timeline
files <- list.files("data/prep/cluster-summaries", full.names = TRUE)

clust_smries <- lapply(files, \(x) {
  date <- str_extract(x, regex("\\d{4}-\\d{2}-\\d{2}"))

  df <- readRDS(x)

  df |>
    mutate(date = as.Date(date))
}) |>
  list_rbind()

p1 <- clust_smries |>
  config_clusters(data_source = "patient") |>
  cluster_timeline_plot(colors = auxdata$ri_bg)

p2 <- clust_smries |>
  config_clusters(data_source = "hospital") |>
  cluster_timeline_plot(colors = auxdata$ri_bg)

syndata$cluster_timeline <- list(
  patient = p1,
  hospital = p2
)

saveRDS(syndata, "data/dashboard/syndrome_data.rds")

