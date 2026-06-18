# Create files for export

rename_vars <- function(x) {
  stringr::str_to_sentence(gsub("_", " ", x))
}

export <- list()

# Cluster summary, hospital clusters only, keep syndromes with > 0 clusters
export$Summary <- clust_smry |>
  filter_cluster_summary(ri_min = 3) |>
  select(-ends_with("_pat")) |>
  mutate(sum = rowSums(across(ends_with("_hosp")))) |>
  filter_out(sum == 0) |>
  select(-sum) |>
  rename_with(~ rename_vars(sub("_hosp", "", .x)))

# Filter location tables with at least 1 cluster of moderate RI level (minimum)
# and at least 1 location in KC
export$tbls <- lapply(syndata$satscan_results$hospital, \(ls) {
  df <- ls$gis

  if (!is.data.frame(df)) return(NULL)

  if (all(!unique(df$kc))) return(NULL)

  vars <- c(
    "Hospital" = "loc_id",
    "In KC" = "kc",
    "Cluster" = "cluster",
    "RI" = "recurr_int",
    "RI level" = "ri_level",
    "Observed" = "loc_obs",
    "Expected" = "loc_exp",
    "Obs/exp" = "loc_ode"
  )

  df <- df |>
    filter(as.numeric(ri_level) >= 3) |>
    mutate(
      recurr_int = prettyNum(signif(recurr_int, 2), scientific = TRUE),
      loc_exp = round_ties_away(loc_exp, 0),
      loc_ode = round_ties_away(loc_ode, 2),
      across(
        c(loc_obs, loc_exp),
        ~ prettyNum(.x, big.mark = ",")
      )
    ) |>
    select(all_of(vars)) |>
    rename(any_of(vars))

  if (nrow(df) == 0) NULL else df
}) |>
  compact()

# Export Excel file
export <- c(export["Summary"], export$tbls)

wb <- wb_workbook()

for (i in 1:length(export)) {
  wb <- wb |>
    wb_add_worksheet(sheet = names(export)[i]) |>
    wb_add_data(x = export[[i]])
}

wb_save(wb, paste0(
  "data/export/hospital-clusters-",
  format(Sys.Date(), "%Y%m%d"),
  ".xlsx"
))

