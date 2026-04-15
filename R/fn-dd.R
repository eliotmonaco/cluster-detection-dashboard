# Functions for data details tables

# ls = `data_details` list from dashboard data
filter_data_details <- function(ls, src = c("hospital", "patient"), syndrome) {
  src <- match.arg(src)

  ls[[src]][[syndrome]]
}

# df = `gis` dataframe from Satscan output
get_location_ids <- function(df, cluster_id) {
  df |>
    dplyr::filter(cluster == cluster_id) |>
    dplyr::pull(loc_id) |>
    sort()
}

# df = `shapeclust` spatial dataframe from Satscan output
get_cluster_dates <- function(df, cluster_id) {
  df |>
    sf::st_drop_geometry() |>
    dplyr::filter(cluster == cluster_id) |>
    dplyr::select(start_date, end_date) |>
    as.character() |>
    as.Date(format = "%Y/%m/%d")
}

# Summarize all data from the data details table
# df = data details table for a specific syndrome
# var = a grouping variable
dd_full_summary <- function(df, var) {
  df |>
    dplyr::count(.data[[var]], .drop = FALSE) |>
    dplyr::mutate(pct = setmeup::pct(n, nrow(df)) / 100) |>
    dplyr::arrange(.data[[var]])
}

# Summarize a single cluster from the data details table
# df = data details table for a specific syndrome
# var = a grouping variable
dd_cluster_summary <- function(df, var, loc_var, loc_ids, cluster_dates) {
  df <- df |>
    dplyr::filter(
      .data[[loc_var]] %in% loc_ids,
      date >= cluster_dates[1],
      date <= cluster_dates[2]
    )

  df |>
    dplyr::count(.data[[var]], .drop = FALSE) |>
    dplyr::mutate(pct = setmeup::pct(n, nrow(df)) / 100) |>
    dplyr::arrange(.data[[var]])
}

# Assemble the full summary and cluster summaries
assemble_dd_summaries <- function(
  data_details,
  cluster_data,
  var,
  src = c("hospital", "patient")
) {
  src <- match.arg(src)

  smry1 <- dd_full_summary(data_details, var)

  cluster_ids <- cluster_data$shapeclust$cluster

  ri_levels <- gsub("\\s", "_", cluster_data$shapeclust$ri_level)

  sfx <- paste0(cluster_ids, "_(", ri_levels, ")")

  # Return smry1 if no clusters were detected
  if (length(cluster_ids) == 0) {
    return(smry1)
  }

  if (src == "hospital") {
    locvar <- "hospital_name_geo"
  } else if (src == "patient") {
    locvar <- "zip_code"
  }

  # Summarize data for each cluster
  smry_clust <- lapply(cluster_ids, \(x) {
    location_ids <- get_location_ids(cluster_data$gis, x)

    dates <- get_cluster_dates(cluster_data$shapeclust, x)

    dd_cluster_summary(
      data_details,
      var = var,
      loc_var = locvar,
      loc_ids = location_ids,
      cluster_dates = dates
    )
  })

  # Join cluster summaries
  smry2 <- purrr::reduce(smry_clust, dplyr::full_join, by = var)

  colnames(smry2) <- c(
    var,
    do.call(paste0, expand.grid(c("n", "pct"), sfx))
  )

  # Join full summary to cluster summaries
  smry1 |>
    dplyr::full_join(smry2, by = var) |>
    dplyr::arrange(.data[[var]])
}

# Data characteristics tables from data details
dd_table <- function(df, var, replace_nm = NULL, color = ri_bg_color) {
  # Assign RI levels as names to `color`
  color <- setNames(
    color,
    c("very_weak", "weak", "moderate", "strong", "very_strong")
  )

  # Configure column groups
  col_groups1 <- list(colGroup(name = "Study area", columns = c("n", "pct")))

  cols <- colnames(df)[grepl("\\d", colnames(df))]

  nm <- sub("^n", "", colnames(df)[grepl("^n\\d", colnames(df))])

  if (length(cols) != 0) {
    col_groups2 <- lapply(nm, \(x) {
      n <- stringr::str_extract(x, "\\d*")

      ri_lvl <- stringr::str_extract(x, "(?<=\\().*(?=\\))")

      bg <- as.character(color[ri_lvl])

      clr <- contrast_color(bg)

      colGroup(
        name = gsub("_", " ", paste("Cluster", x)),
        columns = cols[grepl(n, cols)],
        headerStyle = list(
          backgroundColor = bg,
          color = clr
        )
      )
    })
  } else {
    col_groups2 <- NULL
  }

  col_groups <- c(col_groups1, col_groups2)

  # Format columns
  col_defs1 <- list(colDef(
    name = mod_col_labels(var),
    sticky = "left"
  ))

  names(col_defs1) <- var

  cols <- colnames(df)[grepl("^n\\d*(_|$)", colnames(df))]

  col_defs2 <- lapply(cols, \(x) {
    colDef(
      name = "N",
      format = colFormat(separators = TRUE),
      style = list(borderLeft = "1px solid #ddd")
    )
  })

  names(col_defs2) <- cols

  cols <- colnames(df)[grepl("^pct\\d*(_|$)", colnames(df))]

  col_defs3 <- lapply(cols, \(x) {
    colDef(
      name = "Pct",
      format = colFormat(percent = TRUE)
    )
  })

  names(col_defs3) <- cols

  col_defs <- c(col_defs1, col_defs2, col_defs3)

  df |>
    reactable(
      columnGroups = col_groups,
      columns = col_defs,
      sortable = FALSE,
      pagination = FALSE,
      highlight = TRUE,
      compact = TRUE,
      fullWidth = FALSE,
      theme = reactableTheme(borderColor = "#ddd")
    )
}

