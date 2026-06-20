# Functions for data details tables

# ls = `data_details` list from dashboard data
filter_data_details <- function(ls, src = c("hospital", "patient"), syn) {
  src <- match.arg(src)

  df <- ls[[src]][[syn]]

  # Convert variables with many possible values to factor here to keep levels
  # specific to this syndrome
  lvl <- sort(unique(df$travel))

  lvl_last <- c(
    lvl[grepl("(?i)^other$", lvl)],
    lvl[grepl("(?i)^(no|none)$", lvl)]
  )

  lvl_trav <- c(lvl[!lvl %in% lvl_last], lvl_last)

  df |>
    dplyr::mutate(travel = factor(travel, levels = lvl_trav))
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

# Summarize visits for one syndrome by one grouping variable for the full study
# population
# df = data details table for a specific syndrome
# var = a grouping variable
# suppr = the suppression level (counts below this number will be suppressed)
dd_full_summary <- function(df, var, suppr = NULL) {
  total <- nrow(df)

  df <- df |>
    dplyr::count(.data[[var]], .drop = FALSE)

  if (!is.null(suppr)) {
    df <- suppress_df(df, var = "n", n = suppr, sec = TRUE)
  }

  df |>
    dplyr::mutate(
      pct = setmeup::pct(suppressWarnings(as.numeric(n)), total)
    ) |>
    dplyr::arrange(.data[[var]])
}

# Summarize visits for one syndrome by one grouping variable for a single
# cluster
# df = data details table for a specific syndrome
# var = a grouping variable
dd_cluster_summary <- function(
  df, var, loc_var, loc_ids, cluster_dates, suppr = NULL
) {
  df <- df |>
    dplyr::filter(
      .data[[loc_var]] %in% loc_ids,
      date >= cluster_dates[1],
      date <= cluster_dates[2]
    ) |>
    dd_full_summary(var = var, suppr = suppr)
}

# Assemble the full summary and cluster summaries
assemble_dd_summaries <- function(
  data_details,
  cluster_data,
  var,
  src = c("hospital", "patient"),
  suppr = NULL
) {
  src <- match.arg(src)

  # Return NULL if no data
  if (nrow(data_details) == 0) {
    return(NULL)
  }

  # Summarize data for full population
  smry1 <- dd_full_summary(data_details, var, suppr)

  cluster_ids <- cluster_data$shapeclust$cluster

  ri_levels <- gsub("\\s", "_", cluster_data$shapeclust$ri_level)

  sfx <- paste0(cluster_ids, "_(", ri_levels, ")")

  # Return `smry1` if no clusters were detected
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
      cluster_dates = dates,
      suppr = suppr
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

# Data details output table
dd_table <- function(df, var, color) {
  # Assign RI levels as names to `color`
  color <- setNames(
    color,
    c("very_weak", "weak", "moderate", "strong", "very_strong")
  )

  # Function to rename columns
  mod_col_labels <- function(x) {
    stringr::str_to_sentence(gsub("_", " ", x))
  }

  # Configure column groups: Study area
  col_groups1 <- list(reactable::colGroup(
    name = "Study area",
    columns = c("n", "pct")
  ))

  # Configure column groups: Clusters
  cols <- colnames(df)[grepl("\\d", colnames(df))]

  nm <- sub("^n", "", colnames(df)[grepl("^n\\d", colnames(df))])

  if (length(cols) != 0) {
    col_groups2 <- lapply(nm, \(x) {
      n <- stringr::str_extract(x, "\\d*")

      ri_lvl <- stringr::str_extract(x, "(?<=\\().*(?=\\))")

      bg <- as.character(color[ri_lvl])

      clr <- setmeup::contrast_color(bg)

      reactable::colGroup(
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

  # Format columns: Main variable
  col_defs1 <- list(reactable::colDef(
    name = mod_col_labels(var),
    sticky = "left"
  ))

  names(col_defs1) <- var

  # Format columns: N
  cols <- colnames(df)[grepl("^n\\d*(_|$)", colnames(df))]

  col_defs2 <- lapply(cols, \(x) {
    reactable::colDef(
      name = "N",
      align = "right",
      cell = \(value) ifelse(
        is.numeric(suppressWarnings(as.numeric(value))),
        yes = prettyNum(value, big.mark = ","),
        no = value
      ),
      style = list(borderLeft = "1px solid #ddd")
    )
  })

  names(col_defs2) <- cols

  # Format columns: Pct
  cols <- colnames(df)[grepl("^pct\\d*(_|$)", colnames(df))]

  orange_pal <- function(x) {
    rgb(colorRamp(c("#fff2e0", "#ff9500"))(x), maxColorValue = 255)
  }

  col_defs3 <- lapply(cols, \(x) {
    reactable::colDef(
      name = "Pct",
      align = "right",
      cell = \(value) ifelse(is.na(value), "-", paste0(value, "%")),
      style = \(value) {
        bg <- ifelse(
          is.na(value), "#eee", orange_pal(as.numeric(value / 100))
        )

        clr <- setmeup::contrast_color(bg)

        list(
          background = bg,
          color = clr
        )
      }
    )
  })

  names(col_defs3) <- cols

  col_defs <- c(col_defs1, col_defs2, col_defs3)

  df |>
    reactable::reactable(
      columnGroups = col_groups,
      columns = col_defs,
      sortable = FALSE,
      pagination = FALSE,
      highlight = TRUE,
      compact = TRUE,
      fullWidth = FALSE,
      theme = reactable::reactableTheme(borderColor = "#ddd")
    )
}

