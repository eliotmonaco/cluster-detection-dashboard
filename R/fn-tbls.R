# Functions for syndrome, cluster, and cluster location tables

# Summarize data for significant clusters table
summarize_syndrome_clusters <- function(ls, syndromes, ri_min = 0) {
  # Count clusters by RI level for each syndrome
  ls <- lapply(ls, \(ls2) {
    ct <- lapply(ls2, \(ls3) {
      if (is.data.frame(ls3$shapeclust)) {
        ls3$shapeclust |>
          sf::st_drop_geometry() |>
          dplyr::count(ri_level, .drop = FALSE) |>
          dplyr::mutate(ri_level = gsub("\\s", "_", ri_level)) |>
          tibble::column_to_rownames("ri_level") |>
          t() |>
          as.data.frame()
      } else if (length(ls3) == 0) {
        data.frame(
          very_weak = NA, weak = NA, moderate = NA,
          strong = NA, very_strong = NA
        )
      } else if (is.na(ls3$shapeclust)) {
        data.frame(
          very_weak = 0, weak = 0, moderate = 0,
          strong = 0, very_strong = 0
        )
      }
    })

    purrr::list_rbind(ct, names_to = "abbr")
  })

  # Join patient and hospital dataframes and syndrome names
  df <- ls$patient |>
    dplyr::left_join(
      ls$hospital,
      by = "abbr",
      suffix = c("_pat", "_hosp")
    ) |>
    dplyr::left_join(
      data.frame(
        syndrome = syndromes$name1,
        abbr = syndromes$abbr
      ),
      by = "abbr"
    ) |>
    dplyr::select(syndrome, dplyr::everything(), -abbr)

  # Keep counts above the recurrence interval minimum
  if (ri_min > 1) {
    lvl <- list(
      "very_weak" = 1,
      "weak" = 2,
      "moderate" = 3,
      "strong" = 4,
      "very_strong" = 5
    )

    lvl <- lvl[lvl < ri_min]

    p <- paste(paste0("^", names(lvl)), collapse = "|")

    vars <- colnames(df)[!grepl(p, colnames(df))]

    df[, vars]
  } else {
    df
  }
}

# Table showing the number of clusters detected for each syndrome
cluster_count_table <- function(df, bg_color, text_color) {
  # Function to rename columns
  mod_col_labels <- function(x) {
    x |>
      gsub(pattern = "_pat|_hosp", replacement = "") |>
      gsub(pattern = "_", replacement = " ") |>
      stringr::str_to_title()
  }

  # Number of RI level columns in `df`
  n <- 6 - ((ncol(df) - 1) / 2)

  # Colors
  bg_color <- c(NA, bg_color[n:5], bg_color[n:5])

  text_color <- c(NA, text_color[n:5], text_color[n:5])

  # Style columns
  col_defs <- mapply(colnames(df), bg_color, text_color, FUN = \(nm, bg, txt) {
    if (nm == "syndrome") {
      reactable::colDef(
        name = mod_col_labels(nm),
        minWidth = 150,
        sticky = "left",
        style = list(borderRight = "1px solid #ddd")
      )
    } else {
      reactable::colDef(
        name = mod_col_labels(nm),
        style = function(n) {
          if (!is.na(n) && n > 0) {
            list(
              fontWeight = "bold",
              background = bg,
              color = txt
            )
          }
        }
      )
    }
  })

  names(col_defs) <- colnames(df)

  df |>
    reactable::reactable(
      columnGroups = list(
        reactable::colGroup(
          name = "ER visits by patient location",
          columns = colnames(df)[grepl("_pat$", colnames(df))]
        ),
        reactable::colGroup(
          name = "ER visits by hospital location",
          columns = colnames(df)[grepl("_hosp$", colnames(df))]
        )
      ),
      columns = col_defs,
      pagination = FALSE,
      highlight = TRUE,
      compact = TRUE,
      theme = reactable::reactableTheme(borderColor = "#ddd")
    )
}

# Filter cluster and location data by recurrence interval for a syndrome
filter_cluster_data <- function(ls, ri_min) {
  if (!is.data.frame(ls$shapeclust) || nrow(ls$shapeclust) == 0) {
    ls$gis <- NULL; ls$shapeclust <- NULL; ls$shapegis <- NULL

    return(ls)
  }

  # Filter spatial data by p-value and add labels for map
  lapply(ls[grepl("gis|clust", names(ls))], \(df) {
    df <- df |>
      dplyr::filter(as.numeric(ri_level) >= ri_min) |>
      dplyr::mutate(lbl = paste("Cluster", cluster))

    if ("geometry" %in% colnames(df)) {
      df <- df |>
        dplyr::relocate(geometry, .after = dplyr::everything())
    }

    if (nrow(df) == 0) {
      NULL
    } else {
      df
    }
  })
}

# Point expansion for single location clusters
expand_point_clusters <- function(ls) {
  if (!is.null(ls$shapeclust)) {
    # Find clusters with only 1 location
    clust <- ls$gis$cluster

    clust <- clust[!clust %in% clust[duplicated(clust)]]

    # Expand cluster polygons for visibility on map
    ls$shapeclust <- ls$shapeclust |>
      dplyr::mutate(geometry = dplyr::if_else(
        cluster %in% clust,
        sf::st_buffer(geometry, dist = 2000),
        geometry
      ))
  }

  ls
}

# Modify column labels
mod_col_labels <- function(x) {
  stringr::str_to_sentence(gsub("_", " ", x))
}

# Syndrome table with query names and KR links
syndrome_table <- function(df) {
  df <- df |>
    dplyr::select(name1, esspath, krlink)

  colnames(df) <- c("Syndrome", "ESSENCE path", "krlink")

  make_link <- function(value) {
    if (!is.na(value)) {
      tags$a(href = value, target = "_blank", "KR page")
    }
  }

  df |>
    reactable::reactable(
      columns = list(
        krlink = reactable::colDef(
          name = "NSSP Knowledge Repository link",
          cell = make_link
        )
      ),
      rownames = FALSE,
      pagination = FALSE,
      highlight = TRUE,
      compact = TRUE
    )
}

# Table with cluster data
cluster_table <- function(df, bg_color, text_color) {
  if (is.null(df)) {
    return(NULL)
  }

  # Var name replacements
  replace <- c(
    "Locations" = "Number loc",
    "Test statistic" = "Test stat",
    "P-value" = "P value",
    "RI (days)" = "Recurr int",
    "RI level" = "Ri level",
    "Obs/exp" = "Ode"
  )

  # Configure data
  df <- df |>
    sf::st_drop_geometry() |>
    dplyr::select(
      cluster, start_date, end_date, number_loc, test_stat, p_value,
      recurr_int, ri_level, observed, expected, ode
    ) |>
    dplyr::mutate(
      dplyr::across(
        c(start_date, end_date),
        ~ format(as.Date(.x, "%Y/%m/%d"), "%b %d, %Y")
      ),
      dplyr::across(
        c(test_stat, ode),
        ~ setmeup::round_ties_away(.x, 2)
      ),
      p_value = signif(p_value, 1),
      ri_level = stringr::str_to_sentence(ri_level),
      expected = setmeup::round_ties_away(expected, 0)
    ) |>
    dplyr::rename_with(mod_col_labels) |>
    dplyr::rename(dplyr::any_of(replace))

  # Function to style `ri_level` column
  fn <- function(bg, txt) {
    function(value) {
      if (value == "Very weak") {
        list(background = bg[1], color = txt[1])
      } else if (value == "Weak") {
        list(background = bg[2], color = txt[2])
      } else if (value == "Moderate") {
        list(background = bg[3], color = txt[3])
      } else if (value == "Strong") {
        list(background = bg[4], color = txt[4])
      } else if (value == "Very strong") {
        list(background = bg[5], color = txt[5])
      }
    }
  }

  cell_style <- fn(bg_color, text_color)

  # Style columns
  col_defs <- lapply(colnames(df), \(x) {
    if (x %in% c("P-value", "RI (days)")) {
      # Format as scientific notation
      reactable::colDef(cell = htmlwidgets::JS(
        "function(cellInfo) {
            return cellInfo.value.toExponential(1)
        }"
      ))
    } else if (is.numeric(df[[x]])) {
      # Use comma separators
      reactable::colDef(format = reactable::colFormat(separators = TRUE))
    } else if (x == "RI level") {
      reactable::colDef(style = cell_style)
    }
  })

  names(col_defs) <- colnames(df)

  df |>
    reactable::reactable(
      columns = purrr::compact(col_defs),
      pagination = FALSE,
      highlight = TRUE,
      compact = TRUE,
      selection = "single",
      onClick = "select",
      theme = reactable::reactableTheme(borderColor = "#ddd")
    )
}

# Table with location data for a given cluster
location_table <- function(df, id = NULL, src = c("patient", "hospital")) {
  if (is.null(id)) {
    return(NULL)
  }

  src <- match.arg(src)

  # Var name replacements
  replace <- c(
    "In KC" = "kc",
    "Cluster" = "cluster",
    "Observed" = "loc_obs",
    "Expected" = "loc_exp",
    "Obs/exp" = "loc_ode"
  )

  # Data source-specific adjustments and styling
  if (src == "patient") {
    df <- df |>
      dplyr::mutate(loc_id = as.character(loc_id))

    replace <- c("ZCTA" = "loc_id", replace)

    col_defs <- list(ZCTA = reactable::colDef(minWidth = 200))
  } else if (src == "hospital") {
    df <- df |>
      dplyr::mutate(
        loc_id = gsub("_", " ", loc_id) |>
          stringr::str_to_title() |>
          sub(pattern = "\\sOf\\s", replacement = " of ")
      )

    replace <- c("Hospital" = "loc_id", replace)

    col_defs <- list(Hospital = reactable::colDef(minWidth = 200))
  }

  # Configure data
  df <- df |>
    sf::st_drop_geometry() |>
    dplyr::filter(cluster == id) |>
    dplyr::select(loc_id, kc, cluster, loc_obs, loc_exp, loc_ode) |>
    dplyr::mutate(
      kc = stringr::str_to_sentence(kc),
      loc_exp = setmeup::round_ties_away(loc_exp, 0),
      loc_ode = setmeup::round_ties_away(loc_ode, 2)
    ) |>
    dplyr::arrange(loc_id) |>
    dplyr::rename(dplyr::any_of(replace))

  df |>
    reactable::reactable(
      columns = col_defs,
      pagination = FALSE,
      highlight = TRUE,
      compact = TRUE,
      theme = reactable::reactableTheme(borderColor = "#ddd")
    )
}

# Get the cluster table ID from the map cluster ID (cannot be NULL)
update_cluster_table_id <- function(id) {
  if (is.null(id)) {
    NA
  } else if (is.character(id)) {
    NA
  } else {
    id
  }
}

# Return a heading tag for a syndrome
syndrome_title_tag <- function(x, df) {
  tags$h3(
    df[df$abbr == x, "name1"],
    class = "cluster-tab-title"
  )
}

