# Functions for syndrome, cluster, and cluster location tables

# Summarize data for significant clusters table
summarize_syndrome_clusters <- function(ls, syndromes, ri_min = 0) {
  # Count clusters by strength level for each syndrome
  ls <- lapply(ls, \(ls2) {
    ct <- lapply(ls2, \(ls3) {
      if (is.data.frame(ls3$shapeclust)) {
        ls3$shapeclust |>
          st_drop_geometry() |>
          count(strength, .drop = FALSE) |>
          mutate(strength = gsub("\\s", "_", strength)) |>
          column_to_rownames("strength") |>
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

    list_rbind(ct, names_to = "abbr")
  })

  # Join patient and hospital dataframes and syndrome names
  df <- ls$patient |>
    left_join(
      ls$hospital,
      by = "abbr",
      suffix = c("_pat", "_hosp")
    ) |>
    left_join(
      data.frame(
        syndrome = sapply(syndromes, \(ls) ls$name1),
        abbr = names(syndromes)
      ),
      by = "abbr"
    ) |>
    select(syndrome, everything(), -abbr)

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
cluster_count_table <- function(df, colors) {
  # Function to rename columns
  mod_col_labels <- function(x) {
    x |>
      gsub(pattern = "_pat|_hosp", replacement = "") |>
      gsub(pattern = "_", replacement = " ") |>
      str_to_title()
  }

  # Number of RI strength columns in `df`
  n <- (ncol(df) - 1) / 2

  # Cell background colors
  colors <- colors[(6 - n):5]

  colors <- c(NA, colors, colors)

  # Style columns
  col_defs <- map2(colnames(df), colors, \(x, y) {
    if (x == "syndrome") {
      colDef(
        name = mod_col_labels(x),
        sticky = "left",
        style = list(borderRight = "1px solid #ddd")
      )
    } else {
      colDef(
        name = mod_col_labels(x),
        style = function(n) {
          if (!is.na(n) && n > 0) {
            list(
              fontWeight = "bold",
              background = y
            )
          }
        }
      )
    }
  })

  names(col_defs) <- colnames(df)

  df |>
    reactable(
      columnGroups = list(
        colGroup(
          name = "ER visits by patient location",
          columns = colnames(df)[grepl("_pat$", colnames(df))]
        ),
        colGroup(
          name = "ER visits by hospital location",
          columns = colnames(df)[grepl("_hosp$", colnames(df))]
        )
      ),
      columns = col_defs,
      pagination = FALSE,
      highlight = TRUE,
      compact = TRUE,
      theme = reactableTheme(borderColor = "#ddd")
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
      filter(as.numeric(strength) >= ri_min) |>
      mutate(lbl = paste("Cluster", cluster))

    if ("geometry" %in% colnames(df)) {
      df <- df |>
        relocate(geometry, .after = everything())
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
      mutate(geometry = if_else(
        cluster %in% clust,
        st_buffer(geometry, dist = 2000),
        geometry
      ))
  }

  ls
}

# Modify column labels
mod_col_labels <- function(x) {
  str_to_sentence(gsub("_", " ", x))
}

# Syndrome table with query names and KR links
syndrome_table <- function(ls) {
  df <- data.frame(
    syndrome = sapply(ls, \(ls2) ls2$name1),
    query = sapply(ls, \(ls2) ls2$queryname),
    kr = sapply(ls, \(ls2) ls2$kr)
  )

  colnames(df) <- c("Syndrome", "ESSENCE query", "kr")

  make_link <- function(x) {
    if (x != "") {
      tags$a(href = x, target = "_blank", "KR page")
    }
  }

  df |>
    reactable(
      columns = list(
        kr = colDef(
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
cluster_table <- function(df, colors) {
  if (is.null(df)) {
    return(NULL)
  }

  # Var name replacements
  replace <- c(
    "Locations" = "Number loc",
    "Test statistic" = "Test stat",
    "P-value" = "P value",
    "RI (days)" = "Recurr int",
    "Obs/exp" = "Ode"
  )

  # Configure data
  df <- df |>
    st_drop_geometry() |>
    select(
      cluster, start_date, end_date, number_loc, test_stat, p_value,
      recurr_int, strength, observed, expected, ode
    ) |>
    mutate(
      across(
        c(start_date, end_date),
        ~ format(as.Date(.x, "%Y/%m/%d"), "%b %d, %Y")
      ),
      across(
        c(test_stat, ode),
        ~ round_ties_away(.x, 2)
      ),
      p_value = signif(p_value, 1),
      strength = str_to_sentence(strength),
      expected = round_ties_away(expected, 0)
    ) |>
    rename_with(mod_col_labels) |>
    rename(any_of(replace))

  # Function to style `Strength` column
  fn <- function(clr) {
    function(value) {
      if (value == "Very weak") {
        list(background = clr[1])
      } else if (value == "Weak") {
        list(background = clr[2])
      } else if (value == "Moderate") {
        list(background = clr[3])
      } else if (value == "Strong") {
        list(background = clr[4])
      } else if (value == "Very strong") {
        list(background = clr[5])
      }
    }
  }

  cell_style <- fn(colors)

  # Style columns
  col_defs <- lapply(colnames(df), \(x) {
    if (x %in% c("P-value", "RI (days)")) {
      # Format as scientific notation
      colDef(cell = JS(
        "function(cellInfo) {
            return cellInfo.value.toExponential(1)
        }"
      ))
    } else if (is.numeric(df[[x]])) {
      # Use comma separators
      colDef(format = colFormat(separators = TRUE))
    } else if (x == "Strength") {
      colDef(style = cell_style)
    }
  })

  names(col_defs) <- colnames(df)

  df |>
    reactable(
      columns = compact(col_defs),
      pagination = FALSE,
      highlight = TRUE,
      compact = TRUE,
      selection = "single",
      onClick = "select",
      theme = reactableTheme(borderColor = "#ddd")
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
      mutate(loc_id = as.character(loc_id))

    replace <- c("ZCTA" = "loc_id", replace)

    col_defs <- list(ZCTA = colDef(minWidth = 200))
  } else if (src == "hospital") {
    df <- df |>
      mutate(
        loc_id = gsub("_", " ", loc_id) |>
          str_to_title() |>
          sub(pattern = "\\sOf\\s", replacement = " of ")
      )

    replace <- c("Hospital" = "loc_id", replace)

    col_defs <- list(Hospital = colDef(minWidth = 200))
  }

  # Configure data
  df <- df |>
    st_drop_geometry() |>
    filter(cluster == id) |>
    select(loc_id, kc, cluster, loc_obs, loc_exp, loc_ode) |>
    mutate(
      kc = str_to_sentence(kc),
      loc_exp = round_ties_away(loc_exp, 0),
      loc_ode = round_ties_away(loc_ode, 2)
    ) |>
    arrange(loc_id) |>
    rename(any_of(replace))

  df |>
    reactable(
      columns = col_defs,
      pagination = FALSE,
      highlight = TRUE,
      compact = TRUE,
      theme = reactableTheme(borderColor = "#ddd")
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
syndrome_title_tag <- function(x, ls) {
  tags$h3(names(ls)[which(ls == x)], class = "cluster-tab-title")
}

