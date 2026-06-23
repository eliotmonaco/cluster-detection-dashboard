# Functions for cluster summary, cluster detail, and syndrome info tables

# Filter syndrome summary table by RI minimum
filter_cluster_summary <- function(df, ri_min, compact = FALSE) {
  lvl <- list(
    "very_weak" = 1,
    "weak" = 2,
    "moderate" = 3,
    "strong" = 4,
    "very_strong" = 5
  )

  if (ri_min > 1) {
    lvl <- lvl[lvl < ri_min]

    p <- paste(paste0("^", names(lvl)), collapse = "|")

    vars <- colnames(df)[!grepl(p, colnames(df))]
  } else {
    vars <- colnames(df)
  }

  if (compact) {
    df[, vars] |>
      dplyr::mutate(
        total = rowSums(dplyr::across(dplyr::matches("_pat$|_hosp$")))
      ) |>
      dplyr::filter_out(total == 0) |>
      dplyr::select(-total)
  } else {
    df[, vars]
  }
}

# Table showing the number of clusters detected for each syndrome
cluster_summary_table <- function(df, bg_color, text_color) {
  # Number of RI level columns in `df`
  n <- 6 - sum(grepl("_pat$", colnames(df)))

  # Colors
  bg_color <- c(rep(NA, 2), bg_color[n:5], bg_color[n:5])

  text_color <- c(rep(NA, 2), text_color[n:5], text_color[n:5])

  # Function to rename columns
  mod_col_labels <- function(x) {
    x |>
      gsub(pattern = "_pat|_hosp", replacement = "") |>
      gsub(pattern = "_", replacement = " ") |>
      stringr::str_to_title()
  }

  # Style columns
  col_defs <- mapply(
    colnames(df), bg_color, text_color,
    FUN = \(nm, bg, txt) {
      if (nm == "category") {
        reactable::colDef(
          name = mod_col_labels(nm),
          minWidth = 150,
          maxWidth = 200
        )
      } else if (nm == "syndrome") {
        reactable::colDef(
          name = mod_col_labels(nm),
          minWidth = 160,
          sticky = "left",
          style = list(borderRight = "1px solid #555")
        )
      } else if (nm == "very_strong_pat") {
        reactable::colDef(
          name = mod_col_labels(nm),
          minWidth = 100,
          style = function(value) {
            ls <- list(borderRight = "1px solid #555")
            if (!is.na(value) && value > 0) {
              c(ls, list(fontWeight = "bold", background = bg, color = txt))
            } else {
              ls
            }
          }
        )
      } else {
        reactable::colDef(
          name = mod_col_labels(nm),
          minWidth = 100,
          style = function(value) {
            if (!is.na(value) && value > 0) {
              list(fontWeight = "bold", background = bg, color = txt)
            }
          }
        )
      }
    }
  )

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
      groupBy = "category",
      defaultColDef = reactable::colDef(
        vAlign = "center",
        headerVAlign = "bottom",
        headerClass = "tbl-header"
      ),
      rowStyle = JS( # style row group
        "function(rowInfo) {
          if (rowInfo.level == 0) return {background: '#EEE'}
        }"
      ),
      defaultExpanded = TRUE,
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

# Syndrome table with query names and KR links
syndrome_table <- function(df) {
  df <- df |>
    dplyr::select(category, name1, esspath, krlink)

  make_link <- function(value) {
    if (!is.na(value)) {
      shiny::tags$a(href = value, target = "_blank", "KR page")
    }
  }

  df |>
    reactable::reactable(
      columns = list(
        category = reactable::colDef(
          name = "Category",
          minWidth = 150,
          maxWidth = 200
        ),
        name1 = reactable::colDef(
          name = "Syndrome",
          minWidth = 160
        ),
        esspath = reactable::colDef(
          name = "ESSENCE query",
          minWidth = 300
        ),
        krlink = reactable::colDef(
          name = "NSSP Knowledge Repository link",
          cell = make_link
        )
      ),
      groupBy = "category",
      defaultColDef = reactable::colDef(
        vAlign = "center",
        headerVAlign = "bottom",
        headerClass = "tbl-header"
      ),
      rowStyle = JS( # style row group
        "function(rowInfo) {
          if (rowInfo.level == 0) return {background: '#EEE'}
        }"
      ),
      defaultExpanded = TRUE,
      rownames = FALSE,
      pagination = FALSE,
      highlight = TRUE,
      compact = TRUE
    )
}

# Table with cluster data
# suppr = the suppression level (counts below this number will be suppressed)
cluster_table <- function(df, bg_color, text_color, suppr = NULL) {
  if (is.null(df)) {
    return(NULL)
  }

  # Var name replacements
  vars <- c(
    "Cluster" = "cluster",
    "Start date" = "start_date", "End date" = "end_date",
    "Locations" = "number_loc",
    "Test statistic" = "test_stat", "P-value" = "p_value",
    "RI (days)" = "recurr_int", "RI level" = "ri_level",
    "Observed" = "observed", "Expected" = "expected", "Obs/exp" = "ode"
  )

  # Configure data
  df <- df |>
    sf::st_drop_geometry() |>
    dplyr::select(dplyr::all_of(unname(vars))) |>
    dplyr::mutate(
      dplyr::across(
        c(start_date, end_date),
        ~ format(as.Date(.x, "%Y/%m/%d"), "%b %d, %Y")
      ),
      dplyr::across(
        c(test_stat, expected, ode),
        ~ setmeup::round_ties_away(.x, 2)
      ),
      dplyr::across(
        c(p_value, recurr_int),
        ~ prettyNum(signif(.x, 2), scientific = TRUE)
      ),
      ri_level = stringr::str_to_sentence(ri_level)
    )

  if (!is.null(suppr)) {
    # Suppress values in `expected`
    df <- suppress_df(
      df,
      var = "observed",
      n = suppr,
      var_suppr = "expected",
      sep = TRUE
    )

    # Suppress counts in `observed`
    df <- suppress_df(
      df,
      var = "observed",
      n = suppr,
      sep = TRUE
    )
  }

  df <- df |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.numeric),
      ~ prettyNum(.x, big.mark = ",")
    ))

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

  ri_cell_style <- fn(bg_color, text_color)

  # Style columns
  col_defs <- lapply(colnames(df), \(x) {
    if (x == "ri_level") {
      reactable::colDef(
        name = names(vars)[vars == x],
        minWidth = 80,
        style = ri_cell_style
      )
    } else if (x %in% c(
      "number_loc", "test_stat", "p_value", "recurr_int",
      "observed", "expected", "ode"
    )) {
      reactable::colDef(
        name = names(vars)[vars == x],
        minWidth = 80,
        align = "right"
      )
    } else {
      reactable::colDef(
        name = names(vars)[vars == x],
        minWidth = 80
      )
    }
  })

  names(col_defs) <- colnames(df)

  df |>
    reactable::reactable(
      columns = col_defs,
      defaultColDef = reactable::colDef(
        vAlign = "center",
        headerVAlign = "bottom",
        headerClass = "tbl-header"
      ),
      pagination = FALSE,
      highlight = TRUE,
      compact = TRUE,
      selection = "single",
      onClick = "select",
      theme = reactable::reactableTheme(borderColor = "#ddd")
    )
}

# Table with location data for a given cluster
# suppr = the suppression level (counts below this number will be suppressed)
location_table <- function(
    df, id = NULL, src = c("patient", "hospital"), suppr = NULL
) {
  if (is.null(id)) {
    return(NULL)
  }

  src <- match.arg(src)

  # Var name replacements
  vars <- c(
    "In KC" = "kc",
    "Cluster" = "cluster",
    "Observed" = "loc_obs",
    "Expected" = "loc_exp",
    "Obs/exp" = "loc_ode"
  )

  # Data source-specific config
  if (src == "patient") {
    df <- df |>
      dplyr::mutate(loc_id = as.character(loc_id))

    vars <- c("ZCTA" = "loc_id", vars)
  } else if (src == "hospital") {
    df <- df |>
      dplyr::mutate(
        loc_id = gsub("_", " ", loc_id) |>
          stringr::str_to_title() |>
          sub(pattern = "\\sOf\\s", replacement = " of ")
      )

    vars <- c("Hospital" = "loc_id", vars)
  }

  # Configure data
  df <- df |>
    sf::st_drop_geometry() |>
    dplyr::filter(cluster == id) |>
    dplyr::select(loc_id, kc, loc_obs, loc_exp, loc_ode) |>
    dplyr::mutate(
      kc = stringr::str_to_sentence(kc),
      dplyr::across(
        c(loc_exp, loc_ode),
        ~ setmeup::round_ties_away(.x, 2)
      )
    )

  if (!is.null(suppr)) {
    # Suppress values in `loc_exp`
    df <- suppress_df(
      df,
      var = "loc_obs",
      n = suppr,
      var_suppr = "loc_exp",
      sep = TRUE,
      sec = TRUE
    )

    # Suppress counts in `loc_obs`
    df <- suppress_df(
      df,
      var = "loc_obs",
      n = suppr,
      sep = TRUE,
      sec = TRUE
    )
  }

  df <- df |>
    dplyr::mutate(dplyr::across(
      dplyr::where(is.numeric),
      ~ prettyNum(.x, big.mark = ",")
    )) |>
    dplyr::arrange(loc_id)

  # Style columns
  col_defs <- lapply(colnames(df), \(x) {
    if (x %in% c("loc_obs", "loc_exp", "loc_ode")) {
      reactable::colDef(
        name = names(vars)[vars == x],
        minWidth = 80,
        align = "right"
      )
    } else {
      reactable::colDef(
        name = names(vars)[vars == x],
        minWidth = 80
      )
    }
  })

  names(col_defs) <- colnames(df)

  if (src == "hospital") {
    col_defs$loc_id <- reactable::colDef(
      name = "Hospital",
      minWidth = 120
    )
  }

  df |>
    reactable::reactable(
      columns = col_defs,
      defaultColDef = reactable::colDef(
        vAlign = "center",
        headerVAlign = "bottom",
        headerClass = "tbl-header"
      ),
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

# Add footnote to reactable output in UI using conditionalPanel()
# suppr = the suppression level (counts below this number will be suppressed)
tbl_suppr_footnote <- function(id, output_name, suppr) {
  if (is.null(suppr)) return(invisible(NULL))

  conditionalPanel(
    condition = paste0("output.", output_name),
    div(
      paste0(
        "When observed counts are between 0 and ", suppr, ", observed and ",
        "expected values are suppressed (indicated by \"*\")."
      ),
      class = "reactable-footnote"
    ),
    ns = NS(id)
  )
}

