# Functions for syndrome, cluster, and cluster location tables

# Summarize data for significant clusters table
significant_clusters_by_syndrome <- function(ls, syndromes) {
  ls <- lapply(ls, \(ls2) {
    x <- sapply(ls2, \(ls3) {
      if (is.data.frame(ls3$shapeclust)) {
        ls3$shapeclust |>
          st_drop_geometry() |>
          filter(p_value < .05) |>
          nrow()
      } else if (length(ls3) == 0) {
        NA
      } else if (is.na(ls3$shapeclust)) {
        0
      }
    })

    data.frame(
      abbr = names(x),
      clusters = x
    )
  })

  df <- ls$patient |>
    rename(clust_pat = clusters) |>
    bind_cols(
      ls$hospital |>
        select(clust_hosp = clusters)
    ) |>
    left_join(
      data.frame(
        syndrome = sapply(syndromes, \(ls) ls$name1),
        abbr = names(syndromes)
      ),
      by = "abbr"
    ) |>
    select(syndrome, clust_pat, clust_hosp)

  df
}

# Filter cluster and location data by p-value for a syndrome
filter_cluster_data <- function(ls, sig_pval) {
  if (!is.data.frame(ls$shapeclust) || nrow(ls$shapeclust) == 0) {
    ls$gis <- NULL; ls$shapeclust <- NULL; ls$shapegis <- NULL

    return(ls)
  }

  if (sig_pval) {
    plvl <- .05
  } else {
    plvl <- 1.1
  }

  # Filter spatial data by p-value and add labels for map
  lapply(ls[grepl("gis|clust", names(ls))], \(df) {
    df <- df |>
      filter(p_value < plvl) |>
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

config_syndrome_data <- function(ls, syndrome, sig_pval) {
  ls <- lapply(
    list(
      patient = ls$patient[[syndrome]],
      hospital = ls$hospital[[syndrome]]
    ),
    filter_cluster_data,
    sig_pval = sig_pval
  )

  if (!is.null(ls$hospital$shapeclust)) {
    # Find clusters with only 1 location
    clust <- ls$hospital$gis$cluster

    clust <- clust[!clust %in% clust[duplicated(clust)]]

    # Expand cluster polygons for visibility on map
    ls$hospital$shapeclust <- ls$hospital$shapeclust |>
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

# Table showing the number of clusters detected for each syndrome
clustcount_table <- function(df) {
  bold_text <- function(x) {
    if (!is.na(x) && x > 0) {
      list(fontWeight = "bold")
    }
  }

  pink_bg <- function(r) {
    x <- df[r, "clust_pat"]
    y <- df[r, "clust_hosp"]

    if ((!is.na(x) && x > 0) | (!is.na(y) && y > 0)) {
      list(background = "#fcc7c7")
    }
  }

  df |>
    reactable(
      columns = list(
        syndrome = colDef(
          name = "Syndrome"
        ),
        clust_pat = colDef(
          name = "ER visits by patient location",
          style = bold_text,
          na = "-"
        ),
        clust_hosp = colDef(
          name = "ER visits by hospital location",
          style = bold_text,
          na = "-"
        )
      ),
      columnGroups = list(
        colGroup(
          name = "Number of clusters",
          columns = c("clust_pat", "clust_hosp")
        )
      ),
      rowStyle = pink_bg,
      pagination = FALSE,
      highlight = TRUE,
      compact = TRUE
    )
}

# Table with cluster data
cluster_table <- function(df) {
  if (is.null(df)) {
    return(NULL)
  }

  replace <- c(
    "Locations" = "Number loc",
    "Test statistic" = "Test stat",
    "P-value" = "P value",
    "RI (days)" = "Recurr int",
    "Obs/exp" = "Ode"
  )

  df <- df |>
    st_drop_geometry() |>
    select(
      cluster, start_date, end_date, number_loc, test_stat, p_value,
      recurr_int, observed, expected, ode
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
      expected = round_ties_away(expected, 0)
    ) |>
    rename_with(mod_col_labels) |>
    rename(any_of(replace))

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
      onClick = "select"
    )
}

# Table with location data for a given cluster
location_table <- function(df, id = NULL, type = c("patient", "hospital")) {
  if (is.null(id)) {
    return(NULL)
  }

  type <- match.arg(type)

  replace <- c(
    "In KC" = "kc",
    "Cluster" = "cluster",
    "Observed" = "loc_obs",
    "Expected" = "loc_exp",
    "Obs/exp" = "loc_ode"
  )

  if (type == "patient") {
    df <- df |>
      mutate(loc_id = as.character(loc_id))

    replace <- c("ZCTA" = "loc_id", replace)

    col_defs <- list(ZCTA = colDef(minWidth = 200))
  } else if (type == "hospital") {
    df <- df |>
      mutate(
        loc_id = gsub("_", " ", loc_id) |>
          str_to_title() |>
          sub(pattern = "\\sOf\\s", replacement = " of ")
      )

    replace <- c("Hospital" = "loc_id", replace)

    col_defs <- list(Hospital = colDef(minWidth = 200))
  }

  df |>
    st_drop_geometry() |>
    filter(cluster == id) |>
    select(loc_id, kc, cluster, loc_obs, loc_exp, loc_ode) |>
    mutate(
      kc = str_to_sentence(kc),
      loc_exp = round_ties_away(loc_exp, 0),
      loc_ode = round_ties_away(loc_ode, 2)
    ) |>
    arrange(loc_id) |>
    rename(any_of(replace)) |>
    reactable(
      columns = col_defs,
      pagination = FALSE,
      highlight = TRUE,
      compact = TRUE
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

