# SHINY DATA CONFIG -------------------------------------------------------

# Get data and analysis results for one date from dashboard data
get_db_data <- function(ls, date, name = NULL) {
  ls <- ls[[names(ls)[grepl(gsub("-", "", date), names(ls))]]]

  x <- names(ls)

  # Replace list names
  names(ls) <- replace(
    names(ls),
    list = c(
      which(x == "syndromes"), which(x == "date_range"),
      which(x == "time_series"), which(x == "data_details"),
      which(x == "data_details_error"), which(x == "satscan_results")
    ),
    values = c("syn", "daterng", "ts", "dd", "dderr", "ss")
  )

  if (!is.null(name)) {
    ls[[name]]
  } else {
    ls
  }
}

# Create list options for syndrome select input
syn_select_list <- function(ls) {
  ls2 <- as.list(names(ls))

  names(ls2) <- lapply(ls, \(x) x$name1)

  ls2
}

# Create radio button options for date range input
daterange_select_list <- function(dates) {
  x <- max(dates)

  list(
    "Two weeks" = as.character(x - 14),
    "30 days" = as.character(x - 30),
    "90 days" = as.character(x - 90),
    "180 days" = as.character(x - 180),
    "One year" = as.character(x - 365)
  )
}

# Configure data for Highchart function & data characteristics tables
filter_ess <- function(df, start, end = NULL) {
  if (is.null(end)) {
    end <- max(df$date)
  }

  df |>
    dplyr::filter(
      date >= start,
      date <= end
    )
}

df_to_hc_list <- function(df) {
  list(
    list(
      data = lapply(1:nrow(df), \(r) {
        list(
          x = highcharter::datetime_to_timestamp(df[r, "date"]),
          y = df[r, "count"],
          color = df[r, "alert_fill"],
          marker = list(
            symbol = df[r, "alert_symbol"],
            radius = df[r, "alert_radius"],
            lineWidth = df[r, "alert_line"],
            lineColor = df[r, "alert_color"]
          ),
          alert_status = df[r, "alert_status"]
        )
      })
    )
  )
}

config_ts_plot_data <- function(ls, syndrome, daterange) {
  lapply(ls, \(ls2) {
    ls2[[syndrome]] |>
      filter_ess(start = as.Date(daterange)) |>
      df_to_hc_list()
  })
}

config_dd_table_data <- function(ls, syndrome, daterange) {
  lapply(ls, \(ls2) {
    ls2[[syndrome]] |>
      filter_ess(start = as.Date(daterange))
  })
}

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

  lapply(ls[grepl("gis|clust", names(ls))], \(df) {
    df <- df |>
      filter(p_value < plvl) |>
      mutate(lbl = paste("Cluster", cluster))

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

  if (!is.null(ls$hospital$shapegis)) {
    # Find clusters with > 1 location and remove from `ls$hospital$shapegis` so
    # that only single point clusters in this dataset are mapped
    clusters <- ls$hospital$gis$cluster

    clusters <- unique(clusters[duplicated(clusters)])

    ls$hospital$shapegis <- ls$hospital$shapegis |>
      filter(!cluster %in% clusters)
  }

  ls
}

# Filter location geometries by cluster
filter_location_geometries <- function(ls, geo, var) {
  clust <- ls$shapeclust # contains clusters
  loc <- ls$gis # contains locations within each cluster

  if (!is.data.frame(clust) || nrow(clust) == 0) {
    return(NULL)
  }

  sf <- lapply(clust$cluster, \(x) {
    sfc <- geo |>
      filter(.data[[var]] %in% loc$loc_id[loc$cluster == x]) |>
      st_combine()

    st_set_geometry(data.frame(cluster = x), sfc)
  })

  sf <- do.call(rbind, sf)

  # Add cluster label for map
  sf |>
    mutate(lbl = paste("Cluster", cluster))
}

# SHINY UI ----------------------------------------------------------------

select_input_syndrome <- function(input_id, ls) {
  selectInput(
    inputId = input_id,
    label = "Syndrome",
    choices = ls,
    multiple = FALSE,
    selected = ls[[1]]
  )
}

date_input_analysis <- function(input_id, dates) {
  dateInput(
    inputId = input_id,
    label = "Analysis date",
    value = max(dates),
    min = min(dates),
    max = max(dates)
  )
}

radio_buttons_daterange <- function(input_id, ls) {
  radioButtons(
    inputId = input_id,
    label = "Date range",
    choices = ls,
    selected = ls[[2]]
  )
}

syndrome_title_tag <- function(x, ls) {
  tags$h3(names(ls)[which(ls == x)], class = "cluster-tab-title")
}

card_dc <- function(...) {
  card(
    ...,
    min_height = "300px",
    max_height = "400px"
  )
}

# PLOTS -------------------------------------------------------------------

# Time series plot
ts_plot <- function(ls, title) {
  highchart() |>
    hc_add_series_list(ls) |>
    hc_xAxis(
      type = "datetime",
      title = list(text = "Date"),
      labels = list(format = "{value:%b %d}")
    ) |>
    hc_yAxis(
      title = list(text = "Count")
    ) |>
    hc_legend(enabled = FALSE) |>
    hc_tooltip(formatter = JS(
      "function() {
        const dt = new Date(this.x);
        return dt.toDateString() + '<br>' +
        `Count: <b>${this.y}</b>` + '<br>' +
        `Alert status: <b>${this.point.alert_status}</b>`;
      }"
    )) |>
    hc_title(text = title)
}

# Create a custom leaflet legend to add to the `html` arg in `addControl()`
custom_legend_row <- function(ls) {
  if ("class" %in% names(ls)) {
    icon <- paste0(
      "    <div class = '", ls$class, "'>",
      "</div>\n"
    )
  } else {
    icon <- paste0(
      "    <div style = '",
      "background: ", setmeup::color_to_css_rgba(ls$fill, ls$opac2), "; ",
      "width:20px; height:20px; ",
      "border: ", ls$wt, "px solid ",
      setmeup::color_to_css_rgba(ls$clr, ls$opac1), "; ",
      "border-radius: ", switch(ls$shp, square = "0%;", circle = "50%;"), "'>",
      "</div>\n"
    )
  }

  label <- paste0(
    "    <div style = 'padding-left: 5px;'>",
    ls$name,
    "</div>\n"
  )

  paste0(
    "  <div style = 'display: flex; align-items: center; margin: 1px 0;'>\n",
    icon,
    label,
    "  </div>\n"
  )
}

custom_legend_combine <- function(ls) {
  paste0(
    "<div style = 'line-height: 0px;'>\n",
    paste(ls, collapse = "  <br>\n"),
    "</div>"
  )
}

# Leaflet map showing study area and syndrome clusters using Satscan output
cluster_map <- function(
    cluster_locations,
    cluster_points = NULL,
    location_boundaries,
    kc_boundary,
    hospital_locations = NULL,
    gp,
    zoom_level
) {
  # Map center point
  center <- st_coordinates(st_centroid(st_union(location_boundaries))) |>
    as.data.frame()

  legend_rows <- lapply(gp, custom_legend_row)

  map <- leaflet(
    options = leafletOptions(scrollWheelZoom = FALSE)
  ) |>
    setView(lng = center$X, lat = center$Y, zoom = zoom_level) |>
    addProviderTiles("CartoDB.Positron") |>
    addPolygons(
      data = location_boundaries,
      weight = gp$study$wt,
      color = gp$study$clr,
      opacity = gp$study$opac1,
      fillColor = gp$study$fill,
      fillOpacity = gp$study$opac2
    ) |>
    addPolygons(
      data = kc_boundary,
      weight = gp$kc$wt,
      color = gp$kc$clr,
      opacity = gp$kc$opac1,
      fillColor = gp$kc$fill,
      fillOpacity = gp$kc$opac2
    )

  # Add cluster regions
  if (!is.null(cluster_locations)) {
    map <- map |>
      addPolygons(
        data = cluster_locations,
        layerId = ~cluster,
        weight = gp$clust$wt,
        color = gp$clust$clr,
        opacity = gp$clust$opac1,
        fillColor = gp$clust$fill,
        fillOpacity = gp$clust$opac2,
        label = ~lbl,
        highlightOptions = highlightOptions(
          opacity = 1,
          fillOpacity = .5
        )
      )
  } else {
    legend_rows <- legend_rows[-which(names(legend_rows) == "clust")]
  }

  # Add hospital locations
  if (!is.null(hospital_locations)) {
    hospicon <- makeIcon(
      iconUrl = "www/img/transparent-square.svg",
      iconWidth = 12,
      iconHeight = 12,
      className = "plus"
    )

    map <- map |>
      addMarkers(
        data = hospital_locations,
        icon = hospicon,
        label = ~hospital_name
      )
  }

  # Add cluster points
  if (!is.null(cluster_points) && nrow(cluster_points) != 0) {
    map <- map |>
      addCircleMarkers(
        data = cluster_points,
        radius = 10,
        layerId = ~cluster,
        weight = gp$clust$wt,
        color = gp$clust$clr,
        opacity = gp$clust$opac1,
        fillColor = gp$clust$fill,
        fillOpacity = gp$clust$opac2,
        label = ~lbl
      )
  }

  # Add legend
  legend_html <- custom_legend_combine(legend_rows)

  map |>
    addControl(
      html = legend_html,
      position = "bottomright"
    )
}

# TABLES ------------------------------------------------------------------

# Modify column labels
mod_col_labels <- function(x) {
  str_to_sentence(gsub("_", " ", x))
}

dd_table <- function(df, var, replace_nm = NULL) {
  df <- df |>
    count(.data[[var]])

  if (!is.null(replace_nm)) {
    df <- df |>
      rename(any_of(setNames(var, replace_nm)))
  }

  df |>
    rename_with(mod_col_labels) |>
    reactable(
      columns = list(
        "N" = colDef(format = colFormat(separators = TRUE))
      ),
      sortable = FALSE,
      pagination = FALSE
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
        syndrome = colDef(name = "Syndrome"),
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
      pagination = FALSE
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
      mutate(loc_id = as.numeric(as.character(loc_id)))

    replace <- c(
      "ZCTA" = "loc_id",
      replace
    )
  } else if (type == "hospital") {
    df <- df |>
      mutate(
        loc_id = gsub("_", " ", loc_id) |>
          str_to_title() |>
          sub(pattern = "\\sOf\\s", replacement = " of ")
      )

    replace <- c(
      "Hospital" = "loc_id",
      replace
    )
  }

  df |>
    st_drop_geometry() |>
    filter(cluster == id) |>
    select(loc_id, kc, cluster, loc_obs, loc_exp, loc_ode) |>
    mutate(
      loc_exp = round_ties_away(loc_exp, 0),
      loc_ode = round_ties_away(loc_ode, 2)
    ) |>
    arrange(loc_id) |>
    rename(any_of(replace)) |>
    reactable(
      pagination = FALSE
    )
}

