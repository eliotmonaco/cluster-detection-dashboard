# ESSENCE DATA ------------------------------------------------------------

# Build URL for Essence API
build_ess_url <- function(
    syndrome, # list of syndromes built as in `syndromes.R`
    start, # start date (YYYY-MM-DD)
    end = Sys.Date(), # end date (YYYY-MM-DD)
    data_source = c("hospital", "patient"),
    output = c("dd", "ts"),
    dd_fields = NULL,
    zipcodes = NULL
) {
  data_source <- match.arg(data_source)
  output <- match.arg(output)

  start <- as.Date(start); end <- as.Date(end)

  if (is.na(start) | is.na(end)) {
    stop("`start` and `end` must be valid dates formatted YYYY-MM-DD")
  }

  # Output parameters
  if (output == "dd") {
    if (is.null(dd_fields)) {
      params_out <- "aqtTarget=DataDetails"
    } else {
      dd_fields <- URLencode(dd_fields)

      dd_fields <- paste0("field=", c(dd_fields, "EssenceID"))

      params_out <- paste(
        c("aqtTarget=DataDetails", dd_fields),
        collapse = "&"
      )
    }

    op <- "dataDetails/csv?"
  } else if (output == "ts") {
    op <- "timeSeries?"

    params_out <- "aqtTarget=TimeSeries"
  }

  # Data source & geography parameters
  if (data_source == "hospital") {
    params_ds <- "datasource=va_hosp"

    params_geo <- paste(
      "geographySystem=hospitalregion",
      "geography=mo_cass",
      "geography=mo_clay",
      "geography=mo_jackson",
      "geography=mo_platte",
      sep = "&"
    )
  } else if (data_source == "patient") {
    params_ds <- "datasource=va_er"

    params_geo <- paste(
      "geographySystem=zipcode",
      paste0("geography=", paste(zipcodes, collapse = ",")),
      sep = "&"
    )
  }

  # Additional parameters
  params_add <- paste(
    "userId=5809",
    paste0("startDate=", format(start, "%d%b%Y")),
    paste0("endDate=", format(end, "%d%b%Y")),
    "percentParam=noPercent",
    "detector=probrepswitch",
    "timeResolution=daily",
    "hasBeenE=1",
    sep = "&"
  )

  params <- paste(
    params_out,
    params_ds,
    params_geo,
    params_add,
    sep = "&"
  )

  endpoint <- "https://moessence.inductivehealth.com/ih_essence/api/"

  paste0(
    endpoint,
    op,
    paste0(params, "&", syndrome)
  )
}

get_start_date <- function(end_date) {
  end_date - lubridate::years(1)
}

# Wrapper for `Rnssp::get_api_data()` to pull data details
get_ess_dd <- function(url, repair_colnames = TRUE) {
  Rnssp::get_api_data(
    url,
    fromCSV = TRUE,
    col_types = readr::cols(.default = "c"),
    name_repair = ifelse(repair_colnames, setmeup::fix_colnames, "unique")
  )
}

# Wrapper for `Rnssp::get_api_data()` to pull time series
get_ess_ts <- function(url) {
  ls <- Rnssp::get_api_data(url)

  df <- ls$timeSeriesData

  colnames(df) <- setmeup::fix_colnames(colnames(df))

  df$date <- as.Date(df$date)

  df
}

# Capture both the returned value and the message or error from a function call
capture_message <- function(expr) {
  output <- list()

  withCallingHandlers(
    expr,
    warning = function(w) {
      output <<- append(output, list(message = conditionMessage(w)))
    },
    message = function(m) {
      output <<- append(output, list(message = conditionMessage(m)))
    }
  )

  output <- append(output, list(data = expr), after = 0)

  # Remove the "no encoding supplied" message
  if ("message" %in% names(output)) {
    i <- which(names(output) == "message")

    p <- "No encoding supplied: defaulting to UTF-8."

    m <- output[i][!grepl(p, output[i])]

    m <- paste(m, collapse = " | ")

    output <- output[-i]

    output$message <- m
  }

  output
}

# Deduplicate data details data
deduplicate_dd <- function(df, geo_var) {
  df <- df |>
    dplyr::mutate(row_id = dplyr::row_number(), .before = 1)

  # Find "exact" duplicates
  dupes <- suppressMessages(setmeup::find_dupes(df, c(
    "date", "time", "age", "sex", "date_of_birth",
    "zip_code", "hospital_name", "visit_number"
  )))

  # Early return if no dupes are found
  if (is.null(dupes)) {
    return(list(
      data = dplyr::select(df, -row_id),
      error_rate = NULL
    ))
  }

  # Remove redundant dupes
  df <- df |>
    dplyr::anti_join(
      dupes |>
        dplyr::distinct(dupe_id, .keep_all = TRUE),
      by = "row_id"
    )

  # Deduplicate using `visit_number`
  dupes <- suppressMessages(setmeup::find_dupes(df, "visit_number"))

  # Early return if no dupes are found
  if (is.null(dupes)) {
    return(list(
      data = dplyr::select(df, -row_id),
      error_rate = NULL
    ))
  }

  # If members of a dupe set have the same `patient_id`, `date`, and geography
  # variable, all but one are redundant. Only one from each set will be kept in
  # `df`.
  ls <- lapply(unique(dupes$dupe_id), \(x) {
    dupeset <- dupes |> # filter a single dupe set
      dplyr::filter(dupe_id == x)

    if (all(
      length(dupeset$patient_id) == 1,
      length(dupeset$date) == 1,
      length(dupeset[[geo_var]]) == 1
    )) {
      dupeset[2:nrow(dupeset), ] # redundant dupes to remove
    }
  }) |>
    purrr::compact()

  # Remove redundant dupes
  if (length(ls) > 0) {
    df <- df |> # remove redundant dupes from `df`
      dplyr::anti_join(ls, by = "row_id")

    dupes <- dupes |> # remove redundant dupe sets from `dupes`
      dplyr::filter(!dupe_id %in% unique(ls$dupe_id))
  }

  # Among the remaining dupe sets, if they are indeed duplicates, it isn't
  # possible to determine which records have the correct date or ZIP code, both
  # of which are needed for the spatiotemporal analysis. Therefore, all records
  # will be kept. The potential error rate for both date and the geography
  # variable will be noted.

  # Find the number of duplicate sets with different values for each variable
  diffs <- lapply(unique(dupes$dupe_id), \(x) {
    dupeset <- dupes |> # filter a single dupe set
      dplyr::filter(dupe_id == x)

    apply(dupeset, 2, \(c) { # find the number of unique values for each var
      length(unique(c))
    }) |>
      as.list() |>
      data.frame() |>
      dplyr::mutate(
        n_dupes = nrow(dupeset),
        n_potential_errors = n_dupes - 1 # count the number of potential errors
      )                                  # presuming that one value (date or
  }) |>                                  # the geography variable) is the
    purrr::list_rbind()                  # correct one

  # Replace values > 1 with the number of potential errors. If the value is 1,
  # no potential errors are counted because the single value is presumed
  # correct.
  errors <- diffs |>
    dplyr::mutate(dplyr::across(
      -c(n_dupes, n_potential_errors),
      ~ ifelse(.x > 1, n_potential_errors, NA)
    )) |>
    dplyr::select(-c(row_id, dupe_id, n_dupes, n_potential_errors))

  # Sum the potential errors for each variable
  errors <- colSums(errors, na.rm = TRUE) |>
    dplyr::as_tibble(rownames = "var") |>
    dplyr::rename(n = value) |>
    dplyr::mutate(error_rate = n / nrow(df))

  list(
    data = dplyr::select(df, -row_id),
    error_rate = errors
  )
}

# Configure data details data
config_dd <- function(df, ansi_codes) {
  agecat <- c(
    "00-04" = "0-4", "05-17" = "5-17", "18-44" = "18-44",
    "45-64" = "45-64", "65-1000" = "65+", "Unknown" = "Unknown"
  )

  sexcat <- c("F" = "Female", "M" = "Male", "U" = "Unknown/Other")

  df <- df |>
    dplyr::mutate(
      date = as.Date(date, "%m/%d/%Y"),
      hospital_name = hospital_name |>
        stringr::str_to_title() |>
        gsub(pattern = "\\sOf\\s", replacement = " of "),
      hospital_name_geo = gsub("\\s", "_", hospital_name),
      age_group = unname(agecat[age_group]),
      age_group = dplyr::if_else(age_group %in% agecat, age_group, agecat[6]),
      age_group = factor(age_group, agecat),
      sex = unname(sexcat[sex]),
      sex = dplyr::if_else(sex %in% sexcat, sex, sexcat[3]),
      sex = factor(sex, sexcat),
      patient_state2 = unname(ansi_codes[patient_state]),
      patient_state = dplyr::if_else(
        grepl("[[:alpha:]]", patient_state),
        patient_state,
        patient_state2
      )
    ) |>
    dplyr::select(-patient_state2)
}

# Configure time series data
config_ts <- function(df) {
  # Add alert status, color, symbol, and radius
  lvl <- c("Normal", "Warning", "Anomaly")
  fill <- c("#0703fc", "#f2c00a", "#ff0000")
  clr <- c("#04029e", "#a17f03", "#a30202")
  shp <- c("circle", "diamond", "triangle")

  df <- df |>
    dplyr::mutate(
      alert_status = dplyr::case_when(
        color_id == 0 ~ lvl[1],
        color_id == 1 ~ lvl[1],
        color_id == 2 ~ lvl[2],
        color_id == 3 ~ lvl[3]
      ),
      alert_status = factor(alert_status, levels = lvl),
      alert_fill = fill[alert_status],
      alert_color = clr[alert_status],
      alert_symbol = shp[alert_status],
      alert_radius = 5,
      alert_line = 1
    )
}

# SPATIAL DATA ------------------------------------------------------------

get_centroids <- function(sf, id_var) {
  sf |>
    sf::st_centroid() |>
    dplyr::bind_cols(
      sf |>
        sf::st_centroid() |>
        sf::st_coordinates()
    ) |>
    dplyr::select(
      dplyr::all_of(id_var),
      long = X,
      lat = Y
    )
}

# SATSCAN -----------------------------------------------------------------

config_casefile <- function(df, var) {
  if (nrow(df) == 0) {
    maxdate <- NA
  } else {
    maxdate <- max(df$date)
  }

  df |>
    dplyr::filter(date < maxdate) |> # most recent date with complete data
    dplyr::count(.data[[var]], date) |>
    dplyr::select(dplyr::all_of(var), n, date)
}

set_ss_opts <- function(casefile, coordfile, start, end) {
  rsatscan::ss.options(list(
    # Input
    CaseFile = casefile,
    PrecisionCaseTimes = 3, # day
    StartDate = start,
    EndDate = end,
    CoordinatesFile = coordfile,
    CoordinatesType = 1, # lat/long

    # Analysis
    AnalysisType = 4, # prospective spacetime
    ModelType = 2, # spacetime permutation
    ScanAreas = 1, # high rates
    TimeAggregationUnits = 3, # day

    # Output
    OutputGoogleEarthKML = "n",
    OutputShapefiles = "y",
    OutputCartesianGraph = "n",
    MostLikelyClusterEachCentroidDBase = "n",

    # Data Checking
    StudyPeriodCheckType = 1, # relaxed bounds
    GeographicalCoordinatesCheckType = 1, # relaxed coordinates

    # Spatial Window
    MaxSpatialSizeInPopulationAtRisk = 50,

    # Temporal window
    MinimumTemporalClusterSize = 2, # 2 days
    MaxTemporalSizeInterpretation = 1, # interpret as time
    MaxTemporalSize = 30, # 30 days

    # Space and Time Adjustments
    AdjustForWeeklyTrends = "y",

    # Inference
    MonteCarloReps = 999,

    # Miscellaneous Analysis
    ProspectiveFrequencyType = 1, # daily

    # Spatial Output
    LaunchMapViewer = "n",
    CompressKMLtoKMZ = "n",
    IncludeClusterLocationsKML = "n",
    ReportHierarchicalClusters = "y",
    CriteriaForReportingSecondaryClusters = 1, # NoCentersInOther

    # Temporal output
    OutputTemporalGraphHTML = "y",
    TemporalGraphReportType = 2, # report only significant clusters
    TemporalGraphSignificanceCutoff = 1,

    # Run Options
    LogRunToHistoryFile = "n"
  ))
}

run_satscan <- function(dir, file, satscan_exe) {
  inst_sf <- requireNamespace("sf", quietly = TRUE)

  if (!inst_sf) {
    message("The sf package must be installed to read shapefiles")
  }

  # Run Satscan batch executable
  cmd_output <- system(
    paste(shQuote(satscan_exe), paste0(dir, file, ".prm")),
    intern = TRUE
  )

  # Import Satscan outputs
  ls <- list(
    main = NA, col = NA, rr = NA, gis = NA, llr = NA,
    sci = NA, shapeclust = NA, shapegis = NA, prm = NA,
    cmd_output = cmd_output
  )

  xts <- c(
    ".txt", ".col.dbf", ".rr.dbf", ".gis.dbf",
    ".llr.dbf", ".sci.dbf", ".col.shp", ".gis.shp",
    ".col.prj", ".col.shx", ".gis.prj", ".gis.shx"
  )

  filenames <- sapply(xts, \(x) {
    paste0(dir, file, x)
  })

  if (file.exists(filenames[1])) {
    ls$main <- suppressWarnings(readLines(filenames[1]))
  }

  if (file.exists(filenames[2])) {
    ls$col <- foreign::read.dbf(filenames[2])
  }

  if (file.exists(filenames[3])) {
    ls$rr <- foreign::read.dbf(filenames[3])
  }

  if (file.exists(filenames[4])) {
    ls$gis <- foreign::read.dbf(filenames[4])
  }

  if (file.exists(filenames[5])) {
    ls$llr <- foreign::read.dbf(filenames[5])
  }

  if (file.exists(filenames[6])) {
    ls$sci <- foreign::read.dbf(filenames[6])
  }

  if (file.exists(filenames[7]) & inst_sf) {
    ls$shapeclust <- sf::st_read(dsn = dir, layer = paste0(file, ".col"))
  }

  if (file.exists(filenames[8]) & inst_sf) {
    ls$shapegis <- sf::st_read(dsn = dir, layer = paste0(file, ".gis"))
  }

  ls$prm <- readLines(paste0(dir, file, ".prm"))

  # Delete imported files
  suppressWarnings(file.remove(filenames))

  structure(ls, class = "satscan")
}

# Configure Satscan output with cluster locations
config_ss_locations <- function(df, geo) {
  df |>
    dplyr::left_join(geo, by = "loc_id") |>
    dplyr::relocate(kc, .after = loc_id)
}

# Assign recurrence interval level to clusters (see Levin-Rector 2004)
assign_ri_level <- function(x) {
  cut(
    x,
    breaks = c(0, 100, 365, 5 * 365, 100 * 365, Inf),
    labels = c("very weak", "weak", "moderate", "strong", "very strong"),
    right = FALSE
  )
}

# Configure spatial data in Satscan results
config_ss_spatial <- function(df) {
  df |>
    dplyr::mutate(
      ri_level = assign_ri_level(recurr_int),
      .after = recurr_int
    )
}

# SHINY SETUP -------------------------------------------------------------

# Create a list of choices for syndrome select input
# df = syndrome table
# ls = satscan results
# colors = RI background colors
get_syn_choices <- function(df, ls, colors) {
  max_ri <- get_max_ri_level(ls)

  if (nrow(df) != length(max_ri)) {
    stop("The number of syndromes in `df` and `ls` must be equal")
  }

  df$html <- mapply(max_ri, df$name1, FUN = \(x, y) {
    add_ri_icon(lvl = x, text = y, colors = colors)
  })

  # Divide syndromes by category
  cats <- unique(df$category)

  ls2 <- lapply(cats, \(x) {
    df2 <- df |>
      dplyr::filter(category == x)

    ls <- as.list(df2$abbr)

    names(ls) <- df2$html

    ls
  })

  names(ls2) <- cats

  ls2
}

# Create a list with the max RI level of any cluster for each syndrome
# ls = satscan results
get_max_ri_level <- function(ls) {
  # Get `shapeclust` dataframe for each syndrome
  ss <- lapply(ls, \(src) {
    lapply(src, \(syn) {
      if (is.data.frame(syn$shapeclust) && nrow(syn$shapeclust) > 0) {
        sf::st_drop_geometry(syn$shapeclust)
      } else {
        NA
      }
    })
  })

  # Within each syndrome, combine the dataframes from each source
  ss <- purrr::map2(ss$patient, ss$hospital, rbind)

  # Get the max RI level for each syndrome
  lapply(ss, \(df) {
    if (is.data.frame(df)) {
      row <- suppressWarnings(
        which(df$recurr_int == max(df$recurr_int, na.rm = TRUE))[1]
      )

      as.character(df[row, "ri_level"])
    } else {
      NA
    }
  })
}

# Wrap syndrome name in div with icon representing RI level
add_ri_icon <- function(lvl, text, colors) {
  syn_html <- paste(
    "<div class='syn-icon' style='flex-shrink:0; background: %s;",
    "width: 12px; height: 12px; border: 1px solid %s; border-radius: 50%%;",
    "margin: 0 5px;'></div><div class='syn-text'>%s</div>"
  )

  border <- "#8D959E"

  if (is.na(lvl)) {
    sprintf(syn_html, "white", border, text)
  } else if (lvl == "very weak") {
    sprintf(syn_html, colors[1], border, text)
  } else if (lvl == "weak") {
    sprintf(syn_html, colors[2], border, text)
  } else if (lvl == "moderate") {
    sprintf(syn_html, colors[3], border, text)
  } else if (lvl == "strong") {
    sprintf(syn_html, colors[4], border, text)
  } else if (lvl == "very strong") {
    sprintf(syn_html, colors[5], border, text)
  }
}

# CLUSTERS ----------------------------------------------------------------

# Summarize syndrome clusters by RI level
summarize_clusters <- function(ls, syndromes) {
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
  ls$patient |>
    dplyr::left_join(
      ls$hospital,
      by = "abbr",
      suffix = c("_pat", "_hosp")
    ) |>
    dplyr::left_join(
      data.frame(
        category = syndromes$category,
        syndrome = syndromes$name1,
        abbr = syndromes$abbr
      ),
      by = "abbr"
    ) |>
    dplyr::select(category, syndrome, dplyr::everything(), -abbr)
}

# Filter clusters by data source and pivot longer
config_clusters <- function(df, data_source = c("hospital", "patient")) {
  # Keep/remove variables with "_hosp" or "_pat" suffix
  if (data_source == "hospital") {
    sfx_kp <- "_hosp"; sfx_rm <- "_pat"
  } else if (data_source == "patient") {
    sfx_kp <- "_pat"; sfx_rm <- "_hosp"
  }

  # Remove variables related to the non-pertinent data source
  df <- df |>
    dplyr::select(-dplyr::ends_with(sfx_rm))

  # Find and remove syndromes with no clusters
  syn0 <- df |>
    dplyr::mutate(total = rowSums(
      dplyr::pick(dplyr::ends_with(sfx_kp)),
      na.rm = TRUE
    )) |>
    tidyr::pivot_wider(
      id_cols = syndrome,
      names_from = date,
      names_prefix = "date_",
      values_from = total,
      values_fill = 0
    ) |>
    dplyr::mutate(total = rowSums(
      dplyr::pick(dplyr::starts_with("date_")),
      na.rm = TRUE
    )) |>
    dplyr::filter(total == 0) |>
    dplyr::pull(syndrome)

  df <- df |>
    dplyr::filter(!syndrome %in% syn0)

  # Pivot longer and remove suffix from RI levels
  df |>
    tidyr::pivot_longer(
      cols = dplyr::ends_with(sfx_kp),
      names_to = "ri_level",
      values_to = "n"
    ) |>
    dplyr::mutate(ri_level = sub(sfx_kp, "", ri_level))
}

# Create cluster timeline
cluster_timeline <- function(df, colors) {
  lvl <- c("very_weak", "weak", "moderate", "strong", "very_strong")

  df |>
    dplyr::mutate(ri_level = factor(ri_level, levels = rev(lvl))) |>
    ggplot2::ggplot(ggplot2::aes(
      x = date,
      y = n,
      fill = ri_level
    )) +
    ggplot2::geom_area() +
    ggplot2::facet_wrap(
      syndrome ~ .,
      ncol = 1,
      scales = "free_y",
      drop = FALSE,
      axes = "all_x"
    ) +
    ggplot2::scale_y_continuous(breaks = integer_scale()) +
    ggplot2::scale_fill_manual(
      labels = function(x) stringr::str_to_sentence(gsub("_", " ", x)),
      values = rev(colors)
    ) +
    ggplot2::theme_bw(base_size = 20) +
    ggplot2::labs(
      x = stringr::str_to_sentence,
      y = stringr::str_to_sentence,
      fill = "RI level"
    )
}

# Return function that converts axis labels to integers
integer_scale <- function(n) {
  function(x) {
    breaks <- floor(pretty(x))

    names(breaks) <- attr(breaks, "labels")

    breaks
  }
}

