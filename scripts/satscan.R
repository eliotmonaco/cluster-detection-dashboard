# Run Satscan analyses

t0 <- Sys.time()

dir_in <- paste0(dir_data, "satscan-input/")
dir_out <- paste0(dir_data, "satscan-output/")

dir.create(dir_in)
dir.create(dir_out)

# Satscan analysis --------------------------------------------------------

# Case file: <location ID> <# cases> <date/time>
purrr::imap(dd$patient, \(df, i) {
  tryCatch(
    expr = {
      df <- config_casefile(df, var = "zip_code")
      rsatscan::write.cas(df, dir_in, paste0(i, "-patient"))
    },
    error = function(e) e
  )
})

purrr::imap(dd$hospital, \(df, i) {
  tryCatch(
    expr = {
      df <- config_casefile(df, var = "hospital_name_geo")
      rsatscan::write.cas(df, dir_in, paste0(i, "-hospital"))
    },
    error = function(e) e
  )
})

# Coordinates file: <location ID> <latitude> <longitude>
geo_file_pat <- geo$zcta_pts |>
  sf::st_drop_geometry() |>
  dplyr::select(zcta, lat, long)

geo_file_hosp <- geo$hosp |>
  sf::st_drop_geometry() |>
  dplyr::select(hospital_name_geo, lat, long)

rsatscan::write.geo(geo_file_pat, dir_in, "zctas")
rsatscan::write.geo(geo_file_hosp, dir_in, "hospitals")

# Parameter file
purrr::imap(dd, \(ls, i) {
  purrr::imap(ls, \(df, j) {
    # Set Satscan options to defaults
    invisible(rsatscan::ss.options(reset = TRUE))

    if (is.null(df)) {
      return(invisible(NULL))
    }

    # Select relevant coordinates file
    if (i == "patient") {
      cfnm <- paste0(dir_in, "zctas.geo")
    } else if (i == "hospital") {
      cfnm <- paste0(dir_in, "hospitals.geo")
    }

    # Replace dates with NA if there were no visits in the period
    if (nrow(df) == 0) {
      mindate <- NA; maxdate <- NA
    } else {
      mindate <- format(min(df$date), "%Y/%m/%d")
      maxdate <- format(max(df$date), "%Y/%m/%d")
    }

    nm <- paste0(j, "-", i)

    # Configure Satscan options
    set_ss_opts(
      casefile = paste0(dir_in, nm, ".cas"),
      coordfile = cfnm,
      start = mindate,
      end = maxdate
    )

    write.ss.prm(dir_out, nm)
  })
})

# Run Satscan
ssresults_raw <- purrr::imap(dd, \(ls, i) {
  purrr::imap(ls, \(x, j) {
    nm <- paste0(j, "-", i)

    if (file.exists(paste0(dir_out, nm, ".prm"))) {
      run_satscan(
        dir = dir_out,
        file = nm,
        satscan_exe = "C:/Program Files/SaTScan/SaTScanBatch64"
      )
    } else {
      NULL
    }
  })
})

t1 <- Sys.time()

# Create log entry --------------------------------------------------------

# Import log
log <- readLines(paste0(dir_data, "log.txt"))

dur <- t1 - t0

# Find warnings or error messages in `ssresults_raw$cmd_output`
msg <- purrr::imap(unlist(ssresults_raw, recursive = FALSE), \(ls, i) {
  if (any(grepl("^Warning|^Error", ls$cmd_output))) {
    m <- c(
      paste("-", i),
      paste("   ", gsub("\n", "\n    ", stringr::str_wrap(ls$cmd_output, 80)))
    )

    m[!grepl("^\\s*$", m)]
  }
})

if (length(purrr::compact(msg)) == 0) {
  logmsg <- "CMD warning/error output: None"
} else {
  logmsg <- c("CMD warning/error output:\n", unlist(msg))
}

log <- c(
  log,
  "---------- SATSCAN ANALYSIS ----------\n",
  paste("Started at", format(t0, "%I:%M %p")),
  paste(
    "Computation time:",
    setmeup::round_ties_away(as.numeric(dur), 2),
    units(dur), "\n"
  ),
  logmsg
)

# Configure ---------------------------------------------------------------

# Var names to lowercase
ssresults <- lapply(ssresults_raw, \(ls) {
  lapply(ls, \(ls2) {
    lapply(ls2, \(x) {
      if (is.data.frame(x)) {
        colnames(x) <- tolower(colnames(x))
      }

      x
    })
  })
})

# Join `kc` variable that indicates if a geography is in Kansas City
ssresults$patient <- lapply(ssresults$patient, \(ls) {
  purrr::imap(ls, \(x, i) {
    if (is.data.frame(x) && grepl("gis", i)) {
      geo <- geo$zctas |>
        sf::st_drop_geometry() |>
        dplyr::select(loc_id = GEOID20, kc)

      x <- config_ss_locations(x, geo = geo) # join in/out of KC var
    }

    if (is.data.frame(x) && grepl("gis|clust", i)) {
      x <- config_ss_spatial(x) # assign RI level
    }

    x
  })
})

ssresults$hospital <- lapply(ssresults$hospital, \(ls) {
  purrr::imap(ls, \(x, i) {
    if (is.data.frame(x) && grepl("gis", i)) {
      geo <- geo$hosp |>
        sf::st_drop_geometry() |>
        dplyr::select(loc_id = hospital_name_geo, kc)

      x <- config_ss_locations(x, geo = geo) # join in/out of KC var
    }

    if (is.data.frame(x) && grepl("gis|clust", i)) {
      x <- config_ss_spatial(x) # assign RI level
    }

    x
  })
})

# Save --------------------------------------------------------------------

procdata <- list(
  syndromes = syn,
  date_range = ts_range,
  time_series = ts,
  data_details = dd,
  data_details_error = dderror,
  satscan_results = ssresults
)

writeLines(log, paste0(dir_data, "log.txt"))
saveRDS(procdata, paste0(dir_data, "processed_data.rds"))

