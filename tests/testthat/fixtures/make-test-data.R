# Create mock data for dashboard functions

library(tidyverse)
library(rsatscan)
library(sf)

source("R/fn.R")

geo <- readRDS("data/geographic_data.rds")
ansi <- readRDS("data/ansi_state_codes.rds")
hosp <- readRDS("data/hospital_locations.rds")

syn <- list(
  syn1 = list(
    name = "syndrome 1",
    queryname = "Syndrome 1 query",
    kr = "https://",
    apistring = "???"
  ),
  syn2 = list(
    name = "syndrome 2",
    queryname = "Syndrome 2 query",
    kr = "https://",
    apistring = "???"
  ),
  syn3 = list(
    name = "syndrome 3",
    queryname = "Syndrome 3 query",
    kr = "https://",
    apistring = "???"
  )
)

end_date <- as.Date("3001-01-02")

start_date <- get_start_date(end_date)

date_range <- seq.Date(start_date, end_date, "day")

# Essence time series (raw) -----------------------------------------------

# Variables: `date`, `count`, `color_id`

tsraw <- lapply(list(1, 2), \(x) {
  params <- list(
    dates = list(date_range),
    lambda = list(1:10, 5:20, 50:100),
    seed = list(x)
  )

  ls <- pmap(params, \(dates, lambda, seed) {
    # Counts
    set.seed(seed)

    ct <- rpois(length(dates), lambda)

    # Color IDs
    id <- cut(
      ct,
      breaks = c(0, mean(ct) / 2, mean(ct), mean(ct) * 1.5, Inf),
      labels = c(0, 1, 2, 3),
      right = FALSE
    )

    # Data
    df <- data.frame(
      date = dates,
      count = ct,
      color_id = id
    )

    # API message
    msg <- "Success"

    list(data = df, message = msg)
  })

  names(ls) <- names(syn)

  ls
})

names(tsraw) <- c("patient", "hospital")

# Essence data details (raw) ----------------------------------------------

# Variables: `date`, `time`, `age`, `age_group`, `sex`, `date_of_birth`,
#   `zip_code`, `patient_state`, `patient_country`, `hospital_name`,
#   `hospital_state`, `visit_number`, `has_been_e`
ddraw <- lapply(list(1, 2), \(x) {
  params <- list(
    dates = list(date_range),
    end = end_date,
    lambda = list(1:10, 5:20, 50:100),
    seed = list(x),
    zctas = list(geo$zctas$GEOID20),
    hospitals = list(hosp$hospital_name_essence)
  )

  ls <- pmap(params, \(dates, end, lambda, seed, zctas, hospitals) {
    # Counts
    set.seed(seed)

    ct <- rpois(length(dates), lambda)

    # Create visit dates
    dates <- map2(dates, ct, \(x, y) {
      rep(x, y)
    })

    dates <- as.Date(unlist(dates))

    # Time
    tm <- seq.POSIXt(
      as.POSIXct("2023-01-01 00:00:00"),
      as.POSIXct("2023-01-01 23:00:00"),
      "hour"
    )

    set.seed(seed)

    tm <- sample(tm, length(dates), replace = TRUE)

    tm <- format(tm, "%H:%m %p")

    # Age
    set.seed(seed)

    age <- floor(runif(length(dates), 0, 100))

    # Age group
    agegp <- cut(
      age,
      breaks = c(0, 5, 18, 45, 65, Inf),
      labels = c("00-04", "05-17", "18-44", "45-64", "65-1000"),
      right = FALSE
    )

    # Sex
    set.seed(seed)

    sex <- sample(c("F", "M"), length(dates), replace = TRUE)

    # DOB
    dob <- end - age

    # ZIP codes
    set.seed(seed)

    zip <- sample(zctas, length(dates), replace = TRUE)

    # Hospital name
    set.seed(seed)

    hosp <- sample(hospitals, length(dates), replace = TRUE)

    # Data
    df <- data.frame(
      date = dates,
      time = tm,
      age = age,
      age_group = agegp,
      sex = sex,
      date_of_birth = dob,
      zip_code = zip,
      patient_state = "MO",
      patient_country = "USA",
      hospital_name = hosp,
      hospital_state = "MO",
      visit_number = 1:length(dates),
      has_been_e = 1
    )

    # API message
    msg <- "Success"

    list(data = df, message = msg)
  })

  names(ls) <- names(syn)

  ls
})

names(ddraw) <- c("patient", "hospital")

# Create duplicates for `ddraw$patient$syn1$data`
df <- ddraw$patient$syn1$data

df <- rbind(
  df,
  df[1,][c(1, 1, 1),],
  mutate(df[1,], date = date + 1),
  mutate(df[1,], zip_code = 64108)
)

rownames(df) <- NULL

tail(df, 5)

ddraw$patient$syn1$data <- df

# Create duplicates for `ddraw$hospital$syn2$data`
df <- ddraw$hospital$syn2$data

df <- rbind(
  df,
  df[1,][c(1, 1, 1),],
  mutate(df[1,], date = date + 1),
  mutate(df[1,], hospital_name = "Research_Medical_Center")
)

rownames(df) <- NULL

tail(df, 5)

ddraw$hospital$syn2$data <- df

# Configured Essence data -------------------------------------------------

# Separate data from API messages
dd <- lapply(ddraw, \(ls1) {
  lapply(ls1, \(ls2) {
    ls2$data
  })
})

ts <- lapply(tsraw, \(ls1) {
  lapply(ls1, \(ls2) {
    ls2$data
  })
})

# Configure
dd$patient <- lapply(dd$patient, \(df) {
  tryCatch(
    config_dd(df, geo_var = "zip_code"),
    error = function(e) e
  )
})

dd$hospital <- lapply(dd$hospital, \(df) {
  tryCatch(
    config_dd(df, geo_var = "hospital_name"),
    error = function(e) e
  )
})

ts <- lapply(ts, \(ls) {
  lapply(ls, \(df) {
    tryCatch(
      config_ts(df),
      error = function(e) e
    )
  })
})

config_dd_output <- dd

config_ts_output <- ts

# Separate data from error tables in data details
dderror <- lapply(dd, \(ls1) {
  lapply(ls1, \(ls2) {
    ls2$error_rate
  })
})

dd <- lapply(dd, \(ls1) {
  lapply(ls1, \(ls2) {
    ls2$data
  })
})

# Satscan output ----------------------------------------------------------

dir_data <- "tests/testthat/fixtures/test-data/"

dir_in <- paste0(dir_data, "satscan-input/")
dir_out <- paste0(dir_data, "satscan-output/")

dir.create(dir_data)
dir.create(dir_in)
dir.create(dir_out)

# Case file: <location ID> <# cases> <date/time>
imap(dd$patient, \(df, i) {
  tryCatch(
    expr = {
      df <- config_casefile(df, var = "zip_code")
      write.cas(df, dir_in, paste0(i, "-patient"))
    },
    error = function(e) e
  )
})

imap(dd$hospital, \(df, i) {
  tryCatch(
    expr = {
      df <- config_casefile(df, var = "hospital_name_geo")
      write.cas(df, dir_in, paste0(i, "-hospital"))
    },
    error = function(e) e
  )
})

# Coordinates file: <location ID> <latitude> <longitude>
geo_file_pat <- geo$zcta_pts |>
  st_drop_geometry() |>
  select(zcta, lat, long)

geo_file_hosp <- geo$hosp |>
  st_drop_geometry() |>
  select(hospital_name_geo, lat, long)

write.geo(geo_file_pat, dir_in, "zctas")
write.geo(geo_file_hosp, dir_in, "hospitals")

# Parameter file
imap(dd, \(ls, i) {
  imap(ls, \(df, j) {
    # Set Satscan options to defaults
    invisible(ss.options(reset = TRUE, version = "10.3"))

    if (is.null(df)) {
      return(invisible(NULL))
    }

    if (i == "patient") {
      cfnm <- paste0(dir_in, "zctas.geo")
    } else if (i == "hospital") {
      cfnm <- paste0(dir_in, "hospitals.geo")
    }

    nm <- paste0(j, "-", i)

    # Configure Satscan options
    set_ss_opts(
      casefile = paste0(dir_in, nm, ".cas"),
      coordfile = cfnm,
      start = format(min(df$date), "%Y/%m/%d"),
      end = format(max(df$date), "%Y/%m/%d")
    )

    write.ss.prm(dir_out, nm)
  })
})

# Run Satscan
ssresults <- imap(dd, \(ls, i) {
  imap(ls, \(x, j) {
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

# Var names to lowercase
ssresults <- lapply(ssresults, \(ls) {
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
  imap(ls, \(x, i) {
    if (is.data.frame(x) && grepl("gis", i)) {
      x <- x |>
        left_join(
          geo$zctas |>
            st_drop_geometry() |>
            select(loc_id = GEOID20, kc),
          by = "loc_id"
        ) |>
        relocate(kc, .after = loc_id)
    }

    x
  })
})

ssresults$hospital <- lapply(ssresults$hospital, \(ls) {
  imap(ls, \(x, i) {
    if (is.data.frame(x) && grepl("gis", i)) {
      x <- x |>
        left_join(
          geo$hosp |>
            st_drop_geometry() |>
            select(loc_id = hospital_name_geo, kc),
          by = "loc_id"
        ) |>
        relocate(kc, .after = loc_id)
    }

    x
  })
})

# Clean up
unlink(dir_data, recursive = TRUE, force = TRUE)

# Save --------------------------------------------------------------------

testdata <- list(
  time_series_raw = tsraw,
  data_details_raw = ddraw,
  config_ts_output = config_ts_output,
  config_dd_output = config_dd_output,
  syndromes = syn,
  date_range = date_range,
  time_series = ts,
  data_details = dd,
  data_details_error = dderror,
  satscan_results = ssresults
)

saveRDS(testdata, "tests/testthat/fixtures/test_data.rds")

