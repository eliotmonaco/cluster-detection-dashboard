# Get Essence data via API

# Build URLs and pull data ------------------------------------------------

t0 <- Sys.time()

# Import syndromes table
syn <- readxl::read_excel("data/prep/syndromes.xlsx")

# Start date = 1 year and 1 day before current date
start_date <- get_start_date(end_date)

# Syndrome API strings
syn_api <- apply(syn[, c("api1", "api2")], 1, \(x) {
  paste(x, collapse = "&")
})

syn_api <- as.list(syn_api)

names(syn_api) <- syn$abbr

# Add a syndrome name variable that capitalizes first letter
syn <- syn |>
  dplyr::mutate(
    name1 = paste0(
      toupper(substr(name, 1, 1)),
      substr(name, 2, nchar(name))
    ),
    .before = name
  ) |>
  dplyr::rename(name2 = name)

# Data details: Pulled by both patient location and hospital location. Patient
# locations are ZCTAs intersecting with Cass, Clay, Jackson, or Platte County.
# Hospitals are any hospital within those counties.

# Data details fields
flds <- c(
  "Date", "Time", "Age", "AgeGroup", "Sex", "DateOfBirth",
  "ZipCode", "Patient_City", "Patient_State", "Patient_Country", "Travel",
  "HospitalName", "HospitalState", "VisitNumber", "Patient_ID", "HasBeenE"
)

# Build URLs
url_dd <- list(
  patient = build_ess_url(
    syndrome = syn_api,
    start = start_date,
    end = end_date,
    data_source = "patient",
    output = "dd",
    dd_fields = flds,
    zipcodes = geodata$zctas$GEOID20
  ),
  hospital = build_ess_url(
    syndrome = syn_api,
    start = start_date,
    end = end_date,
    data_source = "hospital",
    output = "dd",
    dd_fields = flds
  )
)

# Time series: Pulled by both patient location and hospital location. Patient
# locations are ZCTAs intersecting with Kansas City that have at least 10% of
# their area within the city. Hospitals are any hospital within Cass, Clay,
# Jackson, or Platte County.

# Time series date range
ts_range <- seq.Date(start_date, end_date, "day")

# Build URLs
url_ts <- list(
  patient = build_ess_url(
    syndrome = syn_api,
    start = start_date,
    end = end_date,
    data_source = "patient",
    output = "ts",
    zipcodes = kcData::geoid$zcta2020
  ),
  hospital = build_ess_url(
    syndrome = syn_api,
    start = start_date,
    end = end_date,
    data_source = "hospital",
    output = "ts"
  )
)

# Get data
t1 <- Sys.time()

ddraw <- lapply(url_dd, \(x) {
  lapply(x, \(y) {
    tryCatch(
      capture_message(get_ess_dd(y)),
      error = function (e) e
    )
  })
})

tsraw <- lapply(url_ts, \(x) {
  lapply(x, \(y) {
    tryCatch(
      capture_message(get_ess_ts(y)),
      error = function (e) e
    )
  })
})

t2 <- Sys.time()

names(ddraw$patient) <- syn$abbr
names(ddraw$hospital) <- syn$abbr
names(tsraw$patient) <- syn$abbr
names(tsraw$hospital) <- syn$abbr

# Essence log entry -------------------------------------------------------

# Pull message text
msgdd <- lapply(ddraw, \(ls1) {
  sapply(ls1, \(ls2) {
    sub("\\n$", "", cli::ansi_strip(ls2$message))
  })
})

msgts <- lapply(tsraw, \(ls1) {
  sapply(ls1, \(ls2) {
    sub("\\n$", "", cli::ansi_strip(ls2$message))
  })
})

df <- data.frame(
  SYNDROME_QUERY = syn$name1,
  DD_BY_PATIENT = msgdd$patient,
  DD_BY_HOSPITAL = msgdd$hospital,
  TS_BY_PATIENT = msgts$patient,
  TS_BY_HOSPITAL = msgts$hospital
)

# Make a table easy to read in a text file
df <- setmeup::readable_table(df, 30)

tf <- tempfile(fileext = ".txt")

write.table(
  df,
  file = tf,
  sep = "\t",
  quote = FALSE,
  row.names = FALSE
)

log <- readLines(tf)

dur <- t2 - t1

log <- c(
  paste0(Sys.Date(), "\n"),
  "---------- ESSENCE DATA DOWNLOAD ----------\n",
  paste("Started at", format(t0, "%I:%M %p")),
  paste(
    "Download time:",
    setmeup::round_ties_away(as.numeric(dur), 2),
    units(dur), "\n"
  ),
  log,
  ""
)

writeLines(log, paste0(dir_data, "log.txt"))

# Configure data ----------------------------------------------------------

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
    expr = {
      ls <- deduplicate_dd(df, geo_var = "zip_code")
      ls$data <- config_dd(ls$data, ansi_codes = ansi)
      ls
    },
    error = function(e) e
  )
})

dd$hospital <- lapply(dd$hospital, \(df) {
  tryCatch(
    expr = {
      ls <- deduplicate_dd(df, geo_var = "hospital_name")
      ls$data <- config_dd(ls$data, ansi_codes = ansi)
      ls
    },
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

# Combine raw data lists
ess_raw <- list(
  data_details = ddraw,
  time_series = tsraw
)

# Save --------------------------------------------------------------------

saveRDS(ess_raw, paste0(dir_data, "essence_raw.rds"))

