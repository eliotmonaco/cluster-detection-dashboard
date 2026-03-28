library(testthat)

test_that("get_start_date()", {
  source("R/fn.R", local = TRUE)
  x <- as.Date("2026-03-26")
  exp <- x - lubridate::years(1) - lubridate::days(1)
  act <- get_start_date(x)
  expect_equal(act, exp)
})

test_that("config_dd()", {
  source("R/fn.R", local = TRUE)
  ansi <- readRDS("data/ansi_state_codes.rds")
  testdata <- readRDS(test_path("fixtures/test_data.rds"))
  exp <- testdata$config_dd_output
  dd <- lapply(testdata$data_details_raw, \(ls1) {
    lapply(ls1, \(ls2) ls2$data)
  })
  act <- list()
  act$patient <- lapply(dd$patient, \(df) {
    config_dd(df, geo_var = "zip_code")
  })
  act$hospital <- lapply(dd$hospital, \(df) {
    config_dd(df, geo_var = "hospital_name")
  })
  expect_equal(act, exp)
})

test_that("config_ts()", {
  source("R/fn.R", local = TRUE)
  testdata <- readRDS(test_path("fixtures/test_data.rds"))
  exp <- testdata$config_ts_output
  ts <- lapply(testdata$time_series_raw, \(ls1) {
    lapply(ls1, \(ls2) ls2$data)
  })
  act <- lapply(ts, \(ls) {
    lapply(ls, config_ts)
  })
  expect_equal(act, exp)
})

test_that("get_centroids()", {
  source("R/fn.R", local = TRUE)
  testdata <- readRDS(test_path("fixtures/test_data.rds"))
  exp <- testdata$get_centroids_output
  act <- get_centroids(testdata$get_centroids_input, "NAME")
  expect_equal(act, exp)
})

test_that("config_casefile()", {
  source("R/fn.R", local = TRUE)
  testdata <- readRDS(test_path("fixtures/test_data.rds"))
  exp <- testdata$config_casefile_output
  act <- list()
  act$patient <- lapply(testdata$data_details$patient, \(df) {
    config_casefile(df, var = "zip_code")
  })
  act$hospital <- lapply(testdata$data_details$hospital, \(df) {
    config_casefile(df, var = "hospital_name_geo")
  })
  expect_equal(act, exp)
})

test_that("get_db_data() full", {
  source("R/fn.R", local = TRUE)
  testdata <- readRDS(test_path("fixtures/test_data.rds"))
  dbdata <- list(an20260325 = NA)
  dbdata$an20260326 <- testdata
  exp <-dbdata$an20260326
  x <- names(exp)
  names(exp) <- replace(
    names(exp),
    list = c(
      which(x == "syndromes"), which(x == "date_range"),
      which(x == "time_series"), which(x == "data_details"),
      which(x == "data_details_error"), which(x == "satscan_results")
    ),
    values = c("syn", "daterng", "ts", "dd", "dderr", "ss")
  )
  act <- get_db_data(dbdata, date = "2026-03-26")
  expect_equal(act, exp)
})

test_that("get_db_data() single element", {
  source("R/fn.R", local = TRUE)
  testdata <- readRDS(test_path("fixtures/test_data.rds"))
  dbdata <- list(an20260325 = NA)
  dbdata$an20260326 <- testdata
  exp <-dbdata$an20260326$syndromes
  act <- get_db_data(dbdata, date = "2026-03-26", name = "syn")
  expect_equal(act, exp)
})

test_that("syn_select_list()", {
  source("R/fn.R", local = TRUE)
  testdata <- readRDS(test_path("fixtures/test_data.rds"))
  exp <- testdata$syn_select_list_output
  act <- syn_select_list(testdata$syndromes)
  expect_equal(act, exp)
})

test_that("daterange_select_list()", {
  source("R/fn.R", local = TRUE)
  testdata <- readRDS(test_path("fixtures/test_data.rds"))
  exp <- testdata$daterange_select_list_output
  act <- daterange_select_list(testdata$date_range)
  expect_equal(act, exp)
})

test_that("custom_legend_row()", {
  source("R/fn.R", local = TRUE)
  testdata <- readRDS(test_path("fixtures/test_data.rds"))
  exp <- testdata$custom_legend_row_output
  act <- lapply(testdata$graphical_parameters, \(ls) {
    lapply(ls, custom_legend_row)
  })
  expect_equal(act, exp)
})

test_that("custom_legend_combine()", {
  source("R/fn.R", local = TRUE)
  testdata <- readRDS(test_path("fixtures/test_data.rds"))
  exp <- testdata$custom_legend_combine_output
  legend_rows <- lapply(testdata$graphical_parameters, \(ls) {
    lapply(ls, custom_legend_row)
  })
  act <- lapply(legend_rows, custom_legend_combine)
  expect_equal(act, exp)
})

