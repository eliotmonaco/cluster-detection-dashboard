library(testthat)

test_that("get_start_date()", {
  source("R/analysis-fns.R", local = TRUE)
  x <- as.Date("2026-03-26")
  act <- get_start_date(x)
  exp <- x - lubridate::years(1) - lubridate::days(1)
  expect_equal(act, exp)
})

test_that("deduplicate_dd()", {
  source("R/analysis-fns.R", local = TRUE)
  ansi <- readRDS("data/ansi_state_codes.rds")
  testdata <- readRDS(test_path("fixtures/test_analysis.rds"))
  dd <- lapply(testdata$data_details_raw, \(ls1) {
    lapply(ls1, \(ls2) ls2$data)
  })
  act <- list()
  act$patient <- lapply(dd$patient, \(df) {
    deduplicate_dd(df, geo_var = "zip_code")
  })
  act$hospital <- lapply(dd$hospital, \(df) {
    deduplicate_dd(df, geo_var = "hospital_name")
  })
  exp <- testdata$deduplicate_dd_output
  expect_equal(act, exp)
})

test_that("config_dd()", {
  source("R/analysis-fns.R", local = TRUE)
  ansi <- readRDS("data/ansi_state_codes.rds")
  testdata <- readRDS(test_path("fixtures/test_analysis.rds"))
  dd <- testdata$deduplicate_dd_output
  act <- list()
  act$patient <- lapply(dd$patient, \(ls) {
    ls$data <- config_dd(ls$data)
    ls
  })
  act$hospital <- lapply(dd$hospital, \(ls) {
    ls$data <- config_dd(ls$data)
    ls
  })
  exp <- testdata$config_dd_output
  expect_equal(act, exp)
})

test_that("config_ts()", {
  source("R/analysis-fns.R", local = TRUE)
  testdata <- readRDS(test_path("fixtures/test_analysis.rds"))
  ts <- lapply(testdata$time_series_raw, \(ls1) {
    lapply(ls1, \(ls2) ls2$data)
  })
  act <- lapply(ts, \(ls) {
    lapply(ls, config_ts)
  })
  exp <- testdata$config_ts_output
  expect_equal(act, exp)
})

test_that("get_centroids()", {
  source("R/analysis-fns.R", local = TRUE)
  testdata <- readRDS(test_path("fixtures/test_analysis.rds"))
  act <- get_centroids(testdata$get_centroids_input, "NAME")
  exp <- testdata$get_centroids_output
  expect_equal(act, exp)
})

test_that("config_casefile()", {
  source("R/analysis-fns.R", local = TRUE)
  testdata <- readRDS(test_path("fixtures/test_analysis.rds"))
  act <- list()
  act$patient <- lapply(testdata$data_details$patient, \(df) {
    config_casefile(df, var = "zip_code")
  })
  act$hospital <- lapply(testdata$data_details$hospital, \(df) {
    config_casefile(df, var = "hospital_name_geo")
  })
  exp <- testdata$config_casefile_output
  expect_equal(act, exp)
})

