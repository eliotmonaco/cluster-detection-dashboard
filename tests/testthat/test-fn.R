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

