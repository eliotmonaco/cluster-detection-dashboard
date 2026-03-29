library(testthat)

test_that("get_db_data() full", {
  source("R/app-fns.R", local = TRUE)
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
  source("R/app-fns.R", local = TRUE)
  testdata <- readRDS(test_path("fixtures/test_data.rds"))
  dbdata <- list(an20260325 = NA)
  dbdata$an20260326 <- testdata
  exp <-dbdata$an20260326$syndromes
  act <- get_db_data(dbdata, date = "2026-03-26", name = "syn")
  expect_equal(act, exp)
})

test_that("syn_select_list()", {
  source("R/app-fns.R", local = TRUE)
  testdata <- readRDS(test_path("fixtures/test_data.rds"))
  exp <- testdata$syn_select_list_output
  act <- syn_select_list(testdata$syndromes)
  expect_equal(act, exp)
})

test_that("daterange_select_list()", {
  source("R/app-fns.R", local = TRUE)
  testdata <- readRDS(test_path("fixtures/test_data.rds"))
  exp <- testdata$daterange_select_list_output
  act <- daterange_select_list(testdata$date_range)
  expect_equal(act, exp)
})

test_that("custom_legend_row()", {
  source("R/app-fns.R", local = TRUE)
  testdata <- readRDS(test_path("fixtures/test_data.rds"))
  exp <- testdata$custom_legend_row_output
  act <- lapply(testdata$graphical_parameters, \(ls) {
    lapply(ls, custom_legend_row)
  })
  expect_equal(act, exp)
})

test_that("custom_legend_combine()", {
  source("R/app-fns.R", local = TRUE)
  testdata <- readRDS(test_path("fixtures/test_data.rds"))
  exp <- testdata$custom_legend_combine_output
  legend_rows <- lapply(testdata$graphical_parameters, \(ls) {
    lapply(ls, custom_legend_row)
  })
  act <- lapply(legend_rows, custom_legend_combine)
  expect_equal(act, exp)
})

