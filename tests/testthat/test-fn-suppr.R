library(testthat)

test_that("basic suppression", {
  source("R/fn-suppr.R", local = TRUE)

  df <- data.frame(x = c(0, 1, 1, 2, 2, 3, 4, 5))

  act <- suppress_df(df, var = "x", n = 3)

  exp <- data.frame(x = c(0, rep("*", 4), 3, 4, 5))

  expect_equal(act, exp)
})

test_that("secondary suppression", {
  source("R/fn-suppr.R", local = TRUE)

  df <- data.frame(x = c(0, 1, 2, 2, 3, 3, 4, 5))

  act <- suppress_df(df, var = "x", n = 2, sec = TRUE)

  exp <- data.frame(x = c(0, "*", 2, "*", 3, 3, 4, 5))

  expect_setequal(act$x, exp$x)

  expect_setequal(act$x[3:4], c(2, "*"))

  expect_identical(act$x[2], "*")
})

test_that("`var_suppr` != `var`", {
  source("R/fn-suppr.R", local = TRUE)

  df <- data.frame(
    x = c(0, 1, 1, 2, 2, 3, 4, 5),
    y = c("a", "b", "b", "c", "c", "d", "e", "f")
  )

  act <- suppress_df(df, var = "x", n = 3, var_suppr = "y")

  exp <- data.frame(
    x = c(0, 1, 1, 2, 2, 3, 4, 5),
    y = c("a", rep("*", 4), "d", "e", "f")
  )

  expect_equal(act, exp)
})

test_that("`var_suppr` != `var` + secondary suppression", {
  source("R/fn-suppr.R", local = TRUE)

  df <- data.frame(
    x = c(0, 1, 2, 2, 3, 3, 4, 5),
    y = c("a", "b", "c", "c", "d", "d", "e", "f")
  )

  act <- suppress_df(df, var = "x", n = 2, var_suppr = "y", sec = TRUE)

  exp <- data.frame(
    x = c(0, 1, 2, 2, 3, 3, 4, 5),
    y = c("a", "*", "c", "*", "d", "d", "e", "f")
  )

  expect_identical(act$x, exp$x)

  expect_setequal(act$y, exp$y)

  expect_setequal(act$y[3:4], c("c", "*"))

  expect_identical(act$y[2], "*")
})

