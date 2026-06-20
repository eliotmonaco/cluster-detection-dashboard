# Functions for data suppression

# Suppress counts
# x = the value to evaluate for suppression
# n = the suppression level (counts below this number will be suppressed)
# value = the value to suppress
# sep = logical; whether or not to use comma separators (converts `value` to
# character)
suppress_count <- function(x, n, value = x, sep = FALSE) {
  if (sep) {
    value <- prettyNum(value, big.mark = ",")
  }

  ifelse(x < n & x > 0, "*", value)
}

# Suppress counts in a dataframe variable
# var = the variable to evaluate for suppression
# n = the suppression level (counts below this number will be suppressed)
# var_suppr = the variable to suppress
# sep = logical; whether or not to use comma separators (converts `value` to
# character)
# sec = logical; whether or not to apply secondary suppression
suppress_df <- function(
  df, var, n, var_suppr = var, sep = FALSE, sec = FALSE
) {
  # Get number of values to suppress
  n_suppr <- sum(df[[var]] > 0 & df[[var]] < n)

  # Suppress counts
  df$rowid <- 1:nrow(df)

  df[[var_suppr]] <- suppress_count(
    x = df[[var]],
    n = n,
    value = df[[var_suppr]],
    sep = FALSE
  )

  if (sec & n_suppr == 1) {
    # Secondary suppression
    df2 <- df |>
      dplyr::mutate({{ var }} := suppressWarnings(as.numeric(.data[[var]]))) |>
      dplyr::filter_out(.data[[var]] < n) |>
      dplyr::slice_min(order_by = .data[[var]], na_rm = TRUE) |>
      dplyr::slice_sample(n = 1) |>
      dplyr::mutate({{ var_suppr }} := "*") |>
      dplyr::select(dplyr::all_of(c("rowid", var_suppr)))

    df <- df |>
      dplyr::mutate({{ var_suppr }} := as.character(.data[[var_suppr]])) |>
      dplyr::rows_update(df2, by = "rowid")
  }

  df |>
    dplyr::select(-rowid)
}

